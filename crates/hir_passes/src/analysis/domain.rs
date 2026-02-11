use std::cmp::{self, Ordering};
use std::collections::HashMap;

use itertools::Itertools;

use calyx_libm_hir as hir;
use calyx_libm_utils::{Diagnostic, Reporter};

#[derive(Debug)]
pub struct UnsupportedPredicateError;

#[derive(Default)]
pub struct Precondition<'ctx> {
    pub domains: HashMap<hir::ArgIdx, Interval<'ctx>>,
}

impl<'ctx> Precondition<'ctx> {
    pub fn new() -> Precondition<'ctx> {
        Default::default()
    }

    pub fn add_constraint(
        &mut self,
        pred: hir::ExprIdx,
        ctx: &'ctx hir::Context,
        reporter: &mut Reporter,
    ) -> Result<(), UnsupportedPredicateError> {
        let pred = &ctx[pred];

        let mut unsupported = || {
            reporter.emit(
                &Diagnostic::error()
                    .with_message("unsupported precondition")
                    .try_with_primary(pred.span, "not allowed here"),
            );

            Err(UnsupportedPredicateError)
        };

        let hir::ExprKind::Op(op, args) = &pred.kind else {
            return unsupported();
        };

        let hir::OpKind::Test(op) = op.kind else {
            return unsupported();
        };

        let args = &ctx[*args];

        match op {
            hir::TestOp::And => {
                for &arg in args {
                    self.add_constraint(arg, ctx, reporter)?;
                }
            }
            hir::TestOp::Or => {
                let mut disjunction = Precondition::new();

                disjunction.add_constraint(args[0], ctx, reporter)?;

                for &arg in &args[1..] {
                    let mut disjunct = Precondition::new();

                    disjunct.add_constraint(arg, ctx, reporter)?;
                    disjunction.union(disjunct);
                }

                self.intersect(disjunction);
            }
            hir::TestOp::Lt
            | hir::TestOp::Gt
            | hir::TestOp::Leq
            | hir::TestOp::Geq => {
                for (a, b) in args.iter().copied().tuple_windows() {
                    let (a, b) =
                        if matches!(op, hir::TestOp::Lt | hir::TestOp::Leq) {
                            (a, b)
                        } else {
                            (b, a)
                        };

                    let (var, domain) = match (&ctx[a].kind, &ctx[b].kind) {
                        (
                            hir::ExprKind::Var(_, var),
                            hir::ExprKind::Num(num),
                        ) => {
                            let domain = Interval::new(
                                Endpoint::Infinite,
                                Endpoint::Finite(&ctx[*num].value),
                            );

                            (*var, domain)
                        }
                        (
                            hir::ExprKind::Num(num),
                            hir::ExprKind::Var(_, var),
                        ) => {
                            let domain = Interval::new(
                                Endpoint::Finite(&ctx[*num].value),
                                Endpoint::Infinite,
                            );

                            (*var, domain)
                        }
                        _ => {
                            return unsupported();
                        }
                    };

                    let hir::VarKind::Arg(arg) = var else {
                        unreachable!("`:pre` only checked at top level");
                    };

                    self.domains
                        .entry(arg)
                        .and_modify(|old| *old = old.intersection(&domain))
                        .or_insert(domain);
                }
            }
            _ => {
                return unsupported();
            }
        }

        Ok(())
    }

    fn union(&mut self, other: Precondition<'ctx>) {
        self.domains = self
            .domains
            .iter()
            .filter_map(|(arg, left)| {
                other
                    .domains
                    .get(arg)
                    .map(|right| (*arg, left.union(right)))
            })
            .collect();
    }

    fn intersect(&mut self, other: Precondition<'ctx>) {
        for (arg, right) in other.domains {
            self.domains
                .entry(arg)
                .and_modify(|left| *left = left.intersection(&right))
                .or_insert(right);
        }
    }
}

pub struct Interval<'a> {
    left: Left<'a>,
    right: Right<'a>,
}

impl<'a> Interval<'a> {
    fn new(left: Endpoint<'a>, right: Endpoint<'a>) -> Interval<'a> {
        Interval {
            left: Left(left),
            right: Right(right),
        }
    }

    pub fn union(&self, other: &Interval<'a>) -> Interval<'a> {
        Interval {
            left: cmp::min(self.left, other.left),
            right: cmp::max(self.right, other.right),
        }
    }

    pub fn intersection(&self, other: &Interval<'a>) -> Interval<'a> {
        Interval {
            left: cmp::max(self.left, other.left),
            right: cmp::min(self.right, other.right),
        }
    }

    pub fn bounds(&self) -> Option<(&'a hir::Rational, &'a hir::Rational)> {
        match (self.left.0, self.right.0) {
            (Endpoint::Finite(left), Endpoint::Finite(right)) => {
                Some((left, right))
            }
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Endpoint<'a> {
    Infinite,
    Finite(&'a hir::Rational),
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Left<'a>(Endpoint<'a>);

impl PartialOrd for Left<'_> {
    fn partial_cmp(&self, other: &Left) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Left<'_> {
    fn cmp(&self, other: &Left) -> Ordering {
        match (self.0, other.0) {
            (Endpoint::Infinite, Endpoint::Infinite) => Ordering::Equal,
            (Endpoint::Infinite, Endpoint::Finite(_)) => Ordering::Less,
            (Endpoint::Finite(_), Endpoint::Infinite) => Ordering::Greater,
            (Endpoint::Finite(a), Endpoint::Finite(b)) => a.cmp(b),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Right<'a>(Endpoint<'a>);

impl PartialOrd for Right<'_> {
    fn partial_cmp(&self, other: &Right) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Right<'_> {
    fn cmp(&self, other: &Right) -> Ordering {
        match (self.0, other.0) {
            (Endpoint::Infinite, Endpoint::Infinite) => Ordering::Equal,
            (Endpoint::Infinite, Endpoint::Finite(_)) => Ordering::Greater,
            (Endpoint::Finite(_), Endpoint::Infinite) => Ordering::Less,
            (Endpoint::Finite(a), Endpoint::Finite(b)) => a.cmp(b),
        }
    }
}
