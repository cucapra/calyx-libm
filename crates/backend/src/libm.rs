use std::collections::HashMap;

use malachite::num::basic::traits::Zero;

use calyx_libm_hir::{self as hir, Metadata, Pool, Visitor};
use calyx_libm_hir_passes::analysis::RangeAnalysis;
use calyx_libm_utils::interface::{Config, RangeAnalysis as AnalysisMode};
use calyx_libm_utils::{Diagnostic, Format, Reporter};

use crate::ComponentManager;
use crate::ops::{self, OpBuilder, Operator, Prototype};

pub fn compile_math_library(
    hir: &hir::Context,
    cfg: &Config,
    reporter: &mut Reporter,
    cm: &mut ComponentManager,
) -> Option<HashMap<hir::ExprIdx, Prototype>> {
    let ranges = match cfg.range_analysis {
        AnalysisMode::Interval => Some(RangeAnalysis::new(hir, cfg, reporter)?),
        AnalysisMode::None => None,
    };

    let mut builder = Builder {
        hir,
        ranges: ranges.as_ref(),
        format: &cfg.format,
        reporter,
        cm,
        prototypes: HashMap::new(),
    };

    builder.visit_definitions(hir).ok()?;

    Some(builder.prototypes)
}

#[derive(Debug)]
pub struct LibraryError;

struct Builder<'a, 'src> {
    hir: &'a hir::Context,
    ranges: Option<&'a RangeAnalysis>,
    format: &'a Format,

    reporter: &'a mut Reporter<'src>,
    cm: &'a mut ComponentManager,

    prototypes: HashMap<hir::ExprIdx, Prototype>,
}

impl Visitor for Builder<'_, '_> {
    type Error = LibraryError;

    fn visit_operation(
        &mut self,
        idx: hir::ExprIdx,
        op: &hir::Operation,
        args: hir::EntityList<hir::ExprIdx>,
        ctx: &hir::Context,
    ) -> Result<(), LibraryError> {
        if let hir::OpKind::Sollya(sollya) = op.kind {
            let op = Operator {
                idx: sollya,
                span: op.span,
                ctx,
            };

            let prototype =
                self.build(&op, &ctx[idx], args).map_err(|err| {
                    self.reporter.emit(&err);

                    LibraryError
                })?;

            self.prototypes.insert(idx, prototype);
        }

        hir::visitor::visit_operation(self, idx, op, args, ctx)
    }
}

impl<'a> Builder<'a, '_> {
    fn build(
        &mut self,
        op: &Operator,
        expr: &hir::Expression,
        args: hir::EntityList<hir::ExprIdx>,
    ) -> Result<Prototype, Diagnostic> {
        let strategy = expr
            .props(self.hir)
            .find_map(|prop| match prop {
                hir::Property::Impl(strategy) => Some(strategy),
                _ => None,
            })
            .ok_or_else(|| {
                Diagnostic::error()
                    .with_message(format!(
                        "no implementation specified for operator `{}`",
                        op.pretty(),
                    ))
                    .try_with_primary(op.span, "no implementation specified")
                    .with_note("help: add a `:calyx-impl` annotation")
            })?;

        match strategy {
            hir::Strategy::Iterative => {
                let builder = ops::Iterative {
                    format: self.format,
                };

                builder.build(op, self.cm)
            }
            &hir::Strategy::Lut { size } => {
                let domain = self.choose_domain(op, expr, args)?;

                let builder = ops::Lut {
                    format: self.format,
                    left: domain.left,
                    right: domain.right,
                    size,
                };

                builder.build(op, self.cm)
            }
            &hir::Strategy::Poly { degree, error } => {
                static ZERO: hir::Rational = hir::Rational::ZERO;

                let domain = self.choose_domain(op, expr, args)?;
                let error =
                    error.map(|error| &self.hir[error].value).unwrap_or(&ZERO);

                let builder = ops::Poly {
                    format: self.format,
                    left: domain.left,
                    right: domain.right,
                    degree,
                    error,
                };

                builder.build(op, self.cm)
            }
        }
    }

    fn choose_domain(
        &self,
        op: &Operator,
        expr: &hir::Expression,
        args: hir::EntityList<hir::ExprIdx>,
    ) -> Result<DomainHint<'a>, Diagnostic> {
        let hint = expr.props(self.hir).find_map(|prop| match prop {
            hir::Property::Domain(domain) => Some(domain),
            _ => None,
        });

        let (left, right) = hint
            .map(|domain| {
                (&self.hir[domain.left].value, &self.hir[domain.right].value)
            })
            .or_else(|| {
                self.ranges.map(|ranges| {
                    let [left, right] =
                        &ranges[args.first(self.hir.pool()).unwrap()];

                    (left, right)
                })
            })
            .ok_or_else(|| {
                Diagnostic::error()
                    .with_message(format!(
                        "operator `{}` has unknown domain",
                        op.pretty(),
                    ))
                    .try_with_primary(op.span, "unknown domain")
                    .with_note("help: add a `:calyx-domain` annotation or enable range analysis")
            })?;

        Ok(DomainHint { left, right })
    }
}

struct DomainHint<'a> {
    left: &'a hir::Rational,
    right: &'a hir::Rational,
}
