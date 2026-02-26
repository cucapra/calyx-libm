use calyx_libm_hir::{self as hir, EntityList, Pool};

use crate::{Pass, PassContext, PassError};

pub struct OperatorCoalescing;

impl Pass for OperatorCoalescing {
    const NAME: &str = "operator-coalescing";
    const IS_DEFAULT: bool = false;

    fn run(ctx: &mut PassContext) -> Result<(), PassError> {
        let mut rewriter = Rewriter { ctx: ctx.hir };

        rewriter.rewrite_definitions();

        Ok(())
    }
}

struct Rewriter<'a> {
    ctx: &'a mut hir::Context,
}

impl Rewriter<'_> {
    fn rewrite_definitions(&mut self) {
        for def in self.ctx.defs.keys() {
            self.rewrite_expression(self.ctx[def].body);
        }
    }

    fn rewrite_expression(&mut self, idx: hir::ExprIdx) {
        let op = self.coalesce_expression(idx);

        self.replace_expression(idx, &op);
    }

    fn replace_expression(&mut self, idx: hir::ExprIdx, op: &Compound) {
        if op.trivial || op.sollya.function(self.ctx).is_some() {
            return;
        }

        let arg = match op.free {
            Free::Variable(free) => {
                let arg = hir::Expression {
                    kind: hir::ExprKind::Var(free, self.ctx[free]),
                    scope: Default::default(),
                    span: hir::Span::NONE,
                };

                self.ctx.exprs.push(arg)
            }
            Free::Anonymous(free) => free,
            _ => {
                return;
            }
        };

        self.ctx[idx].kind = hir::ExprKind::Op(
            hir::Operation {
                kind: hir::OpKind::Sollya(op.sollya),
                span: self.ctx[idx].span,
            },
            EntityList::from_slice(&[arg], self.ctx.mut_pool()),
        );
    }

    fn coalesce_expression(&mut self, idx: hir::ExprIdx) -> Compound {
        let expr = &self.ctx[idx];

        match expr.kind {
            hir::ExprKind::Num(num) => {
                let sollya = self.ctx.ops.intern(hir::SollyaExpr::Number(num));

                Compound {
                    sollya,
                    free: Free::Top,
                    trivial: true,
                }
            }
            hir::ExprKind::Const(_) => self.anonymous(idx),
            hir::ExprKind::Var(var, _) => {
                let sollya = self.ctx.ops.intern(hir::SollyaExpr::Variable);

                Compound {
                    sollya,
                    free: Free::Variable(var),
                    trivial: true,
                }
            }
            hir::ExprKind::Op(
                hir::Operation {
                    kind: hir::OpKind::Arith(op),
                    ..
                },
                args,
            ) => {
                if let hir::ArithOp::Neg = op {
                    let arg = self.coalesce_expression(self.ctx[args][0]);

                    let sollya =
                        self.ctx.ops.intern(hir::SollyaExpr::Neg(arg.sollya));

                    Compound { sollya, ..arg }
                } else if let Ok(op) = hir::SollyaBinOp::try_from(op) {
                    let [lhs, rhs] = self.ctx[args] else {
                        unreachable!()
                    };

                    let lhs_op = self.coalesce_expression(lhs);
                    let rhs_op = self.coalesce_expression(rhs);

                    let free = lhs_op.free.meet(rhs_op.free);

                    if matches!(free, Free::Bottom) {
                        self.replace_expression(lhs, &lhs_op);
                        self.replace_expression(rhs, &rhs_op);

                        self.anonymous(idx)
                    } else {
                        let sollya =
                            self.ctx.ops.intern(hir::SollyaExpr::Binary(
                                op,
                                lhs_op.sollya,
                                rhs_op.sollya,
                            ));

                        let trivial = lhs_op.trivial
                            && rhs_op.trivial
                            && op != hir::SollyaBinOp::Pow;

                        Compound {
                            sollya,
                            free,
                            trivial,
                        }
                    }
                } else if let Ok(op) = hir::SollyaFn::try_from(op) {
                    let arg = self.coalesce_expression(self.ctx[args][0]);

                    let sollya = self
                        .ctx
                        .ops
                        .intern(hir::SollyaExpr::Call(op, arg.sollya));

                    Compound {
                        sollya,
                        free: arg.free,
                        trivial: false,
                    }
                } else {
                    for i in 0..args.len(self.ctx.pool()) {
                        self.rewrite_expression(self.ctx[args][i]);
                    }

                    self.anonymous(idx)
                }
            }
            hir::ExprKind::Op(
                hir::Operation {
                    kind: hir::OpKind::Sollya(op),
                    ..
                },
                args,
            ) => {
                let arg = self.coalesce_expression(self.ctx[args][0]);

                let sollya = op.compose(arg.sollya, self.ctx);

                Compound {
                    sollya,
                    free: arg.free,
                    trivial: false,
                }
            }
            hir::ExprKind::Op(_, args) => {
                for i in 0..args.len(self.ctx.pool()) {
                    self.rewrite_expression(self.ctx[args][i]);
                }

                self.anonymous(idx)
            }
            hir::ExprKind::If(hir::If {
                cond,
                if_true,
                if_false,
            }) => {
                self.rewrite_expression(cond);
                self.rewrite_expression(if_true);
                self.rewrite_expression(if_false);

                self.anonymous(idx)
            }
            hir::ExprKind::Let(hir::Let { writes, body, .. }) => {
                for i in 0..writes.len(self.ctx.pool()) {
                    self.rewrite_expression(self.ctx[self.ctx[writes][i]].val);
                }

                self.rewrite_expression(body);

                self.anonymous(idx)
            }
            hir::ExprKind::While(hir::While {
                cond,
                inits,
                updates,
                body,
                ..
            }) => {
                for i in 0..inits.len(self.ctx.pool()) {
                    self.rewrite_expression(self.ctx[self.ctx[inits][i]].val);
                }

                for i in 0..updates.len(self.ctx.pool()) {
                    self.rewrite_expression(self.ctx[self.ctx[updates][i]].val);
                }

                self.rewrite_expression(cond);
                self.rewrite_expression(body);

                self.anonymous(idx)
            }
        }
    }

    fn anonymous(&mut self, idx: hir::ExprIdx) -> Compound {
        let sollya = self.ctx.ops.intern(hir::SollyaExpr::Variable);

        Compound {
            sollya,
            free: Free::Anonymous(idx),
            trivial: true,
        }
    }
}

struct Compound {
    sollya: hir::SollyaIdx,
    free: Free,
    trivial: bool,
}

#[derive(Clone, Copy)]
enum Free {
    Top,
    Variable(hir::VarIdx),
    Anonymous(hir::ExprIdx),
    Bottom,
}

impl Free {
    fn meet(self, other: Free) -> Free {
        match (self, other) {
            (Free::Top, value) | (value, Free::Top) => value,
            (Free::Variable(a), Free::Variable(b)) if a == b => self,
            (Free::Anonymous(a), Free::Anonymous(b)) if a == b => self,
            _ => Free::Bottom,
        }
    }
}
