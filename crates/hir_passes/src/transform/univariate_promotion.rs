use calyx_libm_hir::{self as hir, Pool};

use crate::{Pass, PassContext, PassError, Visitor, visitor};

pub struct UnivariatePromotion;

impl Pass for UnivariatePromotion {
    fn run(ctx: &mut PassContext) -> Result<(), PassError> {
        Self.visit_definitions(ctx.hir)
    }
}

impl Visitor for UnivariatePromotion {
    type Error = PassError;

    fn visit_expression(
        &mut self,
        idx: hir::ExprIdx,
        ctx: &mut hir::Context,
    ) -> Result<(), PassError> {
        if let hir::ExprKind::Op(ref op, mut args) = ctx[idx].kind
            && let hir::OpKind::Arith(hir::ArithOp::Pow) = op.kind
        {
            let span = op.span;

            visitor::visit_operation(self, args, ctx)?;

            for i in 0..=1 {
                if let hir::ExprKind::Num(num) = ctx[ctx[args][i]].kind {
                    let var = ctx.ops.intern(hir::SollyaExpr::Variable);
                    let num = ctx.ops.intern(hir::SollyaExpr::Number(num));

                    let op = ctx.ops.intern(if i == 0 {
                        hir::SollyaExpr::Binary(hir::SollyaBinOp::Pow, num, var)
                    } else {
                        hir::SollyaExpr::Binary(hir::SollyaBinOp::Pow, var, num)
                    });

                    args.remove(i, ctx.mut_pool());

                    ctx[idx].kind = hir::ExprKind::Op(
                        hir::Operation {
                            kind: hir::OpKind::Sollya(op),
                            span,
                        },
                        args,
                    );

                    break;
                }
            }

            Ok(())
        } else {
            visitor::visit_expression(self, idx, ctx)
        }
    }
}
