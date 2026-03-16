use calyx_libm_hir as hir;

use crate::{Pass, PassContext, PassError, Visitor, visitor};

pub struct ConstantPropagation;

impl Pass for ConstantPropagation {
    const NAME: &str = "constant-propagation";
    const IS_DEFAULT: bool = true;

    fn run(ctx: &mut PassContext) -> Result<(), PassError> {
        Self.visit_definitions(ctx.hir)
    }
}

impl Visitor for ConstantPropagation {
    type Error = PassError;

    fn visit_expression(
        &mut self,
        idx: hir::ExprIdx,
        ctx: &mut hir::Context,
    ) -> Result<(), PassError> {
        match ctx[idx].kind {
            hir::ExprKind::Num(_) => Ok(()),
            hir::ExprKind::Const(_) => Ok(()),
            hir::ExprKind::Var(_, hir::VarKind::Let(expr)) => {
                propagate(idx, expr, ctx);

                Ok(())
            }
            hir::ExprKind::Var(..) => Ok(()),
            hir::ExprKind::Op(_, args) => {
                visitor::visit_operation(self, args, ctx)
            }
            hir::ExprKind::If(hir::If {
                cond,
                if_true,
                if_false,
            }) => {
                visitor::visit_if(self, cond, if_true, if_false, ctx)?;

                if let hir::ExprKind::Const(hir::Constant::Bool(cond)) =
                    ctx[cond].kind
                {
                    let branch = if cond { if_true } else { if_false };

                    ctx[idx] = ctx[branch];
                } else if are_equal_constants(if_true, if_false, ctx) {
                    propagate(idx, if_true, ctx);
                }

                Ok(())
            }
            hir::ExprKind::Let(hir::Let { writes, body, .. }) => {
                visitor::visit_let(self, writes, body, ctx)?;

                propagate(idx, body, ctx);

                Ok(())
            }
            hir::ExprKind::While(hir::While {
                cond,
                inits,
                updates,
                body,
                ..
            }) => {
                visitor::visit_while(self, cond, inits, updates, body, ctx)?;

                propagate(idx, body, ctx);

                Ok(())
            }
        }
    }
}

fn are_equal_constants(
    a: hir::ExprIdx,
    b: hir::ExprIdx,
    ctx: &hir::Context,
) -> bool {
    match (ctx[a].kind, ctx[b].kind) {
        (hir::ExprKind::Num(a), hir::ExprKind::Num(b)) => {
            ctx[a].value == ctx[b].value
        }
        (hir::ExprKind::Const(a), hir::ExprKind::Const(b)) => a == b,
        _ => false,
    }
}

fn propagate(dst: hir::ExprIdx, src: hir::ExprIdx, ctx: &mut hir::Context) {
    let src = &ctx[src];

    if let hir::ExprKind::Num(_) | hir::ExprKind::Const(_) = src.kind {
        ctx[dst] = *src;
    }
}
