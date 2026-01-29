use calyx_libm_hir as hir;

use crate::passes::{Pass, PassContext, PassError, Visitor, visitor};

pub struct ConstantPropagation;

impl Pass for ConstantPropagation {
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

                if let hir::ExprKind::Num(first) = ctx[if_true].kind
                    && let hir::ExprKind::Num(second) = ctx[if_false].kind
                    && ctx[first].value == ctx[second].value
                {
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

fn propagate(to: hir::ExprIdx, from: hir::ExprIdx, ctx: &mut hir::Context) {
    let from = &ctx[from];

    if let hir::ExprKind::Num(num) = from.kind {
        ctx[to] = hir::Expression {
            kind: hir::ExprKind::Num(num),
            scope: from.scope,
            span: from.span,
        };
    }
}
