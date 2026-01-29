//! AST traversal.

use super::ast;

pub trait Visitor<'ast> {
    type Error;

    fn visit_definitions<I>(&mut self, defs: I) -> Result<(), Self::Error>
    where
        I: IntoIterator<Item = &'ast ast::FPCore>,
    {
        visit_definitions(self, defs)
    }

    fn visit_definition(
        &mut self,
        def: &'ast ast::FPCore,
    ) -> Result<(), Self::Error> {
        visit_definition(self, def)
    }

    fn visit_binding(
        &mut self,
        binding: &'ast ast::Binding,
    ) -> Result<(), Self::Error> {
        visit_binding(self, binding)
    }

    fn visit_mutable_var(
        &mut self,
        var: &'ast ast::MutableVar,
    ) -> Result<(), Self::Error> {
        visit_mutable_var(self, var)
    }

    fn visit_induction_var(
        &mut self,
        var: &'ast ast::InductionVar,
    ) -> Result<(), Self::Error> {
        visit_induction_var(self, var)
    }

    fn visit_expression(
        &mut self,
        expr: &'ast ast::Expression,
    ) -> Result<(), Self::Error> {
        visit_expression(self, expr)
    }
}

pub fn visit_definitions<'ast, V, I>(v: &mut V, defs: I) -> Result<(), V::Error>
where
    V: Visitor<'ast> + ?Sized,
    I: IntoIterator<Item = &'ast ast::FPCore>,
{
    for def in defs {
        v.visit_definition(def)?;
    }

    Ok(())
}

pub fn visit_definition<'ast, V: Visitor<'ast> + ?Sized>(
    v: &mut V,
    def: &'ast ast::FPCore,
) -> Result<(), V::Error> {
    v.visit_expression(&def.body)
}

pub fn visit_binding<'ast, V: Visitor<'ast> + ?Sized>(
    v: &mut V,
    binding: &'ast ast::Binding,
) -> Result<(), V::Error> {
    v.visit_expression(&binding.expr)
}

pub fn visit_mutable_var<'ast, V: Visitor<'ast> + ?Sized>(
    v: &mut V,
    var: &'ast ast::MutableVar,
) -> Result<(), V::Error> {
    v.visit_expression(&var.init)?;
    v.visit_expression(&var.update)
}

pub fn visit_induction_var<'ast, V: Visitor<'ast> + ?Sized>(
    v: &mut V,
    var: &'ast ast::InductionVar,
) -> Result<(), V::Error> {
    v.visit_expression(&var.size)
}

pub fn visit_expression<'ast, V: Visitor<'ast> + ?Sized>(
    v: &mut V,
    expr: &'ast ast::Expression,
) -> Result<(), V::Error> {
    match &expr.kind {
        ast::ExprKind::Num(_) => Ok(()),
        ast::ExprKind::Const(_) => Ok(()),
        ast::ExprKind::Id(_) => Ok(()),
        ast::ExprKind::Op(_, exprs) | ast::ExprKind::Array(exprs) => {
            for expr in exprs {
                v.visit_expression(expr)?;
            }

            Ok(())
        }
        ast::ExprKind::If {
            cond,
            true_branch,
            false_branch,
        } => {
            v.visit_expression(cond)?;
            v.visit_expression(true_branch)?;
            v.visit_expression(false_branch)
        }
        ast::ExprKind::Let {
            bindings,
            body,
            sequential: _,
        } => {
            for binding in bindings {
                v.visit_binding(binding)?;
            }

            v.visit_expression(body)
        }
        ast::ExprKind::While {
            cond,
            vars,
            body,
            sequential: _,
        } => {
            for var in vars {
                v.visit_mutable_var(var)?;
            }

            v.visit_expression(cond)?;
            v.visit_expression(body)
        }
        ast::ExprKind::For {
            indices,
            vars,
            body,
            sequential: _,
        }
        | ast::ExprKind::TensorStar {
            indices,
            vars,
            body,
        } => {
            for var in indices {
                v.visit_induction_var(var)?;
            }

            for var in vars {
                v.visit_mutable_var(var)?;
            }

            v.visit_expression(body)
        }
        ast::ExprKind::Tensor { indices, body } => {
            for var in indices {
                v.visit_induction_var(var)?;
            }

            v.visit_expression(body)
        }
        ast::ExprKind::Cast(expr) => v.visit_expression(expr),
        ast::ExprKind::Annotation { props: _, body } => {
            v.visit_expression(body)
        }
    }
}
