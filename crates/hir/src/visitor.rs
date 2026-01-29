use crate::{self as hir, EntityList};

pub trait Visitor {
    type Error;

    fn visit_definitions(
        &mut self,
        ctx: &hir::Context,
    ) -> Result<(), Self::Error> {
        visit_definitions(self, ctx)
    }

    fn visit_definition(
        &mut self,
        idx: hir::DefIdx,
        def: &hir::Definition,
        ctx: &hir::Context,
    ) -> Result<(), Self::Error> {
        visit_definition(self, idx, def, ctx)
    }

    fn visit_expression(
        &mut self,
        idx: hir::ExprIdx,
        ctx: &hir::Context,
    ) -> Result<(), Self::Error> {
        visit_expression(self, idx, ctx)
    }

    fn visit_operation(
        &mut self,
        idx: hir::ExprIdx,
        op: &hir::Operation,
        args: EntityList<hir::ExprIdx>,
        ctx: &hir::Context,
    ) -> Result<(), Self::Error> {
        visit_operation(self, idx, op, args, ctx)
    }

    fn visit_if(
        &mut self,
        idx: hir::ExprIdx,
        expr: &hir::If,
        ctx: &hir::Context,
    ) -> Result<(), Self::Error> {
        visit_if(self, idx, expr, ctx)
    }

    fn visit_let(
        &mut self,
        idx: hir::ExprIdx,
        expr: &hir::Let,
        ctx: &hir::Context,
    ) -> Result<(), Self::Error> {
        visit_let(self, idx, expr, ctx)
    }

    fn visit_while(
        &mut self,
        idx: hir::ExprIdx,
        expr: &hir::While,
        ctx: &hir::Context,
    ) -> Result<(), Self::Error> {
        visit_while(self, idx, expr, ctx)
    }
}

pub fn visit_definitions<V: Visitor + ?Sized>(
    v: &mut V,
    ctx: &hir::Context,
) -> Result<(), V::Error> {
    for (idx, def) in &ctx.defs {
        v.visit_definition(idx, def, ctx)?;
    }

    Ok(())
}

pub fn visit_definition<V: Visitor + ?Sized>(
    v: &mut V,
    _: hir::DefIdx,
    def: &hir::Definition,
    ctx: &hir::Context,
) -> Result<(), V::Error> {
    v.visit_expression(def.body, ctx)
}

pub fn visit_expression<V: Visitor + ?Sized>(
    v: &mut V,
    idx: hir::ExprIdx,
    ctx: &hir::Context,
) -> Result<(), V::Error> {
    match &ctx[idx].kind {
        hir::ExprKind::Num(_) => Ok(()),
        hir::ExprKind::Const(_) => Ok(()),
        hir::ExprKind::Var(..) => Ok(()),
        hir::ExprKind::Op(op, args) => v.visit_operation(idx, op, *args, ctx),
        hir::ExprKind::If(kind) => v.visit_if(idx, kind, ctx),
        hir::ExprKind::Let(kind) => v.visit_let(idx, kind, ctx),
        hir::ExprKind::While(kind) => v.visit_while(idx, kind, ctx),
    }
}

pub fn visit_operation<V: Visitor + ?Sized>(
    v: &mut V,
    _: hir::ExprIdx,
    _: &hir::Operation,
    args: EntityList<hir::ExprIdx>,
    ctx: &hir::Context,
) -> Result<(), V::Error> {
    for &arg in &ctx[args] {
        v.visit_expression(arg, ctx)?;
    }

    Ok(())
}

pub fn visit_if<V: Visitor + ?Sized>(
    v: &mut V,
    _: hir::ExprIdx,
    expr: &hir::If,
    ctx: &hir::Context,
) -> Result<(), V::Error> {
    v.visit_expression(expr.cond, ctx)?;
    v.visit_expression(expr.if_true, ctx)?;
    v.visit_expression(expr.if_false, ctx)
}

pub fn visit_let<V: Visitor + ?Sized>(
    v: &mut V,
    _: hir::ExprIdx,
    expr: &hir::Let,
    ctx: &hir::Context,
) -> Result<(), V::Error> {
    for &write in &ctx[expr.writes] {
        v.visit_expression(ctx[write].val, ctx)?;
    }

    v.visit_expression(expr.body, ctx)
}

pub fn visit_while<V: Visitor + ?Sized>(
    v: &mut V,
    _: hir::ExprIdx,
    expr: &hir::While,
    ctx: &hir::Context,
) -> Result<(), V::Error> {
    for &write in &ctx[expr.inits] {
        v.visit_expression(ctx[write].val, ctx)?;
    }

    for &write in &ctx[expr.updates] {
        v.visit_expression(ctx[write].val, ctx)?;
    }

    v.visit_expression(expr.cond, ctx)?;
    v.visit_expression(expr.body, ctx)
}
