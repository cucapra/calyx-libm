use calyx_libm_hir::{self as hir, EntityList, Pool};

pub trait Visitor {
    type Error;

    fn visit_definitions(
        &mut self,
        ctx: &mut hir::Context,
    ) -> Result<(), Self::Error> {
        visit_definitions(self, ctx)
    }

    fn visit_definition(
        &mut self,
        idx: hir::DefIdx,
        ctx: &mut hir::Context,
    ) -> Result<(), Self::Error> {
        visit_definition(self, idx, ctx)
    }

    fn visit_expression(
        &mut self,
        idx: hir::ExprIdx,
        ctx: &mut hir::Context,
    ) -> Result<(), Self::Error> {
        visit_expression(self, idx, ctx)
    }
}

pub fn visit_definitions<V: Visitor + ?Sized>(
    v: &mut V,
    ctx: &mut hir::Context,
) -> Result<(), V::Error> {
    for def in ctx.defs.keys() {
        v.visit_definition(def, ctx)?;
    }

    Ok(())
}

pub fn visit_definition<V: Visitor + ?Sized>(
    v: &mut V,
    idx: hir::DefIdx,
    ctx: &mut hir::Context,
) -> Result<(), V::Error> {
    v.visit_expression(ctx[idx].body, ctx)
}

pub fn visit_expression<V: Visitor + ?Sized>(
    v: &mut V,
    idx: hir::ExprIdx,
    ctx: &mut hir::Context,
) -> Result<(), V::Error> {
    match ctx[idx].kind {
        hir::ExprKind::Num(_) => Ok(()),
        hir::ExprKind::Const(_) => Ok(()),
        hir::ExprKind::Var(..) => Ok(()),
        hir::ExprKind::Op(_, args) => visit_operation(v, args, ctx),
        hir::ExprKind::If(hir::If {
            cond,
            if_true,
            if_false,
        }) => visit_if(v, cond, if_true, if_false, ctx),
        hir::ExprKind::Let(hir::Let { writes, body, .. }) => {
            visit_let(v, writes, body, ctx)
        }
        hir::ExprKind::While(hir::While {
            cond,
            inits,
            updates,
            body,
            ..
        }) => visit_while(v, cond, inits, updates, body, ctx),
    }
}

pub fn visit_operation<V: Visitor + ?Sized>(
    v: &mut V,
    args: EntityList<hir::ExprIdx>,
    ctx: &mut hir::Context,
) -> Result<(), V::Error> {
    for i in 0..args.len(ctx.pool()) {
        v.visit_expression(ctx[args][i], ctx)?;
    }

    Ok(())
}

pub fn visit_if<V: Visitor + ?Sized>(
    v: &mut V,
    cond: hir::ExprIdx,
    if_true: hir::ExprIdx,
    if_false: hir::ExprIdx,
    ctx: &mut hir::Context,
) -> Result<(), V::Error> {
    v.visit_expression(cond, ctx)?;
    v.visit_expression(if_true, ctx)?;
    v.visit_expression(if_false, ctx)
}

pub fn visit_let<V: Visitor + ?Sized>(
    v: &mut V,
    writes: EntityList<hir::WriteIdx>,
    body: hir::ExprIdx,
    ctx: &mut hir::Context,
) -> Result<(), V::Error> {
    for i in 0..writes.len(ctx.pool()) {
        v.visit_expression(ctx[ctx[writes][i]].val, ctx)?;
    }

    v.visit_expression(body, ctx)
}

pub fn visit_while<V: Visitor + ?Sized>(
    v: &mut V,
    cond: hir::ExprIdx,
    inits: EntityList<hir::WriteIdx>,
    updates: EntityList<hir::WriteIdx>,
    body: hir::ExprIdx,
    ctx: &mut hir::Context,
) -> Result<(), V::Error> {
    for i in 0..inits.len(ctx.pool()) {
        v.visit_expression(ctx[ctx[inits][i]].val, ctx)?;
    }

    for i in 0..updates.len(ctx.pool()) {
        v.visit_expression(ctx[ctx[updates][i]].val, ctx)?;
    }

    v.visit_expression(cond, ctx)?;
    v.visit_expression(body, ctx)
}
