use std::iter::FusedIterator;
use std::ops::{Index, IndexMut};

use super::arena::{EntityList, ListPool, PackedOption, PrimaryMap};
use super::interned::Interned;
use super::{hir, index as idx};

#[derive(Default)]
pub struct Context {
    pub defs: PrimaryMap<idx::DefIdx, hir::Definition>,
    pub args: PrimaryMap<idx::ArgIdx, hir::Argument>,
    pub exprs: PrimaryMap<idx::ExprIdx, hir::Expression>,
    pub numbers: PrimaryMap<idx::NumIdx, hir::Number>,
    pub vars: PrimaryMap<idx::VarIdx, ()>,
    pub writes: PrimaryMap<idx::WriteIdx, hir::Write>,
    pub ops: Interned<idx::SollyaIdx, hir::SollyaExpr>,
    pub scopes: PrimaryMap<idx::ScopeIdx, hir::Scope>,

    pub expr_lists: ListPool<idx::ExprIdx>,
    pub write_lists: ListPool<idx::WriteIdx>,
}

impl Context {
    pub fn new() -> Context {
        Context::default()
    }
}

macro_rules! index_impl {
    ($field:ident, $idx:ty, $out:ty) => {
        impl Index<$idx> for Context {
            type Output = $out;

            #[inline]
            fn index(&self, index: $idx) -> &Self::Output {
                &self.$field[index]
            }
        }

        impl IndexMut<$idx> for Context {
            #[inline]
            fn index_mut(&mut self, index: $idx) -> &mut Self::Output {
                &mut self.$field[index]
            }
        }
    };
}

index_impl!(defs, idx::DefIdx, hir::Definition);
index_impl!(args, idx::ArgIdx, hir::Argument);
index_impl!(exprs, idx::ExprIdx, hir::Expression);
index_impl!(numbers, idx::NumIdx, hir::Number);
index_impl!(writes, idx::WriteIdx, hir::Write);

impl Index<idx::SollyaIdx> for Context {
    type Output = hir::SollyaExpr;

    #[inline]
    fn index(&self, index: idx::SollyaIdx) -> &Self::Output {
        &self.ops[index]
    }
}

impl Index<EntityList<idx::ExprIdx>> for Context {
    type Output = [idx::ExprIdx];

    #[inline]
    fn index(&self, index: EntityList<idx::ExprIdx>) -> &Self::Output {
        index.as_slice(&self.expr_lists)
    }
}

impl Index<EntityList<idx::WriteIdx>> for Context {
    type Output = [idx::WriteIdx];

    #[inline]
    fn index(&self, index: EntityList<idx::WriteIdx>) -> &Self::Output {
        index.as_slice(&self.write_lists)
    }
}

pub trait Pool<P> {
    fn pool(&self) -> &P;

    fn mut_pool(&mut self) -> &mut P;
}

impl Pool<ListPool<idx::ExprIdx>> for Context {
    #[inline]
    fn pool(&self) -> &ListPool<idx::ExprIdx> {
        &self.expr_lists
    }

    #[inline]
    fn mut_pool(&mut self) -> &mut ListPool<idx::ExprIdx> {
        &mut self.expr_lists
    }
}

impl Pool<ListPool<idx::WriteIdx>> for Context {
    #[inline]
    fn pool(&self) -> &ListPool<idx::WriteIdx> {
        &self.write_lists
    }

    #[inline]
    fn mut_pool(&mut self) -> &mut ListPool<idx::WriteIdx> {
        &mut self.write_lists
    }
}

pub trait Metadata {
    fn scope(&self) -> PackedOption<idx::ScopeIdx>;

    fn props<'ctx>(&self, ctx: &'ctx Context) -> Props<'ctx> {
        Props {
            idx: self.scope(),
            ctx,
        }
    }
}

#[derive(Clone)]
pub struct Props<'ctx> {
    idx: PackedOption<idx::ScopeIdx>,
    ctx: &'ctx Context,
}

impl<'ctx> Iterator for Props<'ctx> {
    type Item = &'ctx hir::Property;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(scope) = self.idx.expand() {
            let scope = &self.ctx.scopes[scope];

            self.idx = scope.parent;

            Some(&scope.prop)
        } else {
            None
        }
    }
}

impl FusedIterator for Props<'_> {}

macro_rules! scope_impl {
    ($for:ty) => {
        impl Metadata for $for {
            #[inline]
            fn scope(&self) -> PackedOption<idx::ScopeIdx> {
                self.scope
            }
        }
    };
}

scope_impl!(hir::Definition);
scope_impl!(hir::Argument);
scope_impl!(hir::Expression);
