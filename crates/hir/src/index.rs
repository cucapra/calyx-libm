use std::ops::Range;

use super::arena::{self, EntityRef, IterEntityRange, entity_impl};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefIdx(u32);
entity_impl!(DefIdx);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArgIdx(u32);
entity_impl!(ArgIdx);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprIdx(u32);
entity_impl!(ExprIdx);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct NumIdx(u32);
entity_impl!(NumIdx);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarIdx(u32);
entity_impl!(VarIdx);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct WriteIdx(u32);
entity_impl!(WriteIdx);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SollyaIdx(u32);
entity_impl!(SollyaIdx);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeIdx(u32);
entity_impl!(ScopeIdx);

#[derive(Clone, Copy)]
pub struct IndexRange<I>
where
    I: EntityRef,
{
    pub start: I,
    pub end: I,
}

impl<I: EntityRef> IntoIterator for IndexRange<I> {
    type Item = I;
    type IntoIter = IterEntityRange<I>;

    fn into_iter(self) -> Self::IntoIter {
        arena::iter_entity_range(Range::from(self))
    }
}

impl<I: EntityRef> From<IndexRange<I>> for Range<I> {
    fn from(value: IndexRange<I>) -> Self {
        Range {
            start: value.start,
            end: value.end,
        }
    }
}
