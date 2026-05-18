use super::arena::PackedOption;
use super::index as idx;

#[derive(Clone, Copy, Debug)]
pub enum Property {
    Pre(idx::ExprIdx),
    Domain(Domain),
    Impl(Strategy),
}

#[derive(Clone, Copy, Debug)]
pub struct Domain {
    pub left: idx::NumIdx,
    pub right: idx::NumIdx,
}

#[derive(Clone, Copy, Debug)]
pub enum Strategy {
    Iterative,
    Lut {
        size: u32,
    },
    Poly {
        degree: u32,
        error: PackedOption<idx::NumIdx>,
    },
}
