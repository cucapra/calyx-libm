use super::operator::OpBuilder;
use calyx_libm_dfg as dfg;
use dfg::*;
use malachite::Rational;

pub struct Lut {
    pub left: Rational,
    pub right: Rational,
    pub size: u32,
}

impl OpBuilder for Lut {
    fn build(&self) -> Dfg {
        Dfg::new()
    }
}
