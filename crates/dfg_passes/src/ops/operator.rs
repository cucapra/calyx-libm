use calyx_libm_dfg as dfg;
use dfg::*;

pub trait OpBuilder {
    fn build(&self) -> Dfg;
}
