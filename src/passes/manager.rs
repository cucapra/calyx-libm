use calyx_libm_hir as hir;
use calyx_libm_utils::Reporter;

use super::transform;
use crate::opts::Opts;

#[derive(Debug)]
pub struct PassError;

#[allow(dead_code)]
pub struct PassContext<'pm, 'src> {
    pub hir: &'pm mut hir::Context,
    pub opts: &'pm Opts,
    pub reporter: &'pm mut Reporter<'src>,
}

pub trait Pass {
    fn run(ctx: &mut PassContext) -> Result<(), PassError>;
}

pub fn run_passes(
    ctx: &mut hir::Context,
    opts: &Opts,
    reporter: &mut Reporter,
) -> Result<(), PassError> {
    let mut ctx = PassContext {
        hir: ctx,
        opts,
        reporter,
    };

    let passes = [
        transform::ConstantPropagation::run,
        transform::UnivariatePromotion::run,
    ];

    for pass in passes {
        pass(&mut ctx)?;
    }

    Ok(())
}
