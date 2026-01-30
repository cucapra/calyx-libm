use calyx_libm_hir as hir;
use calyx_libm_utils::{Config, Reporter};

use super::transform;

#[derive(Debug)]
pub struct PassError;

#[allow(dead_code)]
pub struct PassContext<'pm, 'src> {
    pub hir: &'pm mut hir::Context,
    pub cfg: &'pm Config,
    pub reporter: &'pm mut Reporter<'src>,
}

pub trait Pass {
    fn run(ctx: &mut PassContext) -> Result<(), PassError>;
}

pub fn run_passes(
    ctx: &mut hir::Context,
    cfg: &Config,
    reporter: &mut Reporter,
) -> Result<(), PassError> {
    let mut ctx = PassContext {
        hir: ctx,
        cfg,
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
