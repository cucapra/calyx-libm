use std::collections::HashSet;

use calyx_libm_hir as hir;
use calyx_libm_utils::{Config, Reporter};

use super::transform;

#[derive(Debug)]
pub struct PassError;

#[allow(dead_code)]
pub struct PassContext<'pm, 'src> {
    pub hir: &'pm mut hir::Context,
    pub cfg: &'pm Config<'pm>,
    pub reporter: &'pm mut Reporter<'src>,
}

pub trait Pass {
    const NAME: &'static str;
    const IS_DEFAULT: bool;

    fn run(ctx: &mut PassContext) -> Result<(), PassError>;
}

struct PassInfo {
    run: fn(&mut PassContext) -> Result<(), PassError>,
    name: &'static str,
    is_default: bool,
}

const fn pass_info<P: Pass>() -> PassInfo {
    PassInfo {
        run: P::run,
        name: P::NAME,
        is_default: P::IS_DEFAULT,
    }
}

pub fn run_passes(
    ctx: &mut hir::Context,
    cfg: &Config,
    reporter: &mut Reporter,
) -> Result<(), PassError> {
    let enabled: HashSet<&str> =
        cfg.enabled.iter().map(String::as_str).collect();
    let disabled: HashSet<&str> =
        cfg.disabled.iter().map(String::as_str).collect();

    let mut ctx = PassContext {
        hir: ctx,
        cfg,
        reporter,
    };

    static PASSES: [PassInfo; 2] = [
        pass_info::<transform::ConstantPropagation>(),
        pass_info::<transform::UnivariatePromotion>(),
    ];

    for pass in &PASSES {
        if (pass.is_default || enabled.contains(pass.name))
            && !disabled.contains(pass.name)
        {
            (pass.run)(&mut ctx)?;
        }
    }

    Ok(())
}
