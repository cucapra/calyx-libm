use std::collections::HashSet;

use calyx_libm_hir as hir;
use calyx_libm_utils::{Config, Diagnostic, Reporter};

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

static PASSES: [PassInfo; 3] = [
    pass_info::<transform::ConstantPropagation>(),
    pass_info::<transform::UnivariatePromotion>(),
    pass_info::<transform::OperatorCoalescing>(),
];

pub fn run_passes(
    ctx: &mut hir::Context,
    cfg: &Config,
    reporter: &mut Reporter,
) -> Result<(), PassError> {
    let known: HashSet<&str> = PASSES.iter().map(|pass| pass.name).collect();

    for pass in cfg.enabled.iter().chain(cfg.disabled) {
        if !known.contains(pass.as_str()) {
            reporter.emit(
                &Diagnostic::error()
                    .with_message(format!("unrecognized pass `{pass}`")),
            );

            return Err(PassError);
        }
    }

    let enabled: HashSet<&str> =
        cfg.enabled.iter().map(String::as_str).collect();
    let disabled: HashSet<&str> =
        cfg.disabled.iter().map(String::as_str).collect();

    let mut ctx = PassContext {
        hir: ctx,
        cfg,
        reporter,
    };

    for pass in &PASSES {
        if (pass.is_default || enabled.contains(pass.name))
            && !disabled.contains(pass.name)
        {
            (pass.run)(&mut ctx)?;
        }
    }

    Ok(())
}
