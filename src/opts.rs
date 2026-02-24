use std::path::PathBuf;

use calyx_libm_utils::Format;
use calyx_libm_utils::interface::{Config, RangeAnalysis};

/// FPCore frontend for Calyx.
#[derive(argh::FromArgs)]
pub struct Opts {
    /// input file
    #[argh(positional)]
    pub file: Option<PathBuf>,

    /// add directory to library search path
    #[argh(option, short = 'l')]
    pub lib_path: Vec<PathBuf>,

    /// output file
    #[argh(option, short = 'o')]
    pub output: Option<PathBuf>,

    /// emit absolute import paths
    #[argh(switch)]
    pub absolute: bool,

    /// global numeric format
    #[argh(option, default = "Default::default()")]
    pub format: Format,

    /// range analysis mode
    #[argh(option, default = "Default::default()")]
    pub range_analysis: RangeAnalysis,

    /// enable pass
    #[argh(option, short = 'e')]
    pub enable: Vec<String>,

    /// disable pass
    #[argh(option, short = 'd')]
    pub disable: Vec<String>,
}

impl Opts {
    /// Parses options from `env::args`.
    pub fn parse() -> Opts {
        argh::from_env()
    }

    pub fn config(&self) -> Config<'_> {
        Config {
            format: self.format,
            range_analysis: self.range_analysis,
            enabled: &self.enable,
            disabled: &self.disable,
        }
    }
}
