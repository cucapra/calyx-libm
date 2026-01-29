use std::fmt;
use std::path::PathBuf;
use std::str::FromStr;

use calyx_libm_utils::Format;

#[derive(Clone, Copy, Default)]
pub enum RangeAnalysis {
    #[default]
    None,
    Interval,
}

impl FromStr for RangeAnalysis {
    type Err = ParseRangeAnalysisError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "none" => Ok(RangeAnalysis::None),
            "interval" => Ok(RangeAnalysis::Interval),
            _ => Err(ParseRangeAnalysisError),
        }
    }
}

pub struct ParseRangeAnalysisError;

impl fmt::Display for ParseRangeAnalysisError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unknown analysis mode")
    }
}

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
}

impl Opts {
    /// Parses options from `env::args`.
    pub fn parse() -> Opts {
        argh::from_env()
    }
}
