use std::fmt;
use std::str::FromStr;

use crate::Format;

pub struct Config {
    pub format: Format,
    pub range_analysis: RangeAnalysis,
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
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

#[derive(Debug)]
pub struct ParseRangeAnalysisError;

impl fmt::Display for ParseRangeAnalysisError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unknown analysis mode")
    }
}
