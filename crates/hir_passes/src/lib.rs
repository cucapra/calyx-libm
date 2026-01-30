//! Optimization passes.

pub mod analysis;
mod manager;
mod transform;
mod visitor;

use manager::{Pass, PassContext, PassError};
use visitor::Visitor;

pub use manager::run_passes;
