mod builder;
mod compile;
mod components;
mod libm;
mod program;
mod stdlib;

use builder::IrBuilder;
use components::{ComponentManager, Ids};

pub use compile::compile_hir;
pub use program::Program;
pub use stdlib::{Import, ImportPaths, ImportSet};
