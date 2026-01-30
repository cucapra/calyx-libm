mod builder;
mod compile;
mod components;
mod libm;
mod stdlib;

use builder::IrBuilder;
use components::ComponentManager;
use stdlib::Import;

pub use compile::{Program, compile_hir};
pub use stdlib::{ImportPaths, build_library};
