//! Semantic analysis.

mod bindings;
mod call_graph;
mod passes;
mod type_check;

pub use bindings::{Binding, NameResolution};
pub use call_graph::CallGraph;
pub use type_check::{Type, TypeCheck};

pub use passes::PassManager;
