//! AST lowering.

pub mod analysis;
mod lower;

pub use lower::lower_ast;
