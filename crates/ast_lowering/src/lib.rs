//! AST lowering.

pub mod analysis;
mod lowering;

pub use lowering::lower_ast;
