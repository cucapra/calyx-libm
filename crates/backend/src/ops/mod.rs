mod iterative;
mod operator;
mod table;

pub use iterative::Iterative;
pub use table::{Lut, Poly};

pub use operator::{Component, OpBuilder, Operator, Primitive, Prototype};
