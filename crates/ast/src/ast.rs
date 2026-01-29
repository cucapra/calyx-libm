//! Abstract syntax for FPCore.

use std::ops::Range;
use std::sync::atomic::{AtomicU32, Ordering};

pub use calyx_utils::Id;

pub use super::constants::{MathConst, MathOp, TensorOp, TestOp};
pub use super::literals::Rational;
pub use super::metadata::PropKind;

#[derive(Debug)]
pub struct FPCore {
    /// Operator name.
    pub name: Option<Symbol>,
    /// List of free variables.
    pub args: Vec<Argument>,
    /// List of metadata properties.
    pub props: Vec<Property>,
    /// Operator body.
    pub body: Expression,
}

#[derive(Debug)]
pub enum Dimension {
    /// Variable to bind to dimension size.
    Id(Symbol),
    /// Fixed dimension size.
    Num(Number),
}

/// A formal parameter.
#[derive(Debug)]
pub struct Argument {
    pub var: Symbol,
    pub props: Vec<Property>,
    pub dims: Vec<Dimension>,
    pub uid: NodeId,
}

#[derive(Debug)]
pub struct Binding {
    pub var: Symbol,
    pub expr: Expression,
    pub uid: NodeId,
}

#[derive(Debug)]
pub struct MutableVar {
    /// Variable name.
    pub var: Symbol,
    /// Initial value.
    pub init: Expression,
    /// Expression producing an updated value.
    pub update: Expression,
    /// Node identifier.
    pub uid: NodeId,
}

#[derive(Debug)]
pub struct InductionVar {
    /// Variable name.
    pub var: Symbol,
    /// Number of iterations.
    pub size: Expression,
    /// Node identifier.
    pub uid: NodeId,
}

#[derive(Debug)]
pub enum ExprKind {
    Num(Number),
    Const(Constant),
    Id(Symbol),
    Op(Operation, Vec<Expression>),
    If {
        cond: Box<Expression>,
        true_branch: Box<Expression>,
        false_branch: Box<Expression>,
    },
    Let {
        bindings: Vec<Binding>,
        body: Box<Expression>,
        sequential: bool,
    },
    While {
        cond: Box<Expression>,
        vars: Vec<MutableVar>,
        body: Box<Expression>,
        sequential: bool,
    },
    For {
        indices: Vec<InductionVar>,
        vars: Vec<MutableVar>,
        body: Box<Expression>,
        sequential: bool,
    },
    Tensor {
        indices: Vec<InductionVar>,
        body: Box<Expression>,
    },
    TensorStar {
        indices: Vec<InductionVar>,
        vars: Vec<MutableVar>,
        body: Box<Expression>,
    },
    Cast(Box<Expression>),
    Array(Vec<Expression>),
    Annotation {
        props: Vec<Property>,
        body: Box<Expression>,
    },
}

#[derive(Debug)]
pub struct Expression {
    pub kind: ExprKind,
    pub uid: NodeId,
    pub span: Span,
}

/// A numeric literal.
#[derive(Clone, Debug)]
pub struct Number {
    pub value: Rational,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub id: Id,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub enum OpKind {
    Math(MathOp),
    Test(TestOp),
    Tensor(TensorOp),
    FPCore(Id),
}

#[derive(Clone, Debug)]
pub struct Operation {
    pub kind: OpKind,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub enum Constant {
    Math(MathConst),
    Bool(bool),
}

#[derive(Debug)]
pub struct Property {
    pub kind: PropKind,
    pub span: Span,
}

/// Uniquely identifies an AST node.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(u32);

impl NodeId {
    /// Creates a new identifier that is distinct from all previously created
    /// identifiers.
    #[allow(clippy::new_without_default)]
    pub fn new() -> NodeId {
        static NEXT: AtomicU32 = AtomicU32::new(0);

        NodeId(NEXT.fetch_add(1, Ordering::Relaxed))
    }
}

/// A region of source code.
#[derive(Clone, Copy, Debug)]
pub struct Span(usize, usize);

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span(start, end)
    }
}

impl From<Span> for Range<usize> {
    fn from(value: Span) -> Self {
        Range {
            start: value.0,
            end: value.1,
        }
    }
}
