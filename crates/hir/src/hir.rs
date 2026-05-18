use std::ops::Range;

use calyx_libm_ast as ast;

use super::arena::{EntityList, PackedOption};
use super::index as idx;

pub use ast::{Constant, Id, MathConst, Rational, Symbol, TestOp};

pub use super::metadata::*;
pub use super::sollya::*;

#[derive(Clone, Copy, Debug)]
pub struct Definition {
    pub name: Option<Symbol>,
    pub args: idx::IndexRange<idx::ArgIdx>,
    pub scope: PackedOption<idx::ScopeIdx>,
    pub body: idx::ExprIdx,
}

#[derive(Clone, Copy, Debug)]
pub struct Argument {
    pub name: Symbol,
    pub var: idx::VarIdx,
    pub scope: PackedOption<idx::ScopeIdx>,
}

#[derive(Clone, Copy, Debug)]
pub enum ExprKind {
    Num(idx::NumIdx),
    Const(Constant),
    Var(idx::VarIdx),
    Op(Operation, EntityList<idx::ExprIdx>),
    If(If),
    Let(Let),
    While(While),
}

#[derive(Clone, Copy, Debug)]
pub struct Expression {
    pub kind: ExprKind,
    pub scope: PackedOption<idx::ScopeIdx>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Number {
    pub value: Rational,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub enum VarKind {
    Arg(idx::ArgIdx),
    Let(idx::ExprIdx),
    Mut,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Pow,
    Abs,
    Max,
    Min,
}

impl TryFrom<ast::MathOp> for ArithOp {
    type Error = ();

    fn try_from(value: ast::MathOp) -> Result<Self, Self::Error> {
        match value {
            ast::MathOp::Add => Ok(ArithOp::Add),
            ast::MathOp::Sub => Ok(ArithOp::Sub),
            ast::MathOp::Mul => Ok(ArithOp::Mul),
            ast::MathOp::Div => Ok(ArithOp::Div),
            ast::MathOp::Neg => Ok(ArithOp::Neg),
            ast::MathOp::Pow => Ok(ArithOp::Pow),
            ast::MathOp::FAbs => Ok(ArithOp::Abs),
            ast::MathOp::FMax => Ok(ArithOp::Max),
            ast::MathOp::FMin => Ok(ArithOp::Min),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum OpKind {
    Arith(ArithOp),
    Test(TestOp),
    Sollya(idx::SollyaIdx),
    Def(idx::DefIdx),
}

#[derive(Clone, Copy, Debug)]
pub struct Operation {
    pub kind: OpKind,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub struct If {
    pub cond: idx::ExprIdx,
    pub if_true: idx::ExprIdx,
    pub if_false: idx::ExprIdx,
}

#[derive(Clone, Copy, Debug)]
pub struct Let {
    pub writes: EntityList<idx::WriteIdx>,
    pub body: idx::ExprIdx,
    pub sequential: bool,
}

#[derive(Clone, Copy, Debug)]
pub struct While {
    pub cond: idx::ExprIdx,
    pub inits: EntityList<idx::WriteIdx>,
    pub updates: EntityList<idx::WriteIdx>,
    pub body: idx::ExprIdx,
    pub sequential: bool,
}

#[derive(Clone, Copy, Debug)]
pub struct Write {
    pub var: idx::VarIdx,
    pub val: idx::ExprIdx,
}

#[derive(Clone, Copy, Debug)]
pub struct Scope {
    pub prop: Property,
    pub parent: PackedOption<idx::ScopeIdx>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span(usize, usize);

impl Span {
    pub const NONE: Span = Span(usize::MAX, usize::MAX);

    #[inline]
    pub fn new(start: usize, end: usize) -> Span {
        Span(start, end)
    }
}

impl From<ast::Span> for Span {
    #[inline]
    fn from(value: ast::Span) -> Self {
        Span(value.start(), value.end())
    }
}

impl From<Span> for Option<Range<usize>> {
    fn from(value: Span) -> Self {
        (value != Span::NONE).then_some(value.0..value.1)
    }
}
