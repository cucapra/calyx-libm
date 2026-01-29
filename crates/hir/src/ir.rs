use calyx_libm_ast::ast;

use super::arena::{EntityList, PackedOption};
use super::index as idx;

pub use ast::{
    Constant, Id, MathConst, Number, Rational, Span, Symbol, TestOp,
};

pub use super::sollya::{SollyaBinOp, SollyaExpr, SollyaFn};

pub struct Definition {
    pub name: Option<Symbol>,
    pub args: idx::IndexRange<idx::ArgIdx>,
    pub scope: PackedOption<idx::ScopeIdx>,
    pub body: idx::ExprIdx,
}

pub struct Argument {
    pub var: Symbol,
    pub scope: PackedOption<idx::ScopeIdx>,
}

pub enum ExprKind {
    Num(idx::NumIdx),
    Const(Constant),
    Var(idx::VarIdx, VarKind),
    Op(Operation, EntityList<idx::ExprIdx>),
    If(If),
    Let(Let),
    While(While),
}

pub struct Expression {
    pub kind: ExprKind,
    pub scope: PackedOption<idx::ScopeIdx>,
    pub span: Span,
}

#[derive(Clone, Copy)]
pub enum VarKind {
    Arg(idx::ArgIdx),
    Let(idx::ExprIdx),
    Mut,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Pow,
    Sqrt,
    Abs,
    Max,
    Min,
}

#[derive(Clone, Copy)]
pub enum OpKind {
    Arith(ArithOp),
    Test(TestOp),
    Sollya(idx::SollyaIdx),
    Def(idx::DefIdx),
}

pub struct Operation {
    pub kind: OpKind,
    pub span: Span,
}

pub struct If {
    pub cond: idx::ExprIdx,
    pub if_true: idx::ExprIdx,
    pub if_false: idx::ExprIdx,
}

pub struct Let {
    pub writes: EntityList<idx::WriteIdx>,
    pub body: idx::ExprIdx,
    pub sequential: bool,
}

pub struct While {
    pub cond: idx::ExprIdx,
    pub inits: EntityList<idx::WriteIdx>,
    pub updates: EntityList<idx::WriteIdx>,
    pub body: idx::ExprIdx,
    pub sequential: bool,
}

pub struct Write {
    pub var: idx::VarIdx,
    pub val: idx::ExprIdx,
}

pub struct Scope {
    pub prop: Property,
    pub parent: PackedOption<idx::ScopeIdx>,
}

pub enum Property {
    Pre(idx::ExprIdx),
    Domain(Domain),
    Impl(Strategy),
}

#[derive(Clone, Copy)]
pub struct Domain {
    pub left: idx::NumIdx,
    pub right: idx::NumIdx,
}

#[derive(Clone, Copy)]
pub enum Strategy {
    Lut {
        size: u32,
    },
    Poly {
        degree: u32,
        error: PackedOption<idx::NumIdx>,
    },
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
            ast::MathOp::Sqrt => Ok(ArithOp::Sqrt),
            ast::MathOp::FAbs => Ok(ArithOp::Abs),
            ast::MathOp::FMax => Ok(ArithOp::Max),
            ast::MathOp::FMin => Ok(ArithOp::Min),
            _ => Err(()),
        }
    }
}
