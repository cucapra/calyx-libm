use std::fmt;

use calyx_libm_ast as ast;

use super::Context;
use super::index as idx;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum SollyaExpr {
    Variable,
    Number(idx::NumIdx),
    Neg(idx::SollyaIdx),
    Binary(SollyaBinOp, idx::SollyaIdx, idx::SollyaIdx),
    Call(SollyaFn, idx::SollyaIdx),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum SollyaBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

impl SollyaBinOp {
    pub fn as_str(self) -> &'static str {
        match self {
            SollyaBinOp::Add => "+",
            SollyaBinOp::Sub => "-",
            SollyaBinOp::Mul => "*",
            SollyaBinOp::Div => "/",
            SollyaBinOp::Pow => "^",
        }
    }

    fn precedence(self) -> (u8, u8) {
        match self {
            SollyaBinOp::Pow => (0, 1),
            SollyaBinOp::Mul | SollyaBinOp::Div => (5, 4),
            SollyaBinOp::Add | SollyaBinOp::Sub => (7, 6),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[rustfmt::skip]
pub enum SollyaFn {
    Sin,  Cos,   Tan,  Sinh,  Cosh,  Tanh,
    ASin, ACos,  ATan, ASinh, ACosh, ATanh,
    Exp,  ExpM1, Log,  Log2,  Log10, Log1P,
    Erf,  ErfC,  Sqrt,
}

impl SollyaFn {
    pub fn as_str(self) -> &'static str {
        match self {
            SollyaFn::Sin => "sin",
            SollyaFn::Cos => "cos",
            SollyaFn::Tan => "tan",
            SollyaFn::Sinh => "sinh",
            SollyaFn::Cosh => "cosh",
            SollyaFn::Tanh => "tanh",
            SollyaFn::ASin => "asin",
            SollyaFn::ACos => "acos",
            SollyaFn::ATan => "atan",
            SollyaFn::ASinh => "asinh",
            SollyaFn::ACosh => "acosh",
            SollyaFn::ATanh => "atanh",
            SollyaFn::Exp => "exp",
            SollyaFn::ExpM1 => "expm1",
            SollyaFn::Log => "log",
            SollyaFn::Log2 => "log2",
            SollyaFn::Log10 => "log10",
            SollyaFn::Log1P => "log1p",
            SollyaFn::Erf => "erf",
            SollyaFn::ErfC => "erfc",
            SollyaFn::Sqrt => "sqrt",
        }
    }
}

impl TryFrom<ast::MathOp> for SollyaFn {
    type Error = ();

    fn try_from(value: ast::MathOp) -> Result<Self, Self::Error> {
        match value {
            ast::MathOp::Sin => Ok(SollyaFn::Sin),
            ast::MathOp::Cos => Ok(SollyaFn::Cos),
            ast::MathOp::Tan => Ok(SollyaFn::Tan),
            ast::MathOp::Sinh => Ok(SollyaFn::Sinh),
            ast::MathOp::Cosh => Ok(SollyaFn::Cosh),
            ast::MathOp::Tanh => Ok(SollyaFn::Tanh),
            ast::MathOp::ASin => Ok(SollyaFn::ASin),
            ast::MathOp::ACos => Ok(SollyaFn::ACos),
            ast::MathOp::ATan => Ok(SollyaFn::ATan),
            ast::MathOp::ASinh => Ok(SollyaFn::ASinh),
            ast::MathOp::ACosh => Ok(SollyaFn::ACosh),
            ast::MathOp::ATanh => Ok(SollyaFn::ATanh),
            ast::MathOp::Exp => Ok(SollyaFn::Exp),
            ast::MathOp::ExpM1 => Ok(SollyaFn::ExpM1),
            ast::MathOp::Log => Ok(SollyaFn::Log),
            ast::MathOp::Log2 => Ok(SollyaFn::Log2),
            ast::MathOp::Log10 => Ok(SollyaFn::Log10),
            ast::MathOp::Log1P => Ok(SollyaFn::Log1P),
            ast::MathOp::Erf => Ok(SollyaFn::Erf),
            ast::MathOp::ErfC => Ok(SollyaFn::ErfC),
            ast::MathOp::Sqrt => Ok(SollyaFn::Sqrt),
            _ => Err(()),
        }
    }
}

impl idx::SollyaIdx {
    /// If `self` represents a named function, get its name as an identifier.
    pub fn name(self, ctx: &Context) -> Option<&'static str> {
        if let SollyaExpr::Call(f, arg) = ctx[self]
            && let SollyaExpr::Variable = ctx[arg]
        {
            Some(f.as_str())
        } else {
            None
        }
    }

    /// Returns an adapter for formatting `self` as a pretty-printed function.
    pub fn pretty(self, ctx: &Context) -> impl fmt::Display {
        struct Pretty<'ctx> {
            ctx: &'ctx Context,
            idx: idx::SollyaIdx,
        }

        impl fmt::Display for Pretty<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                if let Some(name) = self.idx.name(self.ctx) {
                    write!(f, "{name}")
                } else {
                    Printer::write(self.idx, self.ctx, "x", f)
                }
            }
        }

        Pretty { ctx, idx: self }
    }

    /// Returns an adapter for formatting `self` as a Sollya expression in the
    /// free variable.
    pub fn sollya(self, ctx: &Context) -> impl fmt::Display {
        struct Sollya<'ctx> {
            ctx: &'ctx Context,
            idx: idx::SollyaIdx,
        }

        impl fmt::Display for Sollya<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                Printer::write(self.idx, self.ctx, "_x_", f)
            }
        }

        Sollya { ctx, idx: self }
    }
}

struct Printer<'a, 'b> {
    ctx: &'a Context,
    var: &'a str,
    f: &'a mut fmt::Formatter<'b>,
}

impl Printer<'_, '_> {
    fn write(
        idx: idx::SollyaIdx,
        ctx: &Context,
        var: &str,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        Printer { ctx, var, f }.pretty(idx, u8::MAX)
    }

    fn pretty(&mut self, idx: idx::SollyaIdx, parent: u8) -> fmt::Result {
        match self.ctx[idx] {
            SollyaExpr::Variable => {
                write!(self.f, "{}", self.var)
            }
            SollyaExpr::Number(num) => {
                const NEG_PRECEDENCE: u8 = 3;
                const DIV_PRECEDENCE: u8 = 5;

                let value = &self.ctx[num].value;

                let precedence = if *value.denominator_ref() != 1u32 {
                    DIV_PRECEDENCE
                } else if *value < 0u32 {
                    NEG_PRECEDENCE
                } else {
                    0
                };

                if parent < precedence {
                    write!(self.f, "({value})")
                } else {
                    write!(self.f, "{value}")
                }
            }
            SollyaExpr::Neg(arg) => {
                const NEG_LP: u8 = 3;
                const NEG_RP: u8 = 2;

                if parent < NEG_LP {
                    write!(self.f, "(-")?;
                    self.pretty(arg, NEG_RP)?;
                    write!(self.f, ")")
                } else {
                    write!(self.f, "-")?;
                    self.pretty(arg, NEG_RP)
                }
            }
            SollyaExpr::Binary(op, lhs, rhs) => {
                let (lp, rp) = op.precedence();
                let max = rp | 1; // = max(lp, rp)

                if parent < max {
                    write!(self.f, "(")?;
                }

                self.pretty(lhs, lp)?;

                if op == SollyaBinOp::Pow {
                    write!(self.f, "^")?;
                } else {
                    write!(self.f, " {} ", op.as_str())?;
                }

                self.pretty(rhs, rp)?;

                if parent < max {
                    write!(self.f, ")")?;
                }

                Ok(())
            }
            SollyaExpr::Call(op, arg) => {
                write!(self.f, "{}(", op.as_str())?;
                self.pretty(arg, u8::MAX)?;
                write!(self.f, ")")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use SollyaBinOp::*;
    use SollyaExpr::*;

    #[test]
    fn precedence() {
        let mut ctx = Context::new();

        let var = ctx.ops.intern(Variable);
        let add = ctx.ops.intern(Binary(Add, var, var));
        let pow = ctx.ops.intern(Binary(Pow, var, var));
        let neg_add = ctx.ops.intern(Neg(add));
        let neg_pow = ctx.ops.intern(Neg(pow));
        let mul = ctx.ops.intern(Binary(Mul, neg_add, neg_pow));

        assert_eq!(mul.pretty(&ctx).to_string(), "-(x + x) * -x^x");

        let num = ctx.numbers.push(crate::Number {
            value: crate::Rational::from_unsigneds::<u32>(1, 2),
            span: crate::Span::new(0, 3),
        });

        let num = ctx.ops.intern(Number(num));
        let add_num = ctx.ops.intern(Binary(Add, var, num));
        let pow_num = ctx.ops.intern(Binary(Pow, var, num));

        assert_eq!(add_num.pretty(&ctx).to_string(), "x + 1/2");
        assert_eq!(pow_num.pretty(&ctx).to_string(), "x^(1/2)");

        let call = ctx.ops.intern(Call(SollyaFn::Sqrt, add));

        assert_eq!(call.pretty(&ctx).to_string(), "sqrt(x + x)");
    }

    #[test]
    fn associativity() {
        let mut ctx = Context::new();

        let var = ctx.ops.intern(Variable);
        let neg = ctx.ops.intern(Neg(var));
        let double_neg = ctx.ops.intern(Neg(neg));

        assert_eq!(double_neg.pretty(&ctx).to_string(), "-(-x)");

        let sub = ctx.ops.intern(Binary(Sub, var, var));
        let sub_left = ctx.ops.intern(Binary(Sub, sub, var));
        let sub_right = ctx.ops.intern(Binary(Sub, var, sub));

        assert_eq!(sub_left.pretty(&ctx).to_string(), "x - x - x");
        assert_eq!(sub_right.pretty(&ctx).to_string(), "x - (x - x)");

        let pow = ctx.ops.intern(Binary(Pow, var, var));
        let pow_left = ctx.ops.intern(Binary(Pow, pow, var));
        let pow_right = ctx.ops.intern(Binary(Pow, var, pow));

        assert_eq!(pow_left.pretty(&ctx).to_string(), "(x^x)^x");
        assert_eq!(pow_right.pretty(&ctx).to_string(), "x^x^x");
    }

    #[test]
    fn literals() {
        let mut ctx = Context::new();

        let pos = ctx.numbers.push(crate::Number {
            value: crate::Rational::from(2),
            span: crate::Span::new(0, 1),
        });

        let neg = ctx.numbers.push(crate::Number {
            value: crate::Rational::from(-2),
            span: crate::Span::new(0, 2),
        });

        let var = ctx.ops.intern(Variable);
        let pos = ctx.ops.intern(Number(pos));
        let neg = ctx.ops.intern(Number(neg));
        let pow_pos = ctx.ops.intern(Binary(Pow, pos, var));
        let pow_neg = ctx.ops.intern(Binary(Pow, neg, var));

        assert_eq!(pow_pos.pretty(&ctx).to_string(), "2^x");
        assert_eq!(pow_neg.pretty(&ctx).to_string(), "(-2)^x");
    }
}
