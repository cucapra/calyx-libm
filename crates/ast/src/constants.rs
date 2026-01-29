//! Built-in operations and constants.

#![allow(unreachable_patterns)]

use strum_macros::{EnumString, IntoStaticStr};

#[derive(Clone, Copy, Debug, EnumString, IntoStaticStr)]
#[rustfmt::skip]
pub enum MathOp {
    #[strum(to_string = "+")] Add,
    #[strum(to_string = "-")] Sub,
    #[strum(to_string = "*")] Mul,
    #[strum(to_string = "/")] Div,
    #[strum(to_string = "-")] Neg,
    #[strum(to_string = "fabs")] FAbs,
    #[strum(to_string = "fma")] FMA,
    #[strum(to_string = "exp")] Exp,
    #[strum(to_string = "exp2")] Exp2,
    #[strum(to_string = "expm1")] ExpM1,
    #[strum(to_string = "log")] Log,
    #[strum(to_string = "log10")] Log10,
    #[strum(to_string = "log2")] Log2,
    #[strum(to_string = "log1p")] Log1P,
    #[strum(to_string = "pow")] Pow,
    #[strum(to_string = "sqrt")] Sqrt,
    #[strum(to_string = "cbrt")] Cbrt,
    #[strum(to_string = "hypot")] Hypot,
    #[strum(to_string = "sin")] Sin,
    #[strum(to_string = "cos")] Cos,
    #[strum(to_string = "tan")] Tan,
    #[strum(to_string = "asin")] ASin,
    #[strum(to_string = "acos")] ACos,
    #[strum(to_string = "atan")] ATan,
    #[strum(to_string = "atan2")] ATan2,
    #[strum(to_string = "sinh")] Sinh,
    #[strum(to_string = "cosh")] Cosh,
    #[strum(to_string = "tanh")] Tanh,
    #[strum(to_string = "asinh")] ASinh,
    #[strum(to_string = "acosh")] ACosh,
    #[strum(to_string = "atanh")] ATanh,
    #[strum(to_string = "erf")] Erf,
    #[strum(to_string = "erfc")] ErfC,
    #[strum(to_string = "tgamma")] TGamma,
    #[strum(to_string = "lgamma")] LGamma,
    #[strum(to_string = "ceil")] Ceil,
    #[strum(to_string = "floor")] Floor,
    #[strum(to_string = "fmod")] FMod,
    #[strum(to_string = "remainder")] Remainder,
    #[strum(to_string = "fmax")] FMax,
    #[strum(to_string = "fmin")] FMin,
    #[strum(to_string = "fdim")] FDim,
    #[strum(to_string = "copysign")] CopySign,
    #[strum(to_string = "trunc")] Trunc,
    #[strum(to_string = "round")] Round,
    #[strum(to_string = "nearbyint")] NearbyInt,
}

impl MathOp {
    pub fn arity(self) -> u8 {
        match self {
            MathOp::Add => 2,
            MathOp::Sub => 2,
            MathOp::Mul => 2,
            MathOp::Div => 2,
            MathOp::Neg => 1,
            MathOp::FAbs => 1,
            MathOp::FMA => 3,
            MathOp::Exp => 1,
            MathOp::Exp2 => 1,
            MathOp::ExpM1 => 1,
            MathOp::Log => 1,
            MathOp::Log10 => 1,
            MathOp::Log2 => 1,
            MathOp::Log1P => 1,
            MathOp::Pow => 2,
            MathOp::Sqrt => 1,
            MathOp::Cbrt => 1,
            MathOp::Hypot => 2,
            MathOp::Sin => 1,
            MathOp::Cos => 1,
            MathOp::Tan => 1,
            MathOp::ASin => 1,
            MathOp::ACos => 1,
            MathOp::ATan => 1,
            MathOp::ATan2 => 2,
            MathOp::Sinh => 1,
            MathOp::Cosh => 1,
            MathOp::Tanh => 1,
            MathOp::ASinh => 1,
            MathOp::ACosh => 1,
            MathOp::ATanh => 1,
            MathOp::Erf => 1,
            MathOp::ErfC => 1,
            MathOp::TGamma => 1,
            MathOp::LGamma => 1,
            MathOp::Ceil => 1,
            MathOp::Floor => 1,
            MathOp::FMod => 2,
            MathOp::Remainder => 2,
            MathOp::FMax => 2,
            MathOp::FMin => 2,
            MathOp::FDim => 2,
            MathOp::CopySign => 2,
            MathOp::Trunc => 1,
            MathOp::Round => 1,
            MathOp::NearbyInt => 1,
        }
    }
}

#[derive(Clone, Copy, Debug, EnumString, IntoStaticStr)]
#[rustfmt::skip]
pub enum TestOp {
    #[strum(to_string = "<")] Lt,
    #[strum(to_string = ">")] Gt,
    #[strum(to_string = "<=")] Leq,
    #[strum(to_string = ">=")] Geq,
    #[strum(to_string = "==")] Eq,
    #[strum(to_string = "!=")] Neq,
    #[strum(to_string = "and")] And,
    #[strum(to_string = "or")] Or,
    #[strum(to_string = "not")] Not,
    #[strum(to_string = "isfinite")] IsFinite,
    #[strum(to_string = "isinf")] IsInf,
    #[strum(to_string = "isnan")] IsNan,
    #[strum(to_string = "isnormal")] IsNormal,
    #[strum(to_string = "signbit")] SignBit,
}

impl TestOp {
    pub fn is_variadic(self) -> bool {
        matches!(
            self,
            TestOp::Lt
                | TestOp::Gt
                | TestOp::Leq
                | TestOp::Geq
                | TestOp::Eq
                | TestOp::Neq
                | TestOp::And
                | TestOp::Or
        )
    }
}

#[derive(Clone, Copy, Debug, EnumString, IntoStaticStr)]
#[rustfmt::skip]
pub enum TensorOp {
    #[strum(to_string = "dim")] Dim,
    #[strum(to_string = "size")] Size,
    #[strum(to_string = "ref")] Ref,
}

#[derive(Clone, Copy, Debug, EnumString, IntoStaticStr)]
#[allow(non_camel_case_types)]
#[rustfmt::skip]
pub enum MathConst {
    #[strum(to_string = "E")] E,
    #[strum(to_string = "LOG2E")] LOG2E,
    #[strum(to_string = "LOG10E")] LOG10E,
    #[strum(to_string = "LN2")] LN2,
    #[strum(to_string = "LN10")] LN10,
    #[strum(to_string = "PI")] PI,
    #[strum(to_string = "PI_2")] PI_2,
    #[strum(to_string = "PI_4")] PI_4,
    #[strum(to_string = "M_1_PI")] M_1_PI,
    #[strum(to_string = "M_2_PI")] M_2_PI,
    #[strum(to_string = "M_2_SQRTPI")] M_2_SQRTPI,
    #[strum(to_string = "SQRT2")] SQRT2,
    #[strum(to_string = "SQRT1_2")] SQRT1_2,
    #[strum(to_string = "INFINITY")] INFINITY,
    #[strum(to_string = "NAN")] NAN,
}
