//! Standard library primitive declarations.

use smallvec::{SmallVec, smallvec};

use calyx_libm_utils::Format;

use super::Import;

#[derive(Clone, Copy)]
pub enum Parameters {
    Boolean,
    Integer,
    FixedPoint,
}

struct Arguments;

impl Arguments {
    const UNARY_DEFAULT: &[&str] = &["in"];

    const BINARY_DEFAULT: &[&str] = &["left", "right"];
}

pub struct Signature<'a> {
    pub args: &'a [&'a str],
    pub output: &'a str,
}

impl Signature<'_> {
    const UNARY_DEFAULT: Self = Self {
        args: Arguments::UNARY_DEFAULT,
        output: "out",
    };

    const BINARY_DEFAULT: Self = Self {
        args: Arguments::BINARY_DEFAULT,
        output: "out",
    };
}

pub struct Primitive<'a> {
    pub name: &'a str,
    pub prefix_hint: &'a str,
    pub signature: Signature<'a>,
    pub params: Parameters,
    pub is_comb: bool,
    pub import: Import,
}

impl Primitive<'_> {
    /// Constructs a parameter vector to instantiate the primitive for the given
    /// format.
    ///
    /// # Panics
    ///
    /// May panic if the primitive does not support the format.
    pub fn build_params(&self, format: &Format) -> SmallVec<[u64; 3]> {
        match self.params {
            Parameters::Boolean => smallvec![1],
            Parameters::Integer => smallvec![u64::from(format.width)],
            Parameters::FixedPoint => {
                let (int_width, frac_width) = format.parts().unwrap();

                smallvec![
                    u64::from(format.width),
                    u64::from(int_width),
                    u64::from(frac_width),
                ]
            }
        }
    }
}

pub mod compile {
    use super::*;

    pub const IMPORT: Import = Import::Compile;

    pub const STD_WIRE: Primitive = Primitive {
        name: "std_wire",
        prefix_hint: "id",
        signature: Signature::UNARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_ADD: Primitive = Primitive {
        name: "std_add",
        prefix_hint: "add",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };
}

pub mod core {
    use super::*;

    pub const IMPORT: Import = Import::Core;

    pub const STD_NOT: Primitive = Primitive {
        name: "std_not",
        prefix_hint: "com",
        signature: Signature::UNARY_DEFAULT,
        params: Parameters::Boolean,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_AND: Primitive = Primitive {
        name: "std_and",
        prefix_hint: "con",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Boolean,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_OR: Primitive = Primitive {
        name: "std_or",
        prefix_hint: "dis",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Boolean,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SUB: Primitive = Primitive {
        name: "std_sub",
        prefix_hint: "sub",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_GT: Primitive = Primitive {
        name: "std_gt",
        prefix_hint: "gt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_LT: Primitive = Primitive {
        name: "std_lt",
        prefix_hint: "lt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_EQ: Primitive = Primitive {
        name: "std_eq",
        prefix_hint: "eq",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_NEQ: Primitive = Primitive {
        name: "std_neq",
        prefix_hint: "neq",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_GE: Primitive = Primitive {
        name: "std_ge",
        prefix_hint: "ge",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_LE: Primitive = Primitive {
        name: "std_le",
        prefix_hint: "le",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };
}

pub mod binary_operators {
    use super::*;

    pub const IMPORT: Import = Import::BinaryOperators;

    pub const STD_FP_ADD: Primitive = Primitive {
        name: "std_fp_add",
        prefix_hint: "add",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_FP_SUB: Primitive = Primitive {
        name: "std_fp_sub",
        prefix_hint: "sub",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_FP_MULT_PIPE: Primitive = Primitive {
        name: "std_fp_mult_pipe",
        prefix_hint: "mul",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_FP_DIV_PIPE_REMAINDER: Primitive = Primitive {
        name: "std_fp_div_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_remainder",
        },
        params: Parameters::FixedPoint,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_FP_DIV_PIPE_QUOTIENT: Primitive = Primitive {
        name: "std_fp_div_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_quotient",
        },
        params: Parameters::FixedPoint,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_FP_GT: Primitive = Primitive {
        name: "std_fp_gt",
        prefix_hint: "gt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_FP_SADD: Primitive = Primitive {
        name: "std_fp_sadd",
        prefix_hint: "add",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_FP_SSUB: Primitive = Primitive {
        name: "std_fp_ssub",
        prefix_hint: "sub",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_FP_SMULT_PIPE: Primitive = Primitive {
        name: "std_fp_smult_pipe",
        prefix_hint: "mul",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_FP_SDIV_PIPE_REMAINDER: Primitive = Primitive {
        name: "std_fp_sdiv_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_remainder",
        },
        params: Parameters::FixedPoint,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_FP_SDIV_PIPE_QUOTIENT: Primitive = Primitive {
        name: "std_fp_sdiv_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_quotient",
        },
        params: Parameters::FixedPoint,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_FP_SGT: Primitive = Primitive {
        name: "std_fp_sgt",
        prefix_hint: "gt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_FP_SLT: Primitive = Primitive {
        name: "std_fp_slt",
        prefix_hint: "lt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_MULT_PIPE: Primitive = Primitive {
        name: "std_mult_pipe",
        prefix_hint: "mul",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_DIV_PIPE_REMAINDER: Primitive = Primitive {
        name: "std_div_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_remainder",
        },
        params: Parameters::Integer,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_DIV_PIPE_QUOTIENT: Primitive = Primitive {
        name: "std_div_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_quotient",
        },
        params: Parameters::Integer,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_SADD: Primitive = Primitive {
        name: "std_sadd",
        prefix_hint: "add",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SSUB: Primitive = Primitive {
        name: "std_ssub",
        prefix_hint: "sub",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SMULT_PIPE: Primitive = Primitive {
        name: "std_smult_pipe",
        prefix_hint: "mul",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_SDIV_PIPE_REMAINDER: Primitive = Primitive {
        name: "std_sdiv_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_remainder",
        },
        params: Parameters::Integer,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_SDIV_PIPE_QUOTIENT: Primitive = Primitive {
        name: "std_sdiv_pipe",
        prefix_hint: "div",
        signature: Signature {
            args: Arguments::BINARY_DEFAULT,
            output: "out_quotient",
        },
        params: Parameters::Integer,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_SGT: Primitive = Primitive {
        name: "std_sgt",
        prefix_hint: "gt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SLT: Primitive = Primitive {
        name: "std_slt",
        prefix_hint: "lt",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SEQ: Primitive = Primitive {
        name: "std_seq",
        prefix_hint: "eq",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SNEQ: Primitive = Primitive {
        name: "std_sneq",
        prefix_hint: "neq",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SGE: Primitive = Primitive {
        name: "std_sge",
        prefix_hint: "ge",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const STD_SLE: Primitive = Primitive {
        name: "std_sle",
        prefix_hint: "le",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };
}

pub mod math {
    use super::*;

    pub const IMPORT: Import = Import::Math;

    pub const STD_FP_SQRT: Primitive = Primitive {
        name: "fp_sqrt",
        prefix_hint: "sqrt",
        signature: Signature::UNARY_DEFAULT,
        params: Parameters::FixedPoint,
        is_comb: false,
        import: IMPORT,
    };

    pub const STD_SQRT: Primitive = Primitive {
        name: "sqrt",
        prefix_hint: "sqrt",
        signature: Signature::UNARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: false,
        import: IMPORT,
    };
}

pub mod numbers {
    use super::*;

    pub const IMPORT: Import = Import::Numbers;

    pub const NUM_SNEG: Primitive = Primitive {
        name: "num_sneg",
        prefix_hint: "neg",
        signature: Signature::UNARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const NUM_SABS: Primitive = Primitive {
        name: "num_sabs",
        prefix_hint: "abs",
        signature: Signature::UNARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const NUM_MIN: Primitive = Primitive {
        name: "num_min",
        prefix_hint: "min",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const NUM_MAX: Primitive = Primitive {
        name: "num_max",
        prefix_hint: "max",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const NUM_SMIN: Primitive = Primitive {
        name: "num_smin",
        prefix_hint: "min",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    pub const NUM_SMAX: Primitive = Primitive {
        name: "num_smax",
        prefix_hint: "max",
        signature: Signature::BINARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };

    #[allow(dead_code)]
    pub const NUM_SSIGNBIT: Primitive = Primitive {
        name: "num_ssignbit",
        prefix_hint: "sign",
        signature: Signature::UNARY_DEFAULT,
        params: Parameters::Integer,
        is_comb: true,
        import: IMPORT,
    };
}
