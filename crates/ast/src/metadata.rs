//! Metadata properties.

use strum_macros::{EnumString, IntoStaticStr};

use super::ast::{Binding, Expression, Number, Symbol};

#[derive(Debug)]
#[non_exhaustive]
pub enum PropKind {
    /// Benchmark name.
    Name(String),
    /// Benchmark description.
    Description(String),
    /// List of sources for the benchmark.
    Cite(Vec<Symbol>),
    /// Precision used for rounding.
    Precision(Precision),
    /// Rounding mode.
    Round(RoundingMode),
    /// Overflow behavior for fixed-point values.
    Overflow(OverflowMode),
    /// Precondition on benchmark inputs.
    Pre(Expression),
    /// Real-valued function approximated by the benchmark.
    Spec(Expression),
    /// Alternate formulation of the benchmark.
    Alt(Expression),
    /// Library to be used for mathematical operations.
    MathLib(Symbol),
    /// Example input.
    Example(Vec<Binding>),
    /// Input domain for generated math functions.
    CalyxDomain(CalyxDomain),
    /// Implementation strategy for generated math functions.
    CalyxImpl(CalyxImpl),
    /// Unknown property.
    Unknown(Symbol, Data),
}

/// Property data.
#[derive(Debug)]
#[non_exhaustive]
pub enum Data {
    Symbol(Symbol),
    Num(Number),
    Str(String),
    List(Vec<Data>),
}

impl PropKind {
    pub fn name(&self) -> &str {
        match self {
            PropKind::Name(_) => "name",
            PropKind::Description(_) => "description",
            PropKind::Cite(_) => "cite",
            PropKind::Precision(_) => "precision",
            PropKind::Round(_) => "round",
            PropKind::Overflow(_) => "overflow",
            PropKind::Pre(_) => "pre",
            PropKind::Spec(_) => "spec",
            PropKind::Alt(_) => "alt",
            PropKind::MathLib(_) => "math-library",
            PropKind::Example(_) => "example",
            PropKind::CalyxDomain(_) => "calyx-domain",
            PropKind::CalyxImpl(_) => "calyx-impl",
            PropKind::Unknown(name, _) => name.id.as_ref(),
        }
    }
}

/// A precision family and its parameters.
#[derive(Clone, Copy, Debug)]
pub enum Precision {
    Float { e: u32, nbits: u32 },
    Posit { es: u32, nbits: u32 },
    Fixed { scale: i32, nbits: u32 },
    Real,
    Integer,
}

impl Precision {
    pub fn from_shorthand(s: &str) -> Option<Self> {
        match s {
            "binary16" => Some(Precision::Float { e: 5, nbits: 16 }),
            "binary32" => Some(Precision::Float { e: 8, nbits: 32 }),
            "binary64" => Some(Precision::Float { e: 11, nbits: 64 }),
            "binary128" => Some(Precision::Float { e: 15, nbits: 128 }),
            "binary256" => Some(Precision::Float { e: 19, nbits: 256 }),
            "posit8" => Some(Precision::Posit { es: 0, nbits: 8 }),
            "posit16" => Some(Precision::Posit { es: 1, nbits: 16 }),
            "posit32" => Some(Precision::Posit { es: 2, nbits: 32 }),
            "posit64" => Some(Precision::Posit { es: 3, nbits: 64 }),
            "real" => Some(Precision::Real),
            "integer" => Some(Precision::Integer),
            _ => None,
        }
    }
}

/// An IEEE 754 rounding mode.
#[derive(Clone, Copy, Debug, EnumString, IntoStaticStr)]
pub enum RoundingMode {
    #[strum(to_string = "nearestEven")]
    NearestEven,
    #[strum(to_string = "nearestAway")]
    NearestAway,
    #[strum(to_string = "toPositive")]
    ToPositive,
    #[strum(to_string = "toNegative")]
    ToNegative,
    #[strum(to_string = "toZero")]
    ToZero,
}

#[derive(Clone, Copy, Debug, EnumString, IntoStaticStr)]
pub enum OverflowMode {
    #[strum(to_string = "infinity")]
    Infinity,
    #[strum(to_string = "clamp")]
    Clamp,
    #[strum(to_string = "wrap")]
    Wrap,
}

/// An input domain for a function.
#[derive(Debug)]
pub struct CalyxDomain {
    pub left: Number,
    pub right: Number,
}

/// An implementation strategy and its parameters.
#[derive(Debug)]
pub enum CalyxImpl {
    Lut { size: u32 },
    Poly { degree: u32, error: Option<Number> },
}
