//! Name mangling.

use std::cmp::Ordering;
use std::fmt;
use std::hash::{DefaultHasher, Hasher};

use malachite::num::arithmetic::traits::Sign;
use malachite::{Natural, Rational};

pub use calyx_libm_macros::Mangle;

/// Encodes context information into an identifier. The resulting identifier is
/// a valid name in the IA-64 C++ ABI's name mangling scheme.
#[macro_export]
macro_rules! mangle {
    ($name:expr, $($arg:expr),+ $(,)?) => {{
        use $crate::Mangle as _;

        let mut res = match $name {
            name => ::std::format!("_Z{}{}I", name.len(), name),
        };

        $(
            res.push('X');
            $arg.mangle(&mut res).unwrap();
            res.push('E');
        )+

        res.push('E');
        res
    }};
}

pub use mangle;

/// A trait for formatting data as a mangled expression.
pub trait Mangle {
    /// Formats `self` as an [`<expression>`].
    ///
    /// [`<expression>`]:
    ///     https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.expression
    fn mangle(&self, w: &mut dyn fmt::Write) -> fmt::Result;
}

/// Formats an [`<expression>`] given an [`<identifier>`] and a list of
/// [`<expression>`].
///
/// [`<expression>`]:
///     https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.expression
/// [`<identifier>`]:
///     https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.identifier
macro_rules! init_list {
    ($dst:ident, $type:literal, $($expr:expr),+ $(,)?) => {{
        write!($dst, "tl{}{}", $type.len(), $type)?;
        $(
            $expr.mangle($dst)?;
        )+
        write!($dst, "E")
    }};
}

impl Mangle for bool {
    fn mangle(&self, w: &mut dyn fmt::Write) -> fmt::Result {
        write!(w, "Lb{}E", *self as u8)
    }
}

macro_rules! impl_mangle_unsigned {
    ($($type:ty),*) => {$(
        impl Mangle for $type {
            fn mangle(&self, w: &mut dyn fmt::Write) -> fmt::Result {
                write!(w, "Lj{}E", self)
            }
        }
    )*};
}

impl_mangle_unsigned!(usize, u8, u16, u32, u64, Natural);

macro_rules! impl_mangle_signed {
    ($($type:ty),*) => {$(
        impl Mangle for $type {
            fn mangle(&self, w: &mut dyn fmt::Write) -> fmt::Result {
                if *self < 0 {
                    write!(w, "Lin{}E", self.unsigned_abs())
                } else {
                    write!(w, "Li{}E", self)
                }
            }
        }
    )*};
}

impl_mangle_signed!(isize, i8, i16, i32, i64);

impl<T: Mangle> Mangle for [T] {
    fn mangle(&self, w: &mut dyn fmt::Write) -> fmt::Result {
        write!(w, "il")?;

        for expr in self {
            expr.mangle(w)?;
        }

        write!(w, "E")
    }
}

impl Mangle for Rational {
    fn mangle(&self, w: &mut dyn fmt::Write) -> fmt::Result {
        init_list!(
            w,
            "Rational",
            self.sign() == Ordering::Less,
            self.numerator_ref(),
            self.denominator_ref(),
        )
    }
}

#[derive(Clone, Copy, Mangle)]
pub struct Hash(u64);

impl Hash {
    pub fn new<T>(data: &T) -> Hash
    where
        T: std::hash::Hash + ?Sized,
    {
        let mut hasher = DefaultHasher::new();
        data.hash(&mut hasher);

        Hash(hasher.finish())
    }
}
