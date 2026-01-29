//! Numeric conversions.

use std::cmp::Ordering;

use malachite::num::arithmetic::traits::{CheckedSub, PowerOf2, Sign};
use malachite::num::basic::traits::Zero;
use malachite::num::logic::traits::SignificantBits;
use malachite::{Natural, Rational};

use super::FractionBits;
use crate::Format;

/// A trait for converting to fixed-point representation.
pub trait FixedPoint {
    type Output;

    /// Computes the unsigned fixed-point representation of `self`.
    fn to_unsigned_fixed_point(
        &self,
        scale: i32,
        width: u32,
    ) -> Option<Self::Output>;

    /// Computes the signed fixed-point representation of `self`.
    fn to_signed_fixed_point(
        &self,
        scale: i32,
        width: u32,
    ) -> Option<Self::Output>;

    /// Computes the fixed-point representation of `self`.
    fn to_fixed_point(&self, format: &Format) -> Option<Self::Output> {
        if format.is_signed {
            self.to_signed_fixed_point(format.scale, format.width)
        } else {
            self.to_unsigned_fixed_point(format.scale, format.width)
        }
    }
}

impl FixedPoint for Rational {
    type Output = Natural;

    fn to_unsigned_fixed_point(
        &self,
        scale: i32,
        width: u32,
    ) -> Option<Natural> {
        if self.sign() == Ordering::Less {
            return None;
        }

        let value = to_scaled_integer(self, scale)?;

        if value.significant_bits() <= u64::from(width) {
            Some(value)
        } else {
            None
        }
    }

    fn to_signed_fixed_point(&self, scale: i32, width: u32) -> Option<Natural> {
        if self.sign() == Ordering::Equal {
            return Some(Natural::ZERO);
        }

        let value = to_scaled_integer(self, scale)?;

        if self.sign() == Ordering::Less {
            let complement =
                Natural::power_of_2(u64::from(width)).checked_sub(&value)?;

            // Check that value <= 2^(width-1)
            if value <= complement {
                return Some(complement);
            }
        } else if value.significant_bits() < u64::from(width) {
            return Some(value);
        }

        None
    }
}

fn to_scaled_integer(magnitude: &Rational, scale: i32) -> Option<Natural> {
    let numerator = magnitude.numerator_ref();
    let denominator = magnitude.denominator_ref();

    let delta = u64::from(scale.unsigned_abs());

    if scale <= 0 {
        Some(numerator << delta.checked_sub(magnitude.fraction_bits()?)?)
    } else {
        let Some(zeros) = numerator.trailing_zeros() else {
            return Some(Natural::ZERO);
        };

        if *denominator == 1u32 && zeros >= delta {
            Some(numerator >> delta)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use malachite::num::conversion::traits::FromSciString;

    #[test]
    fn unsigned_fixed_point_conversion() {
        assert_eq!(
            Rational::from_sci_string("42.125")
                .unwrap()
                .to_unsigned_fixed_point(-16, 32)
                .unwrap(),
            2760704u32
        );
        assert_eq!(
            Rational::from_sci_string("1.14404296875")
                .unwrap()
                .to_unsigned_fixed_point(-31, 32)
                .unwrap(),
            2456813568u32
        );
        assert_eq!(
            Rational::from(0).to_unsigned_fixed_point(-16, 32).unwrap(),
            0u32
        );
        assert_eq!(
            Rational::from(20).to_unsigned_fixed_point(2, 32).unwrap(),
            5u32
        );
    }

    #[test]
    fn unsigned_fixed_point_conversion_widths() {
        assert_eq!(
            Rational::from_sci_string("42.125")
                .unwrap()
                .to_unsigned_fixed_point(-2, 32),
            None
        );
        assert_eq!(
            Rational::from_sci_string("1.1")
                .unwrap()
                .to_unsigned_fixed_point(-32, 64),
            None
        );
        assert_eq!(
            Rational::from(65536).to_unsigned_fixed_point(-16, 32),
            None
        );
        assert_eq!(Rational::from(3).to_unsigned_fixed_point(1, 32), None);
        assert_eq!(
            Rational::from_sci_string("0.4")
                .unwrap()
                .to_unsigned_fixed_point(1, 32),
            None
        );
        assert_eq!(
            Rational::from_unsigneds(1, 0x100000000u64)
                .to_unsigned_fixed_point(-32, 32)
                .unwrap(),
            1u32
        );
        assert_eq!(
            Rational::from(0xffffffffu32)
                .to_unsigned_fixed_point(0, 32)
                .unwrap(),
            0xffffffffu32
        );
        assert_eq!(
            Rational::from(0x100000000u64)
                .to_unsigned_fixed_point(1, 64)
                .unwrap(),
            0x80000000u32
        );
    }

    #[test]
    fn signed_fixed_point_conversion() {
        assert_eq!(
            Rational::from(-1).to_signed_fixed_point(-16, 32).unwrap(),
            0xffff0000u32
        );
        assert_eq!(
            Rational::from_sci_string("-42.125")
                .unwrap()
                .to_signed_fixed_point(-16, 32)
                .unwrap(),
            4292206592u32
        );
        assert_eq!(
            Rational::from(-3).to_signed_fixed_point(0, 3).unwrap(),
            5u32
        );
        assert_eq!(
            Rational::from(-4).to_signed_fixed_point(0, 3).unwrap(),
            4u32
        );
        assert_eq!(
            Rational::from(-20).to_signed_fixed_point(2, 32).unwrap(),
            0xfffffffbu32
        );
    }

    #[test]
    fn signed_fixed_point_conversion_widths() {
        assert_eq!(Rational::from(-5).to_signed_fixed_point(0, 3), None);
        assert_eq!(Rational::from(-65536).to_signed_fixed_point(-4, 8), None);
        assert_eq!(
            Rational::from_sci_string("0.5")
                .unwrap()
                .to_signed_fixed_point(-1, 1),
            None
        );
        assert_eq!(
            Rational::from_sci_string("-0.5")
                .unwrap()
                .to_signed_fixed_point(-1, 1)
                .unwrap(),
            1u32
        );
    }
}
