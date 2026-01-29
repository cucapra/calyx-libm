//! Numeric literals.

use malachite::Natural;
use malachite::num::arithmetic::traits::Pow;
use malachite::num::basic::traits::Zero;
use malachite::num::conversion::traits::{ExactFrom, FromStringBase};

pub use malachite::Rational;

/// Constructs a [`Rational`] from a sign and an unsigned numerator and
/// denominator. The sign indicates whether the rational, if non-zero, is
/// positive.
pub fn rational_from_ratio_str(
    sign: bool,
    numerator: &str,
    denominator: &str,
    radix: u8,
) -> Option<Rational> {
    let numerator = Natural::from_string_base(radix, numerator)?;
    let denominator = Natural::from_string_base(radix, denominator)?;

    Some(Rational::from_sign_and_naturals(
        sign,
        numerator,
        denominator,
    ))
}

/// Constructs a [`Rational`] from a sign and unsigned integer and fractional
/// parts. The sign indicates whether the rational, if non-zero, is positive.
pub fn rational_from_fixed_point_str(
    sign: bool,
    integer: &str,
    fraction: &str,
    radix: u8,
) -> Option<Rational> {
    let fraction = fraction.trim_end_matches('0');
    let w = fraction.len();

    let integer = Natural::from_string_base(radix, integer)?;

    let fraction = if w != 0 {
        Natural::from_string_base(radix, fraction)?
    } else {
        Natural::ZERO
    };

    let denominator = Natural::from(radix).pow(u64::exact_from(w));
    let numerator = integer * &denominator + fraction;

    Some(Rational::from_sign_and_naturals(
        sign,
        numerator,
        denominator,
    ))
}

/// Constructs a [`Rational`] from its components in scientific form.
///
/// The sign indicates whether the rational, if non-zero, is positive; the
/// integer and fractional parts are unsigned. The exponent may be signed and is
/// assumed to be in radix 10 regardless of the radix of the mantissa.
#[allow(clippy::from_str_radix_10)]
pub fn rational_from_scientific_str(
    sign: bool,
    integer: &str,
    fraction: &str,
    radix: u8,
    base: u8,
    exponent: &str,
) -> Option<Rational> {
    let mantissa =
        rational_from_fixed_point_str(sign, integer, fraction, radix)?;
    let exponent = i64::from_str_radix(exponent, 10).ok()?;

    Some(mantissa * Rational::from(base).pow(exponent))
}

/// Constructs a [`Rational`] from a sign and a triple of decimal integers
/// `(m, e, b)` representing the value `m * b^e`.
///
/// The sign indicates whether the rational, if non-zero, is positive; the
/// mantissa and base are unsigned. The exponent may be signed.
#[allow(clippy::from_str_radix_10)]
pub fn rational_from_digits(
    sign: bool,
    mantissa: &str,
    exponent: &str,
    base: &str,
) -> Option<Rational> {
    let mantissa = Natural::from_string_base(10, mantissa)?;
    let exponent = i64::from_str_radix(exponent, 10).ok()?;
    let base = Natural::from_string_base(10, base)?;

    let magnitude =
        Rational::from(mantissa) * Rational::from(base).pow(exponent);

    if sign {
        Some(magnitude)
    } else {
        Some(-magnitude)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fixed_point_parsing() {
        assert_eq!(
            rational_from_fixed_point_str(true, "1", "42", 10).unwrap(),
            Rational::from_signeds(71, 50)
        );
        assert_eq!(
            rational_from_fixed_point_str(true, "10", "8", 16).unwrap(),
            Rational::from_signeds(33, 2)
        );
        assert_eq!(
            rational_from_fixed_point_str(true, "0", "0101010", 2).unwrap(),
            Rational::from_signeds(21, 64)
        );
        assert_eq!(
            rational_from_fixed_point_str(true, "0Ab", "0", 16).unwrap(),
            Rational::from(171)
        );
        assert_eq!(
            rational_from_fixed_point_str(true, "0", "0", 2).unwrap(),
            Rational::ZERO
        );
    }

    #[test]
    fn scientific_parsing() {
        assert_eq!(
            rational_from_scientific_str(true, "1", "5", 10, 10, "3").unwrap(),
            Rational::from(1500)
        );
        assert_eq!(
            rational_from_scientific_str(true, "1", "8", 16, 2, "-10").unwrap(),
            Rational::from_signeds(3, 2048)
        );
        assert_eq!(
            rational_from_scientific_str(true, "1", "1", 2, 2, "0").unwrap(),
            Rational::from_signeds(3, 2)
        );
    }
}
