//! Dyadic notation.

use std::cmp::Ordering;
use std::fmt;

use malachite::num::arithmetic::traits::{IsPowerOf2, Sign};
use malachite::num::logic::traits::SignificantBits;
use malachite::{Integer, Rational};

pub trait Dyadic: Sized {
    /// Parses a number in Gappa dyadic notation.
    ///
    /// The accepted format is defined by the following regular expression:
    /// `[-+]?[0-9]+([bB][-+]?[0-9]+)?`.
    fn from_dyadic(s: &str) -> Option<Self>;

    /// Returns an adapter for formatting `self` in Gappa dyadic notation.
    ///
    /// This method is infallible. However, attempting to format a non-dyadic
    /// number with the returned adapter will result in a panic.
    fn dyadic(&self) -> impl fmt::Display;
}

impl Dyadic for Rational {
    fn from_dyadic(s: &str) -> Option<Self> {
        if let Some((m, e)) = s.split_once(['b', 'B']) {
            let mantissa: Integer = m.parse().ok()?;
            let exponent: i64 = e.parse().ok()?;

            Some(Rational::from(mantissa) << exponent)
        } else {
            let value: Integer = s.parse().ok()?;

            Some(Rational::from(value))
        }
    }

    fn dyadic(&self) -> impl fmt::Display {
        struct Dyadic<'a>(&'a Rational);

        impl fmt::Display for Dyadic<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let exponent = self.0.fraction_bits().unwrap();

                if self.0.sign() == Ordering::Less {
                    write!(f, "-")?
                }

                write!(f, "{}", self.0.numerator_ref())?;

                if exponent != 0 {
                    write!(f, "b-{}", exponent)
                } else {
                    Ok(())
                }
            }
        }

        Dyadic(self)
    }
}

pub trait FractionBits {
    /// Returns the number of fraction bits needed to represent `self`, or
    /// `None` if no finite number suffices.
    fn fraction_bits(self) -> Option<u64>;
}

impl FractionBits for &Rational {
    fn fraction_bits(self) -> Option<u64> {
        let denominator = self.denominator_ref();

        if denominator.is_power_of_2() {
            Some(denominator.significant_bits() - 1)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dyadic_parsing() {
        assert_eq!(
            Rational::from_dyadic("+01B+01").unwrap(),
            Rational::from(2)
        );
        assert_eq!(
            Rational::from_dyadic("-3b-2").unwrap(),
            Rational::from_signeds(-3, 4)
        );
    }
}
