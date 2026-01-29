//! Numeric formats.

use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

use itertools::PeekingNext;

use crate::Mangle;

/// A fixed-point number format with `width` total bits and a scaling factor of
/// 2^`scale`.
#[derive(Clone, Copy, Mangle)]
pub struct Format {
    pub scale: i32,
    pub width: u32,
    pub is_signed: bool,
}

impl Format {
    /// Computes the widths of the integer and fractional parts of the format,
    /// returning `None` if either part contains implicit zero bits.
    ///
    /// # Examples
    ///
    /// ```
    /// # use calyx_libm_utils::Format;
    /// #
    /// let format = Format { scale: -1, width: 4, is_signed: false };
    ///
    /// assert_eq!(format.parts(), Some((3, 1)));
    /// assert_eq!(Format { scale: 1, ..format }.parts(), None);
    /// assert_eq!(Format { scale: -5, ..format }.parts(), None);
    /// ```
    pub fn parts(&self) -> Option<(u32, u32)> {
        let frac_width = self.scale.unsigned_abs();
        let int_width = self.width.checked_sub(frac_width)?;

        (self.scale <= 0).then_some((int_width, frac_width))
    }

    pub fn msb(&self) -> i64 {
        i64::from(self.scale) + i64::from(self.width) - 1
    }

    pub fn lsb(&self) -> i64 {
        i64::from(self.scale)
    }

    /// Computes the positions of the most- and least-significant bits of the
    /// format, as per the VHDL fixed-point data types.
    ///
    /// # Examples
    ///
    /// ```
    /// # use calyx_libm_utils::Format;
    /// #
    /// let format = Format { scale: -1, width: 4, is_signed: false };
    ///
    /// assert_eq!(format.vhdl(), (2, -1));
    /// ```
    pub fn vhdl(&self) -> (i64, i64) {
        (self.msb(), self.lsb())
    }

    /// Returns an adapter for formatting `self` in ARM-style Q notation.
    ///
    /// Using the returned adapter will result in a panic if the format cannot
    /// be expressed in Q notation.
    pub fn q(&self) -> impl fmt::Display {
        struct QNotation<'a>(&'a Format);

        impl fmt::Display for QNotation<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let (int_width, frac_width) = self.0.parts().unwrap();

                if !self.0.is_signed {
                    write!(f, "U")?;
                }

                write!(f, "Q{}.{}", int_width, frac_width)
            }
        }

        QNotation(self)
    }
}

impl Default for Format {
    fn default() -> Self {
        Format {
            scale: 0,
            width: 32,
            is_signed: false,
        }
    }
}

impl FromStr for Format {
    type Err = ParseFormatError;

    /// Parses a fixed-point format in ARM-style Q notation.
    #[allow(clippy::from_str_radix_10)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.chars();
        let is_signed = iter.peeking_next(|&x| x == 'U').is_none();

        if !matches!(iter.next(), Some('Q')) {
            return Err(ParseFormatError);
        }

        let rest = iter.as_str();
        let split = rest.find('.').ok_or(ParseFormatError)?;

        let int_width = u16::from_str_radix(&rest[..split], 10)?;
        let frac_width = u16::from_str_radix(&rest[split + 1..], 10)?;

        Ok(Format {
            scale: -i32::from(frac_width),
            width: u32::from(int_width) + u32::from(frac_width),
            is_signed,
        })
    }
}

#[derive(Debug)]
pub struct ParseFormatError;

impl From<ParseIntError> for ParseFormatError {
    fn from(_: ParseIntError) -> Self {
        ParseFormatError
    }
}

impl fmt::Display for ParseFormatError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid format")
    }
}
