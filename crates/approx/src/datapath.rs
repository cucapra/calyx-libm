//! Polynomial evaluator sizing.

use std::cmp::{self, Ordering};
use std::iter;

use malachite::Rational;
use malachite::num::arithmetic::traits::{FloorLogBase2, PowerOf2, Sign};
use malachite::num::basic::traits::{NegativeOne, One};
use malachite::num::conversion::traits::ExactFrom;

use calyx_libm_utils::interval::Interval;
use calyx_libm_utils::{Format, Mangle};

use crate::PolynomialApprox;

/// Architectural parameters for a polynomial evaluator.
#[derive(Mangle)]
pub struct Datapath {
    pub lut_widths: Vec<u32>,
    pub lut_scale: i32,
    pub product_width: u32,
    pub sum_width: u32,
    pub sum_scale: i32,
}

impl Datapath {
    pub fn from_approx(
        approx: &PolynomialApprox,
        degree: u32,
        scale: i32,
        error: &Rational,
    ) -> Datapath {
        let lut_widths = table_ranges(&approx.table, degree, approx.scale);
        let sum_scale = horner_precision(approx, degree, scale, error);

        let HornerRanges {
            product_width,
            sum_width,
        } = HornerRanges::from_table(&approx.table, sum_scale, scale);

        Datapath {
            lut_widths,
            lut_scale: approx.scale,
            product_width,
            sum_width,
            sum_scale,
        }
    }

    pub fn lut_formats(&self) -> Vec<Format> {
        self.lut_widths
            .iter()
            .map(|&width| Format {
                scale: self.lut_scale,
                width,
                is_signed: true,
            })
            .collect()
    }
}

// Cf. Algorithm 2 (de Dinechin, 2015).
pub fn horner_precision(
    approx: &PolynomialApprox,
    degree: u32,
    scale: i32,
    error: &Rational,
) -> i32 {
    if degree == 0 {
        approx.scale
    } else {
        let ulp = Rational::power_of_2(i64::from(scale));

        let error_target =
            cmp::max(error, &ulp) - &approx.error - (ulp >> 1u32);

        assert!(error_target > approx.error);

        let scale = (error_target / Rational::from(degree)).floor_log_base_2();
        let scale = i32::exact_from(scale);

        cmp::min(approx.scale, scale)
    }
}

pub fn table_ranges(
    table: &[Vec<Rational>],
    degree: u32,
    scale: i32,
) -> Vec<u32> {
    let mut widths = vec![1; usize::exact_from(degree) + 1];

    for row in table {
        for (value, max_width) in iter::zip(row, &mut widths) {
            *max_width = cmp::max(*max_width, signed_width(value, scale));
        }
    }

    widths
}

pub struct HornerRanges {
    pub product_width: u32,
    pub sum_width: u32,
}

impl HornerRanges {
    pub fn from_table(
        table: &[Vec<Rational>],
        sum_scale: i32,
        out_scale: i32,
    ) -> HornerRanges {
        let init = HornerRanges {
            product_width: 1,
            sum_width: 1,
        };

        table.iter().fold(init, |max, poly| {
            let HornerRanges {
                product_width,
                sum_width,
            } = HornerRanges::from_poly(poly, sum_scale, out_scale);

            HornerRanges {
                product_width: cmp::max(max.product_width, product_width),
                sum_width: cmp::max(max.sum_width, sum_width),
            }
        })
    }

    // See `computeHornerMSBs` (de Dinechin, 2015).
    fn from_poly(
        poly: &[Rational],
        sum_scale: i32,
        out_scale: i32,
    ) -> HornerRanges {
        let (last, rest) = poly.split_last().unwrap();

        let mut product_width = 0;
        let mut sum_width = signed_width(last, sum_scale);

        let mut sigma = Interval::from(last);

        for value in rest.iter().rev() {
            let pi =
                Interval::new(Rational::NEGATIVE_ONE, Rational::ONE) * sigma;

            product_width = product_width
                .max(signed_width(&pi.inf, sum_scale))
                .max(signed_width(&pi.sup, sum_scale));

            sigma = Interval::from(value) + pi;

            sum_width = sum_width
                .max(signed_width(&sigma.inf, sum_scale))
                .max(signed_width(&sigma.sup, sum_scale));
        }

        let sup = sigma.sup + Rational::power_of_2(i64::from(out_scale) - 1);

        HornerRanges {
            product_width,
            sum_width: cmp::max(sum_width, signed_width(&sup, sum_scale)),
        }
    }
}

fn signed_width(x: &Rational, scale: i32) -> u32 {
    if x.sign() == Ordering::Equal {
        return 0;
    }

    let msb = if x.sign() == Ordering::Less {
        x.ceiling_log_base_2_abs()
    } else {
        x.floor_log_base_2_abs() + 1
    };

    let msb = i32::exact_from(msb);

    if msb >= scale {
        msb.abs_diff(scale) + 1
    } else {
        0
    }
}
