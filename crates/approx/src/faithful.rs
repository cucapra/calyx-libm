//! Faithfully-rounded polynomial approximations (de Dinechin, 2015).

use itertools::Itertools;
use malachite::Rational;

use calyx_libm_utils::rational::{Dyadic, RoundBinary};
use calyx_libm_utils::sollya::{self, ScriptError};

use crate::TableDomain;

/// Guesses the minimum number of subintervals needed to approximate `f` by
/// polynomials of the given degree and within the given error bound after
/// evaluation and final rounding.
///
/// Uniform segmentation is assumed. For each candidate division, the actual
/// domain is chosen as if by [`TableDomain::from_hint`].
pub fn segment_domain(
    f: &str,
    degree: u32,
    left: &Rational,
    right: &Rational,
    scale: i32,
    error: &Rational,
) -> Result<u32, ScriptError> {
    let a = left.floor(i64::from(scale));
    let b = right.ceil(i64::from(scale));

    let cmd = include_bytes!("scripts/segment.sollya");

    let args = [
        f,
        &format!("{degree}"),
        &format!("{a}"),
        &format!("{b}"),
        &format!("{}", TableDomain::center(a, b)),
        &format!("{scale}"),
        &format!("{error}"),
    ];

    let result = sollya::sollya(cmd, &args)?;

    result
        .trim_end()
        .parse()
        .map_err(|_| ScriptError::BadResponse)
}

pub struct PolynomialApprox {
    pub table: Vec<Vec<Rational>>,
    pub scale: i32,
    pub error: Rational,
}

/// Constructs a table of polynomials approximating `f` piecewise over the given
/// domain, selecting the least precision needed to meet the given error bound
/// after evaluation and final rounding.
///
/// Uniform segmentation is assumed, with `size` denoting the number of
/// subintervals. The actual error achieved is reported along with the table.
pub fn build_table(
    f: &str,
    degree: u32,
    domain: &TableDomain,
    size: u32,
    scale: i32,
    error: &Rational,
) -> Result<PolynomialApprox, ScriptError> {
    let cmd = include_bytes!("scripts/faithful.sollya");

    let args = [
        f,
        &format!("{degree}"),
        &format!("{}", domain.left.dyadic()),
        &format!("{}", domain.right.dyadic()),
        &format!("{size}"),
        &format!("{scale}"),
        &format!("{error}"),
    ];

    let result = sollya::sollya(cmd, &args)?;

    let (table, scale, error) =
        parse_response(&result).ok_or(ScriptError::BadResponse)?;

    let table = table
        .lines()
        .map(|line| {
            line.split(' ')
                .map(|c| {
                    Rational::from_dyadic(c).ok_or(ScriptError::BadResponse)
                })
                .collect()
        })
        .collect::<Result<_, _>>()?;

    Ok(PolynomialApprox {
        table,
        scale,
        error,
    })
}

fn parse_response(response: &str) -> Option<(&str, i32, Rational)> {
    let (scale, error, table) = response.splitn(3, '\n').collect_tuple()?;

    let scale = scale.parse().ok()?;
    let error = Rational::from_dyadic(error)?;

    Some((table, scale, error))
}
