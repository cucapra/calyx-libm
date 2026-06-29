use std::iter;

use malachite::num::basic::traits::Zero;
use malachite::{Natural, Rational};

use calyx_libm_approx::{AddressSpec, Datapath, TableDomain, faithful, remez};
use calyx_libm_utils::rational::FixedPoint;
use calyx_libm_utils::{Diagnostic, Format};

use super::{Component, OpBuilder, Operator, Prototype};
use crate::ComponentManager;
use crate::components::{LookupTable, PiecewisePoly};

/// Packs a sequence of values into a single bit vector. The first element of
/// the sequence occupies the most-significant position.
fn pack<V, W>(values: V, widths: W) -> Natural
where
    V: IntoIterator<Item = Natural>,
    W: IntoIterator<Item = u32>,
{
    iter::zip(values, widths)
        .fold(Natural::ZERO, |acc, (value, width)| (acc << width) | value)
}

pub struct Lut<'a> {
    pub format: &'a Format,
    pub left: &'a Rational,
    pub right: &'a Rational,
    pub size: u32,
}

impl OpBuilder for Lut<'_> {
    fn build(
        &self,
        op: &Operator,
        cm: &mut ComponentManager,
    ) -> Result<Prototype, Diagnostic> {
        let f = op.sollya().to_string();

        let (addr, domain) =
            &widen_domain(op, self.left, self.right, self.format, self.size)?;

        let values =
            remez::build_table(&f, domain, self.size, self.format.scale)
                .map_err(|err| {
                    Diagnostic::from_sollya_and_span(
                        err,
                        &op.pretty().to_string(),
                        op.span,
                    )
                })?;

        let data: Vec<_> = values
            .iter()
            .map(|value| {
                value.to_fixed_point(self.format).ok_or_else(|| {
                    Diagnostic::error()
                        .with_message("overflow")
                        .try_with_secondary(
                            op.span,
                            format!(
                                "while compiling operator `{}`",
                                op.pretty(),
                            ),
                        )
                        .with_note(format!(
                            "table value {} out of range for `{}`",
                            value,
                            self.format.fpcore(),
                        ))
                })
            })
            .collect::<Result<_, _>>()?;

        let builder = LookupTable {
            in_width: u64::from(self.format.width),
            out_width: u64::from(self.format.width),
            addr,
            data: &data,
        };

        let (name, signature) = cm.get(&builder)?;

        Ok(Prototype::Comp(Component {
            name,
            prefix_hint: op.prefix_hint().into(),
            signature,
            is_comb: true,
        }))
    }
}

pub struct Poly<'a> {
    pub format: &'a Format,
    pub left: &'a Rational,
    pub right: &'a Rational,
    pub degree: u32,
    pub error: &'a Rational,
}

impl OpBuilder for Poly<'_> {
    fn build(
        &self,
        op: &Operator,
        cm: &mut ComponentManager,
    ) -> Result<Prototype, Diagnostic> {
        let f = op.sollya().to_string();

        let Poly {
            format: &Format { scale, .. },
            left,
            right,
            degree,
            error,
        } = *self;

        let size =
            faithful::segment_domain(&f, degree, left, right, scale, error)
                .map_err(|err| {
                    Diagnostic::from_sollya_and_span(
                        err,
                        &op.pretty().to_string(),
                        op.span,
                    )
                })?;

        let (addr, domain) = &widen_domain(op, left, right, self.format, size)?;

        let approx =
            faithful::build_table(&f, degree, domain, size, scale, error)
                .map_err(|err| {
                    Diagnostic::from_sollya_and_span(
                        err,
                        &op.pretty().to_string(),
                        op.span,
                    )
                })?;

        let eval = Datapath::from_approx(&approx, degree, scale, error);

        let data: Vec<_> = approx
            .table
            .iter()
            .map(|row| {
                pack(
                    iter::zip(row, eval.lut_formats()).map(
                        |(value, format)| {
                            value.to_fixed_point(&format).unwrap()
                        },
                    ),
                    eval.lut_widths.iter().copied(),
                )
            })
            .collect();

        let lut_width: u32 = eval.lut_widths.iter().sum();

        let builder = PiecewisePoly {
            format: self.format,
            eval: &eval,
            table: LookupTable {
                in_width: u64::from(self.format.width),
                out_width: u64::from(lut_width),
                addr,
                data: &data,
            },
        };

        let (name, signature) = cm.get(&builder)?;

        Ok(Prototype::Comp(Component {
            name,
            prefix_hint: op.prefix_hint().into(),
            signature,
            is_comb: false,
        }))
    }
}

fn widen_domain(
    op: &Operator,
    left: &Rational,
    right: &Rational,
    format: &Format,
    size: u32,
) -> Result<(AddressSpec, TableDomain), Diagnostic> {
    AddressSpec::from_domain_hint(left, right, format, size).map_err(|err| {
        Diagnostic::error()
            .with_message(format!(
                "operator `{}` has infeasible domain",
                op.pretty(),
            ))
            .try_with_primary(op.span, "operator has infeasible domain")
            .with_note(err.to_string())
    })
}
