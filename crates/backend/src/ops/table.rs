use std::slice;

use malachite::Rational;

use calyx_libm_approx::{AddressSpec, Datapath, TableDomain, faithful, remez};
use calyx_libm_utils::mangling::{Hash, Mangle};
use calyx_libm_utils::{Diagnostic, Format};

use super::{Component, OpBuilder, Operator, Prototype};
use crate::ComponentManager;
use crate::components::{LookupTable, PiecewisePoly};

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
        let f = op.sollya();

        let (addr, domain) =
            &widen_domain(op, self.left, self.right, self.format, self.size)?;

        let values =
            &remez::build_table(&f, 0, domain, self.size, self.format.scale)
                .map_err(|err| {
                    Diagnostic::from_sollya_and_span(
                        err,
                        &op.pretty().to_string(),
                        op.span,
                    )
                })?;

        let data = LutSpec {
            op: Hash::new(&f),
            domain,
            size: self.size,
        };

        let builder = LookupTable {
            values,
            columns: slice::from_ref(self.format),
            format: self.format,
            addr,
            data: &data,
        };

        let (name, signature) = cm.get(&builder).map_err(|err| {
            err.try_with_secondary(
                op.span,
                format!("while compiling operator `{}`", op.pretty()),
            )
        })?;

        Ok(Prototype::Comp(Component {
            name,
            prefix_hint: op.prefix_hint().into(),
            signature,
            is_comb: true,
        }))
    }
}

#[derive(Mangle)]
struct LutSpec<'a> {
    op: Hash,
    domain: &'a TableDomain,
    size: u32,
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
        let f = op.sollya();

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

        let spec = Datapath::from_approx(&approx, degree, scale, error);

        let data = CoefficientSpec {
            op: Hash::new(&f),
            degree,
            domain,
            size,
            scale: spec.lut_scale,
            error,
        };

        let builder = PiecewisePoly {
            table: LookupTable {
                values: &approx.table,
                columns: &spec.lut_formats(),
                format: self.format,
                addr,
                data: &data,
            },
            spec,
        };

        let (name, signature) = cm.get(&builder).map_err(|err| {
            err.try_with_secondary(
                op.span,
                format!("while compiling operator `{}`", op.pretty()),
            )
        })?;

        Ok(Prototype::Comp(Component {
            name,
            prefix_hint: op.prefix_hint().into(),
            signature,
            is_comb: false,
        }))
    }
}

#[derive(Mangle)]
struct CoefficientSpec<'a> {
    op: Hash,
    degree: u32,
    domain: &'a TableDomain,
    size: u32,
    scale: i32,
    error: &'a Rational,
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
