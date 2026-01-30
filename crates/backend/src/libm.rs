use std::collections::HashMap;
use std::slice;

use calyx_ir as ir;
use malachite::num::basic::traits::Zero;

use calyx_libm_approx::{AddressSpec, Datapath, TableDomain, faithful, remez};
use calyx_libm_hir::{self as hir, Metadata, Pool, Visitor};
use calyx_libm_hir_passes::analysis::RangeAnalysis;
use calyx_libm_utils::interface::{Config, RangeAnalysis as AnalysisMode};
use calyx_libm_utils::mangling::{Hash, Mangle};
use calyx_libm_utils::{Diagnostic, Format, Reporter};

use super::components::{self as comp, ComponentManager};

pub struct Prototype {
    pub name: ir::Id,
    pub prefix_hint: ir::Id,
    pub signature: Vec<ir::PortDef<u64>>,
    pub is_comb: bool,
}

pub fn compile_math_library(
    ctx: &hir::Context,
    cfg: &Config,
    reporter: &mut Reporter,
    cm: &mut ComponentManager,
    lib: &mut ir::LibrarySignatures,
) -> Option<HashMap<hir::ExprIdx, Prototype>> {
    let ranges = match cfg.range_analysis {
        AnalysisMode::Interval => Some(RangeAnalysis::new(ctx, cfg, reporter)?),
        AnalysisMode::None => None,
    };

    let mut builder = Builder {
        ctx,
        ranges: ranges.as_ref(),
        format: &cfg.format,
        reporter,
        cm,
        lib,
        prototypes: HashMap::new(),
    };

    builder.visit_definitions(ctx).ok()?;

    Some(builder.prototypes)
}

#[derive(Debug)]
pub struct LibraryError;

struct Builder<'a, 'src> {
    ctx: &'a hir::Context,
    ranges: Option<&'a RangeAnalysis>,
    format: &'a Format,

    reporter: &'a mut Reporter<'src>,
    cm: &'a mut ComponentManager,
    lib: &'a mut ir::LibrarySignatures,

    prototypes: HashMap<hir::ExprIdx, Prototype>,
}

impl Visitor for Builder<'_, '_> {
    type Error = LibraryError;

    fn visit_operation(
        &mut self,
        idx: hir::ExprIdx,
        op: &hir::Operation,
        args: hir::EntityList<hir::ExprIdx>,
        ctx: &hir::Context,
    ) -> Result<(), LibraryError> {
        if let hir::OpKind::Sollya(sollya) = op.kind {
            let op = Operator::new(op, sollya, ctx);

            let prototype =
                self.build(&ctx[idx], &op, args).map_err(|err| {
                    self.reporter.emit(&err);

                    LibraryError
                })?;

            self.prototypes.insert(idx, prototype);
        }

        hir::visitor::visit_operation(self, idx, op, args, ctx)
    }
}

impl<'a> Builder<'a, '_> {
    fn build(
        &mut self,
        expr: &hir::Expression,
        op: &Operator,
        args: hir::EntityList<hir::ExprIdx>,
    ) -> Result<Prototype, Diagnostic> {
        static ZERO: hir::Rational = hir::Rational::ZERO;

        let domain = expr.props(self.ctx).find_map(|prop| match prop {
            hir::Property::Domain(domain) => Some(domain),
            _ => None,
        });

        let strategy = expr.props(self.ctx).find_map(|prop| match prop {
            hir::Property::Impl(strategy) => Some(strategy),
            _ => None,
        });

        let domain = self.choose_domain(op, args, domain)?;

        match strategy {
            Some(&hir::Strategy::Lut { size }) => {
                self.build_lut(op, &domain, size)
            }
            Some(&hir::Strategy::Poly { degree, error }) => {
                let error =
                    error.map(|error| &self.ctx[error].value).unwrap_or(&ZERO);

                self.build_poly(op, &domain, degree, error)
            }
            None => Err(Diagnostic::error()
                .with_message("operator with unspecified implementation")
                .with_primary(op.span, "no implementation specified")
                .with_note("help: add a `:calyx-impl` annotation")),
        }
    }

    fn choose_domain(
        &self,
        op: &Operator,
        args: hir::EntityList<hir::ExprIdx>,
        hint: Option<&hir::Domain>,
    ) -> Result<DomainHint<'a>, Diagnostic> {
        let (left, right) = hint
            .map(|domain| {
                (&self.ctx[domain.left].value, &self.ctx[domain.right].value)
            })
            .or_else(|| {
                self.ranges.map(|ranges| {
                    let [left, right] =
                        &ranges[args.first(self.ctx.pool()).unwrap()];

                    (left, right)
                })
            })
            .ok_or_else(|| {
                Diagnostic::error()
                    .with_message("operator with unknown domain")
                    .with_primary(op.span, "unknown domain")
                    .with_note("help: add a `:calyx-domain` annotation or enable range analysis")
            })?;

        Ok(DomainHint { left, right })
    }

    fn build_lut(
        &mut self,
        op: &Operator,
        domain: &DomainHint,
        size: u32,
    ) -> Result<Prototype, Diagnostic> {
        let (addr_spec, domain) = &domain.widen(op, self.format, size)?;

        let values =
            &remez::build_table(&op.sollya, 0, domain, size, self.format.scale)
                .map_err(|err| {
                    Diagnostic::from_sollya_and_span(err, op.span)
                })?;

        let table_spec = LutSpec {
            op: op.mangle(),
            domain,
            size,
        };

        let data = comp::TableData {
            values,
            formats: slice::from_ref(self.format),
            spec: &table_spec,
        };

        let builder = comp::LookupTable {
            data,
            format: self.format,
            spec: addr_spec,
            span: op.span,
        };

        let (name, signature) = self.cm.get(&builder, self.lib)?;

        Ok(Prototype {
            name,
            prefix_hint: op.prefix_hint,
            signature,
            is_comb: true,
        })
    }

    fn build_poly(
        &mut self,
        op: &Operator,
        domain: &DomainHint,
        degree: u32,
        error: &hir::Rational,
    ) -> Result<Prototype, Diagnostic> {
        let f = op.sollya.as_str();
        let DomainHint { left, right } = domain;
        let scale = self.format.scale;

        let size =
            faithful::segment_domain(f, degree, left, right, scale, error)
                .map_err(|err| {
                    Diagnostic::from_sollya_and_span(err, op.span)
                })?;

        let (addr_spec, domain) = &domain.widen(op, self.format, size)?;

        let approx =
            faithful::build_table(f, degree, domain, size, scale, error)
                .map_err(|err| {
                    Diagnostic::from_sollya_and_span(err, op.span)
                })?;

        let datapath = Datapath::from_approx(&approx, degree, scale, error);

        let table_spec = CoefficientSpec {
            op: op.mangle(),
            degree,
            domain,
            size,
            scale: datapath.lut_scale,
            error,
        };

        let data = comp::TableData {
            values: &approx.table,
            formats: &datapath.lut_formats(),
            spec: &table_spec,
        };

        let builder = comp::PiecewisePoly {
            table: comp::LookupTable {
                data,
                format: self.format,
                spec: addr_spec,
                span: op.span,
            },
            spec: datapath,
        };

        let (name, signature) = self.cm.get(&builder, self.lib)?;

        Ok(Prototype {
            name,
            prefix_hint: op.prefix_hint,
            signature,
            is_comb: false,
        })
    }
}

#[derive(Mangle)]
struct LutSpec<'a> {
    op: Hash,
    domain: &'a TableDomain,
    size: u32,
}

#[derive(Mangle)]
struct CoefficientSpec<'a> {
    op: Hash,
    degree: u32,
    domain: &'a TableDomain,
    size: u32,
    scale: i32,
    error: &'a hir::Rational,
}

struct DomainHint<'a> {
    left: &'a hir::Rational,
    right: &'a hir::Rational,
}

impl DomainHint<'_> {
    fn widen(
        &self,
        op: &Operator,
        format: &Format,
        size: u32,
    ) -> Result<(AddressSpec, TableDomain), Diagnostic> {
        AddressSpec::from_domain_hint(self.left, self.right, format, size)
            .map_err(|err| {
                Diagnostic::error()
                    .with_message("operator with infeasible domain")
                    .with_primary(op.span, "operator has infeasible domain")
                    .with_note(err.to_string())
            })
    }
}

struct Operator {
    sollya: String,
    prefix_hint: ir::Id,
    span: hir::Span,
}

impl Operator {
    fn new(
        op: &hir::Operation,
        idx: hir::SollyaIdx,
        ctx: &hir::Context,
    ) -> Operator {
        Operator {
            sollya: idx.sollya(ctx).to_string(),
            prefix_hint: idx.name(ctx).unwrap_or("f").into(),
            span: op.span,
        }
    }

    fn mangle(&self) -> Hash {
        Hash::new(&self.sollya)
    }
}
