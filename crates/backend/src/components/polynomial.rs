//! Glue code for polynomials.

use std::cmp;

use calyx_ir::{self as ir, build_assignments};

use calyx_libm_approx::Datapath;
use calyx_libm_utils::diagnostics::Diagnostic;
use calyx_libm_utils::mangling::mangle;

use super::{ComponentBuilder, ComponentManager, Horner, Ids, LookupTable};
use crate::IrBuilder;

pub struct PiecewisePoly<'a> {
    pub table: LookupTable<'a>,
    pub spec: Datapath,
}

impl ComponentBuilder for PiecewisePoly<'_> {
    fn name(&self) -> ir::Id {
        ir::Id::new(mangle!(
            "poly",
            self.table.data.spec,
            self.table.data.formats,
            self.table.format,
            self.table.spec,
            self.spec,
        ))
    }

    fn signature(&self) -> Vec<ir::PortDef<u64>> {
        let mut stable = ir::Attributes::default();
        stable.insert(ir::Attribute::Bool(ir::BoolAttr::Stable), 1);

        vec![
            ir::PortDef::new(
                "in",
                u64::from(self.table.format.width),
                ir::Direction::Input,
                Default::default(),
            ),
            ir::PortDef::new(
                "out",
                u64::from(self.table.format.width),
                ir::Direction::Output,
                stable,
            ),
        ]
    }

    fn build(
        &self,
        name: ir::Id,
        cm: &mut ComponentManager,
    ) -> Result<ir::Component, Diagnostic> {
        let horner = Horner {
            format: self.table.format,
            spec: &self.spec,
            in_width: cmp::max(self.table.spec.idx_lsb, 1),
        };

        let (lookup, lookup_ports) = cm.get(&self.table)?;
        let (horner, horner_ports) = cm.get(&horner)?;

        let ports = self.signature();

        let mut component = ir::Component::new(name, ports, true, false, None);
        let mut builder = IrBuilder::new(&mut component, cm);

        let lookup = builder.add_component("lookup", lookup, lookup_ports);
        let horner = builder.add_component("horner", horner, horner_ports);

        let Ids { in_, out, .. } = builder.cm.ids;
        let signature = &builder.component.signature;

        let [lookup_in, component_out] = build_assignments!(builder;
            lookup[in_] = ? signature[in_];
            signature[out] = ? horner[out];
        );

        builder.add_continuous_assignment(component_out);

        let inputs = vec![
            (in_, lookup.borrow().get("arg")),
            (ir::Id::new("lut"), lookup.borrow().get(out)),
        ];

        let control =
            builder.invoke_with(horner, inputs, "index", vec![lookup_in]);

        *component.control.borrow_mut() = control;

        Ok(component)
    }
}
