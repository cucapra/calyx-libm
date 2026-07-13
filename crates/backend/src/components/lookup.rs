//! Lookup tables.

use std::cmp;

use calyx_ir::{self as ir, build_assignments, structure};
use malachite::Natural;

use calyx_libm_approx::AddressSpec;
use calyx_libm_utils::Diagnostic;
use calyx_libm_utils::mangling::{Hash, mangle};

use super::{ComponentBuilder, ComponentManager, Ids, Rom};
use crate::{Import, IrBuilder};

/// A lookup table with uniform power-of-two domain splitting and argument
/// reduction.
///
/// # Interface
///
/// * `in` - The unreduced argument.
/// * `out` - The result of the ROM lookup.
/// * `arg` - The reduced argument normalized to the interval `[-1, 1)`.
pub struct LookupTable<'a> {
    pub in_width: u64,
    pub out_width: u64,
    pub addr: &'a AddressSpec,
    pub data: &'a [Natural],
}

impl LookupTable<'_> {
    fn build_rom(
        &self,
        cm: &mut ComponentManager,
    ) -> Result<ir::Id, Diagnostic> {
        let rom = Rom {
            idx_width: self.addr.idx_width,
            out_width: self.out_width,
            data: self.data,
        };

        cm.get_primitive(&rom)
    }
}

impl ComponentBuilder for LookupTable<'_> {
    fn name(&self) -> ir::Id {
        ir::Id::new(mangle!(
            "lookup",
            self.in_width,
            self.out_width,
            self.addr,
            Hash::new(self.data),
        ))
    }

    fn signature(&self) -> Vec<ir::PortDef<u64>> {
        vec![
            ir::PortDef::new(
                "in",
                self.in_width,
                ir::Direction::Input,
                Default::default(),
            ),
            ir::PortDef::new(
                "out",
                self.out_width,
                ir::Direction::Output,
                Default::default(),
            ),
            ir::PortDef::new(
                "arg",
                cmp::max(self.addr.idx_lsb, 1),
                ir::Direction::Output,
                Default::default(),
            ),
        ]
    }

    fn build(
        &self,
        name: ir::Id,
        cm: &mut ComponentManager,
    ) -> Result<ir::Component, Diagnostic> {
        let rom = self.build_rom(cm)?;
        let ports = self.signature();

        let mut component = ir::Component::new(name, ports, false, true, None);
        let mut builder = IrBuilder::new(&mut component, cm);

        let rom = builder.add_primitive("rom", rom, &[]);
        let start = builder.big_constant(&self.addr.subtrahend, self.in_width);

        builder.cm.import(Import::Core);

        structure!(builder;
            let sub = prim std_sub(self.in_width);
            let slice = prim std_bit_slice(
                self.in_width,
                self.addr.idx_lsb,
                self.addr.idx_lsb + self.addr.idx_width - 1,
                self.addr.idx_width
            );
        );

        let Ids {
            in_,
            out,
            left,
            right,
        } = builder.cm.ids;

        let signature = &builder.component.signature;

        let assigns = build_assignments!(builder;
            sub[left] = ? signature[in_];
            sub[right] = ? start[out];
            slice[in_] = ? sub[out];
            rom["idx"] = ? slice[out];
            signature[out] = ? rom[out];
        );

        builder.add_continuous_assignments(assigns);

        if self.addr.idx_lsb == 0 {
            let zero = builder.add_constant(0, 1);
            let signature = &builder.component.signature;

            let [assign] = build_assignments!(builder;
                signature["arg"] = ? zero[out];
            );

            builder.add_continuous_assignment(assign);
        } else {
            let msb = self.addr.idx_lsb - 1;

            structure!(builder;
                let high = prim std_bit_slice(self.in_width, msb, msb, 1);
                let com = prim std_not(1);
            );

            if self.addr.idx_lsb == 1 {
                let signature = &builder.component.signature;

                let assigns = build_assignments!(builder;
                    high[in_] = ? sub[out];
                    com[in_] = ? high[out];
                    signature["arg"] = ? com[out];
                );

                builder.add_continuous_assignments(assigns);
            } else {
                structure!(builder;
                    let low = prim std_slice(self.in_width, msb);
                    let cat = prim std_cat(1, msb, self.addr.idx_lsb);
                );

                let signature = &builder.component.signature;

                let assigns = build_assignments!(builder;
                    high[in_] = ? sub[out];
                    com[in_] = ? high[out];
                    low[in_] = ? sub[out];
                    cat[left] = ? com[out];
                    cat[right] = ? low[out];
                    signature["arg"] = ? cat[out];
                );

                builder.add_continuous_assignments(assigns);
            }
        }

        Ok(component)
    }
}
