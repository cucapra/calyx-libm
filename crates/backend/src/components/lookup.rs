//! Lookup tables.

use std::{cmp, iter};

use calyx_ir::{self as ir, build_assignments, structure};
use malachite::num::basic::traits::Zero;
use malachite::{Natural, Rational};

use calyx_libm_approx::AddressSpec;
use calyx_libm_utils::mangling::{Mangle, mangle};
use calyx_libm_utils::rational::FixedPoint;
use calyx_libm_utils::{Diagnostic, Format};

use super::{ComponentBuilder, ComponentManager, Rom};
use crate::IrBuilder;

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

pub struct TableData<'a> {
    pub values: &'a [Vec<Rational>],
    pub formats: &'a [Format],
    pub spec: &'a dyn Mangle,
}

impl TableData<'_> {
    pub fn widths(&self) -> impl Iterator<Item = u32> {
        self.formats.iter().map(|format| format.width)
    }

    pub fn width(&self) -> u32 {
        self.widths().sum()
    }
}

pub struct LookupTable<'a> {
    pub data: TableData<'a>,
    pub format: &'a Format,
    pub spec: &'a AddressSpec,
}

impl LookupTable<'_> {
    fn build_rom(
        &self,
        cm: &mut ComponentManager,
        lib: &mut ir::LibrarySignatures,
    ) -> Result<ir::Id, Diagnostic> {
        let diagnostic = |value| {
            Diagnostic::error()
                .with_message("implementation error")
                .with_note(format!(
                    "generated constant {value} overflows the target format"
                ))
        };

        let data: Vec<_> = self
            .data
            .values
            .iter()
            .map(|row| {
                itertools::process_results(
                    iter::zip(row, self.data.formats).map(|(value, format)| {
                        value
                            .to_fixed_point(format)
                            .ok_or_else(|| diagnostic(value))
                    }),
                    |bits| pack(bits, self.data.widths()),
                )
            })
            .collect::<Result<_, _>>()?;

        let rom = Rom {
            idx_width: self.spec.idx_width,
            out_width: u64::from(self.data.width()),
            data: &data,
        };

        cm.get_primitive(&rom, lib)
    }
}

impl ComponentBuilder for LookupTable<'_> {
    fn name(&self) -> ir::Id {
        ir::Id::new(mangle!(
            "lookup",
            self.data.spec,
            self.data.formats,
            self.format,
            self.spec,
        ))
    }

    fn signature(&self) -> Vec<ir::PortDef<u64>> {
        vec![
            ir::PortDef::new(
                "in",
                u64::from(self.format.width),
                ir::Direction::Input,
                Default::default(),
            ),
            ir::PortDef::new(
                "out",
                u64::from(self.data.width()),
                ir::Direction::Output,
                Default::default(),
            ),
            ir::PortDef::new(
                "arg",
                cmp::max(self.spec.idx_lsb, 1),
                ir::Direction::Output,
                Default::default(),
            ),
        ]
    }

    fn build(
        &self,
        name: ir::Id,
        cm: &mut ComponentManager,
        lib: &mut ir::LibrarySignatures,
    ) -> Result<ir::Component, Diagnostic> {
        let rom = self.build_rom(cm, lib)?;
        let ports = self.signature();

        let mut component = ir::Component::new(name, ports, false, true, None);
        let mut builder = IrBuilder::new(&mut component, lib);

        let global = u64::from(self.format.width);

        let rom = builder.add_primitive("rom", rom, &[]);
        let left = builder.big_constant(&self.spec.subtrahend, global, cm);

        structure!(builder;
            let sub = prim std_sub(global);
            let slice = prim std_bit_slice(
                global,
                self.spec.idx_lsb,
                self.spec.idx_lsb + self.spec.idx_width - 1,
                self.spec.idx_width
            );
        );

        let [in_, out] = ["in", "out"].map(ir::Id::new);
        let signature = &builder.component.signature;

        let assigns = build_assignments!(builder;
            sub["left"] = ? signature[in_];
            sub["right"] = ? left[out];
            slice[in_] = ? sub[out];
            rom["idx"] = ? slice[out];
            signature[out] = ? rom[out];
        );

        builder.add_continuous_assignments(assigns);

        if self.spec.idx_lsb == 0 {
            let zero = builder.add_constant(0, 1);
            let signature = &builder.component.signature;

            let [assign] = build_assignments!(builder;
                signature["arg"] = ? zero[out];
            );

            builder.add_continuous_assignment(assign);
        } else {
            let msb = self.spec.idx_lsb - 1;

            structure!(builder;
                let high = prim std_bit_slice(global, msb, msb, 1);
                let com = prim std_not(1);
            );

            if self.spec.idx_lsb == 1 {
                let signature = &builder.component.signature;

                let assigns = build_assignments!(builder;
                    high[in_] = ? sub[out];
                    com[in_] = ? high[out];
                    signature["arg"] = ? com[out];
                );

                builder.add_continuous_assignments(assigns);
            } else {
                structure!(builder;
                    let low = prim std_slice(global, msb);
                    let cat = prim std_cat(1, msb, self.spec.idx_lsb);
                );

                let signature = &builder.component.signature;

                let assigns = build_assignments!(builder;
                    high[in_] = ? sub[out];
                    com[in_] = ? high[out];
                    low[in_] = ? sub[out];
                    cat["left"] = ? com[out];
                    cat["right"] = ? low[out];
                    signature["arg"] = ? cat[out];
                );

                builder.add_continuous_assignments(assigns);
            }
        }

        Ok(component)
    }
}
