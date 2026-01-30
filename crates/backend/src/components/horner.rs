//! Horner evaluation of polynomials.

use std::iter;

use calyx_ir::{self as ir, build_assignments, structure};
use itertools::{Itertools, Position};
use malachite::Natural;
use malachite::num::basic::traits::One;

use calyx_libm_approx::Datapath;
use calyx_libm_utils::mangling::mangle;
use calyx_libm_utils::{Diagnostic, Format};

use super::{Cast, ComponentBuilder, ComponentManager};
use crate::{Import, IrBuilder};

const INLINE: ir::Attribute = ir::Attribute::Bool(ir::BoolAttr::Inline);

type Signature = (ir::Id, Vec<ir::PortDef<u64>>);

pub struct Horner<'a> {
    pub format: &'a Format,
    pub spec: &'a Datapath,
    pub in_width: u64,
}

impl Horner<'_> {
    fn output_cast(
        &self,
        cm: &mut ComponentManager,
        lib: &mut ir::LibrarySignatures,
    ) -> Result<Signature, Diagnostic> {
        let cast = Cast {
            from: &Format {
                scale: self.spec.sum_scale,
                width: self.spec.sum_width,
                is_signed: true,
            },
            to: self.format,
        };

        cm.get(&cast, lib)
    }

    fn table_casts(
        &self,
        cm: &mut ComponentManager,
        lib: &mut ir::LibrarySignatures,
    ) -> Result<Vec<Signature>, Diagnostic> {
        let max_width = *self.spec.lut_widths.iter().max().unwrap();

        self.spec
            .lut_widths
            .iter()
            .with_position()
            .map(|(pos, &width)| {
                let to = if matches!(pos, Position::Last | Position::Only) {
                    Format {
                        scale: self.spec.sum_scale,
                        width: self.spec.sum_width,
                        is_signed: true,
                    }
                } else {
                    Format {
                        scale: self.spec.lut_scale,
                        width: max_width,
                        is_signed: true,
                    }
                };

                let cast = Cast {
                    from: &Format {
                        scale: self.spec.lut_scale,
                        width,
                        is_signed: true,
                    },
                    to: &to,
                };

                cm.get(&cast, lib)
            })
            .collect()
    }
}

impl ComponentBuilder for Horner<'_> {
    fn name(&self) -> ir::Id {
        ir::Id::new(mangle!("horner", self.format, self.spec, self.in_width))
    }

    fn signature(&self) -> Vec<ir::PortDef<u64>> {
        let mut stable = ir::Attributes::default();
        stable.insert(ir::Attribute::Bool(ir::BoolAttr::Stable), 1);

        let lut_width: u32 = self.spec.lut_widths.iter().sum();

        vec![
            ir::PortDef::new(
                "in",
                self.in_width,
                ir::Direction::Input,
                Default::default(),
            ),
            ir::PortDef::new(
                "lut",
                u64::from(lut_width),
                ir::Direction::Input,
                Default::default(),
            ),
            ir::PortDef::new(
                "out",
                u64::from(self.format.width),
                ir::Direction::Output,
                stable,
            ),
        ]
    }

    fn build(
        &self,
        name: ir::Id,
        cm: &mut ComponentManager,
        lib: &mut ir::LibrarySignatures,
    ) -> Result<ir::Component, Diagnostic> {
        let table_casts = self.table_casts(cm, lib)?;
        let output_cast = self.output_cast(cm, lib)?;

        let ports = self.signature();

        let mut component = ir::Component::new(name, ports, true, false, None);
        let mut builder = IrBuilder::new(&mut component, lib);

        cm.import(Import::Numbers);

        assert!(self.spec.sum_scale <= self.spec.lut_scale);

        structure!(builder;
            let acc = prim std_reg(u64::from(self.spec.sum_width));
            let mul = prim num_smul(
                self.in_width,
                u64::from(self.spec.sum_width),
                u64::from(self.spec.product_width),
                self.in_width - 1
            );
            let add = prim num_sadd(
                u64::from(*self.spec.lut_widths.iter().max().unwrap()),
                u64::from(self.spec.lut_scale.abs_diff(self.spec.sum_scale)),
                u64::from(self.spec.product_width),
                0u64,
                u64::from(self.spec.sum_width)
            );
            let round = prim std_add(u64::from(self.spec.sum_width));
        );

        let bias = builder.big_constant(
            &(Natural::ONE << (self.format.scale - self.spec.sum_scale - 1)),
            u64::from(self.spec.sum_width),
            cm,
        );

        let lut_width: u32 = self.spec.lut_widths.iter().sum();

        let [in_, out, left, right] =
            ["in", "out", "left", "right"].map(ir::Id::new);

        let table_casts: Vec<_> = iter::zip(&self.spec.lut_widths, table_casts)
            .rev()
            .scan(0, |lsb, (&width, (id, ports))| {
                structure!(builder;
                    let slice = prim std_bit_slice(
                        u64::from(lut_width),
                        u64::from(*lsb),
                        u64::from(*lsb + width - 1),
                        u64::from(width)
                    );
                );

                let cast = builder.add_component("cast", id, ports);
                cast.borrow_mut().add_attribute(INLINE, 1);

                let signature = &builder.component.signature;

                let assigns = build_assignments!(builder;
                    slice[in_] = ? signature["lut"];
                    cast[in_] = ? slice[out];
                );

                builder.add_continuous_assignments(assigns);

                *lsb += width;

                Some(cast)
            })
            .collect();

        let (leading, addends) = table_casts.split_first().unwrap();

        let init = ir::Control::invoke(
            acc.clone(),
            vec![(in_, leading.borrow().get(out))],
            vec![],
        );

        let products: Vec<_> = iter::repeat_with(|| {
            let signature = &builder.component.signature;

            ir::Control::invoke(
                mul.clone(),
                vec![
                    (left, signature.borrow().get(in_)),
                    (right, acc.borrow().get(out)),
                ],
                vec![],
            )
        })
        .take(addends.len())
        .collect();

        let sums: Vec<_> = addends
            .iter()
            .with_position()
            .map(|(pos, addend)| {
                let mut assigns = Vec::from(build_assignments!(builder;
                    add[left] = ? addend[out];
                    add[right] = ? mul[out];
                ));

                let cell = if matches!(pos, Position::Last | Position::Only) {
                    assigns.extend(build_assignments!(builder;
                        round[left] = ? add[out];
                        round[right] = ? bias[out];
                    ));

                    &round
                } else {
                    &add
                };

                builder.invoke_with(
                    acc.clone(),
                    vec![(in_, cell.borrow().get(out))],
                    "addend",
                    assigns,
                )
            })
            .collect();

        let (cast, ports) = output_cast;

        let cast = builder.add_component("cast", cast, ports);
        cast.borrow_mut().add_attribute(INLINE, 1);

        let signature = &builder.component.signature;

        let assigns = build_assignments!(builder;
            cast[in_] = ? acc[out];
            signature[out] = ? cast[out];
        );

        builder.add_continuous_assignments(assigns);

        *component.control.borrow_mut() = ir::Control::seq(
            iter::once(init)
                .chain(itertools::interleave(products, sums))
                .collect(),
        );

        Ok(component)
    }
}
