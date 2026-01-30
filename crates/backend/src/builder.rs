//! IR builder based on [`calyx_ir::Builder`].

use calyx_ir as ir;
use malachite::Natural;

use super::components::{ComponentManager, Constant};

pub struct IrBuilder<'a> {
    pub component: &'a mut ir::Component,
    pub lib: &'a mut ir::LibrarySignatures,
}

impl<'a> IrBuilder<'a> {
    pub fn new(
        component: &'a mut ir::Component,
        lib: &'a mut ir::LibrarySignatures,
    ) -> IrBuilder<'a> {
        IrBuilder { component, lib }
    }

    pub fn add_constant(
        &mut self,
        value: u64,
        width: u64,
    ) -> ir::RRC<ir::Cell> {
        let name = ir::Cell::constant_name(value, width);

        if let Some(cell) = self.component.cells.find(name) {
            return cell;
        }

        let prototype = ir::CellType::Constant { val: value, width };

        let ports = [ir::PortDef::new(
            ir::Id::new("out"),
            width,
            ir::Direction::Output,
            Default::default(),
        )];

        let cell = ir::rrc(ir::Cell::new(name, prototype));
        add_ports_to_cell(&cell, ports);

        self.component.cells.add(cell.clone());

        cell
    }

    pub fn add_comb_group<S>(
        &mut self,
        prefix: S,
        assignments: Vec<ir::Assignment<ir::Nothing>>,
    ) -> ir::RRC<ir::CombGroup>
    where
        S: Into<ir::Id>,
    {
        fn inner(
            builder: &mut IrBuilder,
            prefix: ir::Id,
            assignments: Vec<ir::Assignment<ir::Nothing>>,
        ) -> ir::RRC<ir::CombGroup> {
            let group = builder.ir_builder().add_comb_group(prefix);
            group.borrow_mut().assignments = assignments;

            group
        }

        inner(self, prefix.into(), assignments)
    }

    pub fn add_primitive<S, T>(
        &mut self,
        prefix: S,
        primitive: T,
        parameters: &[u64],
    ) -> ir::RRC<ir::Cell>
    where
        S: Into<ir::Id>,
        T: Into<ir::Id>,
    {
        fn inner(
            builder: &mut IrBuilder,
            prefix: ir::Id,
            primitive: ir::Id,
            parameters: &[u64],
        ) -> ir::RRC<ir::Cell> {
            let definition = builder.lib.get_primitive(primitive);
            let (parameters, ports) = definition.resolve(parameters).unwrap();

            let name = builder.component.generate_name(prefix);

            let prototype = ir::CellType::Primitive {
                name: primitive,
                param_binding: Box::new(parameters),
                is_comb: definition.is_comb,
                latency: definition.latency,
            };

            let cell = ir::rrc(ir::Cell::new(name, prototype));
            add_ports_to_cell(&cell, ports);

            builder.component.cells.add(cell.clone());

            cell
        }

        inner(self, prefix.into(), primitive.into(), parameters)
    }

    pub fn add_component<S, T>(
        &mut self,
        prefix: S,
        component: T,
        ports: Vec<ir::PortDef<u64>>,
    ) -> ir::RRC<ir::Cell>
    where
        S: Into<ir::Id>,
        T: Into<ir::Id>,
    {
        fn inner(
            builder: &mut IrBuilder,
            prefix: ir::Id,
            component: ir::Id,
            ports: Vec<ir::PortDef<u64>>,
        ) -> ir::RRC<ir::Cell> {
            let name = builder.component.generate_name(prefix);
            let prototype = ir::CellType::Component { name: component };

            let cell = ir::rrc(ir::Cell::new(name, prototype));
            add_ports_to_cell(&cell, ports);

            builder.component.cells.add(cell.clone());

            cell
        }

        inner(self, prefix.into(), component.into(), ports)
    }

    #[inline]
    pub fn add_continuous_assignment(
        &mut self,
        assignment: ir::Assignment<ir::Nothing>,
    ) {
        self.component.continuous_assignments.push(assignment);
    }

    #[inline]
    pub fn add_continuous_assignments<I>(&mut self, assignments: I)
    where
        I: IntoIterator<Item = ir::Assignment<ir::Nothing>>,
    {
        self.component.continuous_assignments.extend(assignments);
    }

    /// For compatibility with [`calyx_ir::build_assignments`].
    pub fn build_assignment<T>(
        &self,
        dst: ir::RRC<ir::Port>,
        src: ir::RRC<ir::Port>,
        guard: ir::Guard<T>,
    ) -> ir::Assignment<T> {
        ir::Assignment {
            dst,
            src,
            guard: Box::new(guard),
            attributes: Default::default(),
        }
    }

    pub fn big_constant(
        &mut self,
        value: &Natural,
        width: u64,
        cm: &mut ComponentManager,
    ) -> ir::RRC<ir::Cell> {
        if let Ok(value) = u64::try_from(value) {
            self.add_constant(value, width)
        } else {
            let primitive = cm
                .get_primitive(&Constant { width, value }, self.lib)
                .ok()
                .unwrap();

            self.add_primitive("c", primitive, &[])
        }
    }

    pub fn invoke_with<S>(
        &mut self,
        component: ir::RRC<ir::Cell>,
        inputs: Vec<(ir::Id, ir::RRC<ir::Port>)>,
        prefix: S,
        assignments: Vec<ir::Assignment<ir::Nothing>>,
    ) -> ir::Control
    where
        S: Into<ir::Id>,
    {
        let comb_group = (!assignments.is_empty())
            .then(|| self.add_comb_group(prefix, assignments));

        ir::Control::Invoke(ir::Invoke {
            comp: component,
            inputs,
            outputs: Vec::new(),
            attributes: Default::default(),
            comb_group,
            ref_cells: Vec::new(),
        })
    }

    fn ir_builder(&mut self) -> ir::Builder<'_> {
        ir::Builder::new(self.component, self.lib).not_generated()
    }

    pub fn collapse<I, F>(statements: I, f: F) -> ir::Control
    where
        I: IntoIterator<Item = ir::Control>,
        F: FnOnce(Vec<ir::Control>) -> ir::Control,
    {
        let statements: Vec<_> = statements
            .into_iter()
            .filter(|control| !matches!(control, ir::Control::Empty(_)))
            .collect();

        match statements.len() {
            0 => ir::Control::empty(),
            1 => statements.into_iter().next().unwrap(),
            _ => f(statements),
        }
    }

    #[inline]
    pub fn clone_control(control: &ir::Control) -> ir::Control {
        clone_control(control)
    }
}

fn add_ports_to_cell<I>(cell: &ir::RRC<ir::Cell>, ports: I)
where
    I: IntoIterator<Item = ir::PortDef<u64>>,
{
    let wrc = ir::WRC::from(cell);
    let cell_ports = &mut cell.borrow_mut().ports;

    for port in ports {
        let port = ir::rrc(ir::Port {
            name: port.name(),
            width: port.width,
            direction: port.direction,
            parent: ir::PortParent::Cell(wrc.clone()),
            attributes: port.attributes,
        });

        cell_ports.push(port);
    }
}

fn clone_control(control: &ir::Control) -> ir::Control {
    match control {
        ir::Control::Seq(ir::Seq { stmts, attributes }) => {
            ir::Control::Seq(ir::Seq {
                stmts: stmts.iter().map(clone_control).collect(),
                attributes: attributes.clone(),
            })
        }
        ir::Control::Par(ir::Par { stmts, attributes }) => {
            ir::Control::Par(ir::Par {
                stmts: stmts.iter().map(clone_control).collect(),
                attributes: attributes.clone(),
            })
        }
        ir::Control::If(ir::If {
            port,
            cond,
            tbranch,
            fbranch,
            attributes,
        }) => ir::Control::If(ir::If {
            port: port.clone(),
            cond: cond.clone(),
            tbranch: Box::new(clone_control(tbranch)),
            fbranch: Box::new(clone_control(fbranch)),
            attributes: attributes.clone(),
        }),
        ir::Control::While(ir::While {
            port,
            cond,
            body,
            attributes,
        }) => ir::Control::While(ir::While {
            port: port.clone(),
            cond: cond.clone(),
            body: Box::new(clone_control(body)),
            attributes: attributes.clone(),
        }),
        ir::Control::Repeat(ir::Repeat {
            attributes,
            body,
            num_repeats,
        }) => ir::Control::Repeat(ir::Repeat {
            attributes: attributes.clone(),
            body: Box::new(clone_control(body)),
            num_repeats: *num_repeats,
        }),
        ir::Control::Invoke(ir::Invoke {
            comp,
            inputs,
            outputs,
            attributes,
            comb_group,
            ref_cells,
        }) => ir::Control::Invoke(ir::Invoke {
            comp: comp.clone(),
            inputs: inputs.clone(),
            outputs: outputs.clone(),
            attributes: attributes.clone(),
            comb_group: comb_group.clone(),
            ref_cells: ref_cells.clone(),
        }),
        ir::Control::Enable(ir::Enable { group, attributes }) => {
            ir::Control::Enable(ir::Enable {
                group: group.clone(),
                attributes: attributes.clone(),
            })
        }
        ir::Control::Empty(ir::Empty { attributes }) => {
            ir::Control::Empty(ir::Empty {
                attributes: attributes.clone(),
            })
        }
        ir::Control::Static(control) => {
            ir::Control::Static(clone_static_control(control))
        }
    }
}

fn clone_static_control(control: &ir::StaticControl) -> ir::StaticControl {
    match control {
        ir::StaticControl::Repeat(ir::StaticRepeat {
            attributes,
            body,
            num_repeats,
            latency,
        }) => ir::StaticControl::Repeat(ir::StaticRepeat {
            attributes: attributes.clone(),
            body: Box::new(clone_static_control(body)),
            num_repeats: *num_repeats,
            latency: *latency,
        }),
        ir::StaticControl::Enable(ir::StaticEnable { group, attributes }) => {
            ir::StaticControl::Enable(ir::StaticEnable {
                group: group.clone(),
                attributes: attributes.clone(),
            })
        }
        ir::StaticControl::Par(ir::StaticPar {
            stmts,
            attributes,
            latency,
        }) => ir::StaticControl::Par(ir::StaticPar {
            stmts: stmts.iter().map(clone_static_control).collect(),
            attributes: attributes.clone(),
            latency: *latency,
        }),
        ir::StaticControl::Seq(ir::StaticSeq {
            stmts,
            attributes,
            latency,
        }) => ir::StaticControl::Seq(ir::StaticSeq {
            stmts: stmts.iter().map(clone_static_control).collect(),
            attributes: attributes.clone(),
            latency: *latency,
        }),
        ir::StaticControl::If(ir::StaticIf {
            port,
            latency,
            tbranch,
            fbranch,
            attributes,
        }) => ir::StaticControl::If(ir::StaticIf {
            port: port.clone(),
            latency: *latency,
            tbranch: Box::new(clone_static_control(tbranch)),
            fbranch: Box::new(clone_static_control(fbranch)),
            attributes: attributes.clone(),
        }),
        ir::StaticControl::Empty(ir::Empty { attributes }) => {
            ir::StaticControl::Empty(ir::Empty {
                attributes: attributes.clone(),
            })
        }
        ir::StaticControl::Invoke(ir::StaticInvoke {
            comp,
            latency,
            inputs,
            outputs,
            attributes,
            ref_cells,
        }) => ir::StaticControl::Invoke(ir::StaticInvoke {
            comp: comp.clone(),
            latency: *latency,
            inputs: inputs.clone(),
            outputs: outputs.clone(),
            attributes: attributes.clone(),
            ref_cells: ref_cells.clone(),
        }),
    }
}
