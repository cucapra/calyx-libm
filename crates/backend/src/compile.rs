use std::collections::{HashMap, HashSet};
use std::{iter, mem};

use calyx_ir as ir;
use itertools::Itertools;

use calyx_libm_hir as hir;
use calyx_libm_utils::rational::{FixedPoint, RoundBinary};
use calyx_libm_utils::{Config, Diagnostic, Format, Reporter};

use crate::libm::{Prototype, compile_math_library};
use crate::stdlib::Primitive;
use crate::{ComponentManager, Ids, IrBuilder, Program};

struct CompiledExpr {
    control: ir::Control,
    assignments: Vec<ir::Assignment<ir::Nothing>>,
    /// Valid after executing the control program, as long as the assignments
    /// are active.
    out: ir::RRC<ir::Port>,
}

impl CompiledExpr {
    fn from_port(port: ir::RRC<ir::Port>) -> CompiledExpr {
        CompiledExpr {
            control: ir::Control::empty(),
            assignments: Vec::new(),
            out: port,
        }
    }
}

struct Builder<'a, 'src> {
    hir: &'a hir::Context,
    signatures: &'a HashMap<hir::DefIdx, Prototype>,
    libm: &'a HashMap<hir::ExprIdx, Prototype>,
    format: &'a Format,

    reporter: &'a mut Reporter<'src>,
    builder: IrBuilder<'a>,

    stores: HashMap<hir::VarIdx, ir::RRC<ir::Cell>>,
}

impl Builder<'_, '_> {
    fn compile_number(&mut self, number: &hir::Number) -> Option<CompiledExpr> {
        let rounded = (&number.value).round_convergent(self.format.lsb());

        let Some(value) = rounded.to_fixed_point(self.format) else {
            self.reporter.emit(
                &Diagnostic::error()
                    .with_message("overflow")
                    .try_with_primary(
                        number.span,
                        format!(
                            "value out of range for `{}`",
                            self.format.fpcore(),
                        ),
                    ),
            );

            return None;
        };

        let cell = self.builder.big_constant(&value, self.format.width.into());
        let port = cell.borrow().get(self.builder.cm.ids.out);

        Some(CompiledExpr::from_port(port))
    }

    fn compile_boolean(&mut self, value: bool) -> Option<CompiledExpr> {
        let cell = self.builder.std_const(value.into(), 1);
        let port = cell.borrow().get(self.builder.cm.ids.out);

        Some(CompiledExpr::from_port(port))
    }

    fn compile_constant(
        &mut self,
        constant: hir::Constant,
        span: hir::Span,
    ) -> Option<CompiledExpr> {
        match constant {
            hir::Constant::Math(_) => {
                self.reporter.emit(
                    &Diagnostic::error()
                        .with_message("unsupported numeric constant")
                        .try_with_primary(span, "unsupported constant"),
                );

                None
            }
            hir::Constant::Bool(value) => self.compile_boolean(value),
        }
    }

    fn compile_variadic_operation(
        &mut self,
        op: hir::TestOp,
        args: hir::EntityList<hir::ExprIdx>,
    ) -> Option<CompiledExpr> {
        let mut assignments = Vec::new();

        let (control, args): (Vec<_>, Vec<_>) = self.hir[args]
            .iter()
            .map(|&arg| {
                let expr = self.compile_expression(arg)?;

                assignments.extend(expr.assignments);

                Some((expr.control, expr.out))
            })
            .collect::<Option<_>>()?;

        let control = IrBuilder::par(control);

        let Ids {
            out, left, right, ..
        } = self.builder.cm.ids;

        let mut reduce =
            |lhs, rhs, decl: &Primitive, builder: &mut IrBuilder| {
                let cell = builder.add_primitive(
                    decl.prefix_hint,
                    decl.name,
                    &decl.build_params(self.format),
                );

                let cell = cell.borrow();

                assignments.push(ir::Assignment::new(cell.get(left), lhs));
                assignments.push(ir::Assignment::new(cell.get(right), rhs));

                cell.get(out)
            };

        let args = match op {
            hir::TestOp::Lt
            | hir::TestOp::Gt
            | hir::TestOp::Leq
            | hir::TestOp::Geq
            | hir::TestOp::Eq => {
                let importer = &mut self.builder.cm.importer;

                let decl = match op {
                    hir::TestOp::Lt => importer.lt(self.format),
                    hir::TestOp::Gt => importer.gt(self.format),
                    hir::TestOp::Leq => importer.le(self.format),
                    hir::TestOp::Geq => importer.ge(self.format),
                    hir::TestOp::Eq => importer.eq(self.format),
                    _ => unreachable!(),
                };

                args.into_iter()
                    .tuple_windows()
                    .map(|(lhs, rhs)| reduce(lhs, rhs, decl, &mut self.builder))
                    .collect()
            }
            hir::TestOp::Neq => {
                let decl = self.builder.cm.importer.neq(self.format);

                args.into_iter()
                    .tuple_combinations()
                    .map(|(lhs, rhs)| reduce(lhs, rhs, decl, &mut self.builder))
                    .collect()
            }
            hir::TestOp::And | hir::TestOp::Or => args,
            _ => unreachable!(),
        };

        let decl = if op == hir::TestOp::Or {
            self.builder.cm.importer.or()
        } else {
            self.builder.cm.importer.and()
        };

        let out = args
            .into_iter()
            .tree_fold1(|lhs, rhs| reduce(lhs, rhs, decl, &mut self.builder))
            .unwrap();

        Some(CompiledExpr {
            control,
            assignments,
            out,
        })
    }

    fn compile_instantiated_operation(
        &mut self,
        cell: ir::RRC<ir::Cell>,
        input_ports: &[ir::Id],
        output_port: ir::Id,
        is_comb: bool,
        args: hir::EntityList<hir::ExprIdx>,
    ) -> Option<CompiledExpr> {
        let mut assignments = Vec::new();

        let (control, args): (Vec<_>, Vec<_>) = self.hir[args]
            .iter()
            .map(|&arg| {
                let expr = self.compile_expression(arg)?;

                assignments.extend(expr.assignments);

                Some((expr.control, expr.out))
            })
            .collect::<Option<_>>()?;

        let control = IrBuilder::par(control);
        let out = cell.borrow().get(output_port);

        let control = if is_comb {
            assignments.extend(iter::zip(input_ports, args).map(
                |(dst, src)| ir::Assignment::new(cell.borrow().get(dst), src),
            ));

            control
        } else {
            let inputs = input_ports.iter().copied().zip(args).collect();

            let invoke = self.builder.invoke_with(
                cell,
                inputs,
                "args",
                mem::take(&mut assignments),
            );

            IrBuilder::seq([control, invoke])
        };

        Some(CompiledExpr {
            control,
            assignments,
            out,
        })
    }

    fn compile_primitive_operation(
        &mut self,
        primitive: &Primitive,
        args: hir::EntityList<hir::ExprIdx>,
    ) -> Option<CompiledExpr> {
        let cell = self.builder.add_primitive(
            primitive.prefix_hint,
            primitive.name,
            &primitive.build_params(self.format),
        );

        let inputs: Vec<_> =
            primitive.signature.args.iter().map(ir::Id::new).collect();

        self.compile_instantiated_operation(
            cell,
            &inputs,
            ir::Id::new(primitive.signature.output),
            primitive.is_comb,
            args,
        )
    }

    fn compile_component_operation(
        &mut self,
        prototype: &Prototype,
        args: hir::EntityList<hir::ExprIdx>,
    ) -> Option<CompiledExpr> {
        let cell = self.builder.add_component(
            prototype.prefix_hint,
            prototype.name,
            prototype.signature.clone(),
        );

        let inputs: Vec<_> = prototype
            .signature
            .iter()
            .filter_map(|port| {
                (port.direction == ir::Direction::Input).then_some(port.name())
            })
            .collect();

        self.compile_instantiated_operation(
            cell,
            &inputs,
            self.builder.cm.ids.out,
            prototype.is_comb,
            args,
        )
    }

    fn compile_operation(
        &mut self,
        idx: hir::ExprIdx,
        op: &hir::Operation,
        args: hir::EntityList<hir::ExprIdx>,
    ) -> Option<CompiledExpr> {
        let mut unsupported = || {
            self.reporter.emit(
                &Diagnostic::error()
                    .with_message("unsupported operation")
                    .try_with_primary(op.span, "unsupported operator"),
            )
        };

        match op.kind {
            hir::OpKind::Arith(op) => {
                let importer = &mut self.builder.cm.importer;

                let decl = match op {
                    hir::ArithOp::Add => importer.add(self.format),
                    hir::ArithOp::Sub => importer.sub(self.format),
                    hir::ArithOp::Mul => importer.mul(self.format),
                    hir::ArithOp::Div => importer.div(self.format),
                    hir::ArithOp::Neg => importer.neg(self.format),
                    hir::ArithOp::Sqrt => importer.sqrt(self.format),
                    hir::ArithOp::Abs => importer.abs(self.format),
                    hir::ArithOp::Max => importer.max(self.format),
                    hir::ArithOp::Min => importer.min(self.format),
                    hir::ArithOp::Pow => {
                        unsupported();

                        return None;
                    }
                };

                self.compile_primitive_operation(decl, args)
            }
            hir::OpKind::Test(op) => {
                if op == hir::TestOp::Not {
                    let decl = self.builder.cm.importer.not();

                    self.compile_primitive_operation(decl, args)
                } else if op.is_variadic() {
                    self.compile_variadic_operation(op, args)
                } else {
                    unsupported();

                    None
                }
            }
            hir::OpKind::Sollya(_) => {
                self.compile_component_operation(&self.libm[&idx], args)
            }
            hir::OpKind::Def(def) => {
                self.compile_component_operation(&self.signatures[&def], args)
            }
        }
    }

    fn compile_if(&mut self, expr: &hir::If) -> Option<CompiledExpr> {
        let cond = self.compile_expression(expr.cond)?;
        let true_branch = self.compile_expression(expr.if_true)?;
        let false_branch = self.compile_expression(expr.if_false)?;

        let width = true_branch.out.borrow().width;

        match (&true_branch.control, &false_branch.control) {
            (ir::Control::Empty(_), ir::Control::Empty(_)) => {
                let mux = self.builder.std_mux(width);
                let out = mux.borrow().get(self.builder.cm.ids.out);

                let inputs = [
                    ("cond", cond.out),
                    ("tru", true_branch.out),
                    ("fal", false_branch.out),
                ];

                let mut assignments: Vec<_> = inputs
                    .into_iter()
                    .map(|(dst, src)| {
                        ir::Assignment::new(mux.borrow().get(dst), src)
                    })
                    .collect();

                assignments.extend(cond.assignments);
                assignments.extend(true_branch.assignments);
                assignments.extend(false_branch.assignments);

                Some(CompiledExpr {
                    control: cond.control,
                    assignments,
                    out,
                })
            }
            _ => {
                let reg = self.builder.std_reg(width);
                let out = reg.borrow().get(self.builder.cm.ids.out);

                let store_true = self.builder.invoke_with(
                    reg.clone(),
                    vec![(self.builder.cm.ids.in_, true_branch.out)],
                    "branch",
                    true_branch.assignments,
                );

                let store_false = self.builder.invoke_with(
                    reg,
                    vec![(self.builder.cm.ids.in_, false_branch.out)],
                    "branch",
                    false_branch.assignments,
                );

                let group =
                    self.builder.add_comb_group("cond", cond.assignments);

                let conditional = ir::Control::if_(
                    cond.out,
                    Some(group),
                    Box::new(IrBuilder::seq([true_branch.control, store_true])),
                    Box::new(IrBuilder::seq([
                        false_branch.control,
                        store_false,
                    ])),
                );

                let control = IrBuilder::seq([cond.control, conditional]);

                Some(CompiledExpr {
                    control,
                    assignments: Vec::new(),
                    out,
                })
            }
        }
    }

    fn compile_let(&mut self, expr: &hir::Let) -> Option<CompiledExpr> {
        let (args, stores): (Vec<_>, Vec<_>) = self.hir[expr.writes]
            .iter()
            .map(|&write| {
                let write = &self.hir[write];
                let expr = self.compile_expression(write.val)?;

                let reg = self.builder.std_reg(expr.out.borrow().width);

                let invoke = self.builder.invoke_with(
                    reg.clone(),
                    vec![(self.builder.cm.ids.in_, expr.out)],
                    "expr",
                    expr.assignments,
                );

                self.stores.insert(write.var, reg);

                Some((expr.control, invoke))
            })
            .collect::<Option<_>>()?;

        let body = self.compile_expression(expr.body)?;

        let control = if expr.sequential {
            IrBuilder::seq(
                itertools::interleave(args, stores)
                    .chain(iter::once(body.control)),
            )
        } else {
            IrBuilder::seq([
                IrBuilder::par(args),
                IrBuilder::par(stores),
                body.control,
            ])
        };

        Some(CompiledExpr { control, ..body })
    }

    fn compile_while(&mut self, expr: &hir::While) -> Option<CompiledExpr> {
        let (inits, init_stores): (Vec<_>, Vec<_>) = self.hir[expr.inits]
            .iter()
            .map(|&write| {
                let write = &self.hir[write];
                let init = self.compile_expression(write.val)?;

                let reg = self.builder.std_reg(init.out.borrow().width);

                let invoke = self.builder.invoke_with(
                    reg.clone(),
                    vec![(self.builder.cm.ids.in_, init.out)],
                    "init",
                    init.assignments,
                );

                self.stores.insert(write.var, reg);

                Some((init.control, invoke))
            })
            .collect::<Option<_>>()?;

        let cond = self.compile_expression(expr.cond)?;
        let body = self.compile_expression(expr.body)?;

        let (updates, update_stores): (Vec<_>, Vec<_>) = self.hir[expr.updates]
            .iter()
            .map(|&write| {
                let write = &self.hir[write];
                let update = self.compile_expression(write.val)?;
                let reg = self.stores[&write.var].clone();

                let invoke = self.builder.invoke_with(
                    reg,
                    vec![(self.builder.cm.ids.in_, update.out)],
                    "update",
                    update.assignments,
                );

                Some((update.control, invoke))
            })
            .collect::<Option<_>>()?;

        let group = self.builder.add_comb_group("cond", cond.assignments);

        let control = if expr.sequential {
            IrBuilder::seq(
                itertools::interleave(inits, init_stores).chain([
                    IrBuilder::clone_control(&cond.control),
                    ir::Control::while_(
                        cond.out,
                        Some(group),
                        Box::new(IrBuilder::seq(
                            itertools::interleave(updates, update_stores)
                                .chain(iter::once(cond.control)),
                        )),
                    ),
                    body.control,
                ]),
            )
        } else {
            IrBuilder::seq([
                IrBuilder::par(inits),
                IrBuilder::par(init_stores),
                IrBuilder::clone_control(&cond.control),
                ir::Control::while_(
                    cond.out,
                    Some(group),
                    Box::new(IrBuilder::seq([
                        IrBuilder::par(updates),
                        IrBuilder::par(update_stores),
                        cond.control,
                    ])),
                ),
                body.control,
            ])
        };

        Some(CompiledExpr { control, ..body })
    }

    fn compile_expression(
        &mut self,
        idx: hir::ExprIdx,
    ) -> Option<CompiledExpr> {
        let expr = &self.hir[idx];

        match &expr.kind {
            hir::ExprKind::Num(idx) => self.compile_number(&self.hir[*idx]),
            hir::ExprKind::Const(constant) => {
                self.compile_constant(*constant, expr.span)
            }
            hir::ExprKind::Var(_, hir::VarKind::Arg(arg)) => {
                let id = self.hir[*arg].name.id;
                let port = self.builder.component.signature.borrow().get(id);

                Some(CompiledExpr::from_port(port))
            }
            hir::ExprKind::Var(var, _) => {
                let port =
                    self.stores[var].borrow().get(self.builder.cm.ids.out);

                Some(CompiledExpr::from_port(port))
            }
            hir::ExprKind::Op(op, args) => {
                self.compile_operation(idx, op, *args)
            }
            hir::ExprKind::If(expr) => self.compile_if(expr),
            hir::ExprKind::Let(expr) => self.compile_let(expr),
            hir::ExprKind::While(expr) => self.compile_while(expr),
        }
    }
}

struct NameGenerator {
    generated: usize,
    used: HashSet<hir::Id>,
}

impl NameGenerator {
    fn new(used: HashSet<hir::Id>) -> NameGenerator {
        NameGenerator { generated: 0, used }
    }

    fn next(&mut self) -> ir::Id {
        loop {
            let id = if self.generated == 0 {
                hir::Id::new("main")
            } else {
                hir::Id::new(format!("main{}", self.generated - 1))
            };

            self.generated += 1;

            if !self.used.contains(&id) {
                break ir::Id { id };
            }
        }
    }
}

struct CompileContext<'a, 'src> {
    hir: &'a hir::Context,
    libm: &'a HashMap<hir::ExprIdx, Prototype>,
    format: &'a Format,

    cm: &'a mut ComponentManager,
    reporter: &'a mut Reporter<'src>,

    names: NameGenerator,
    signatures: HashMap<hir::DefIdx, Prototype>,
}

fn compile_definition(
    idx: hir::DefIdx,
    def: &hir::Definition,
    ctx: &mut CompileContext,
) -> Option<ir::Component> {
    let name = def
        .name
        .as_ref()
        .map_or_else(|| ctx.names.next(), |sym| ir::Id { id: sym.id });

    let ports: Vec<_> = def
        .args
        .into_iter()
        .map(|arg| {
            ir::PortDef::new(
                ir::Id {
                    id: ctx.hir[arg].name.id,
                },
                u64::from(ctx.format.width),
                ir::Direction::Input,
                Default::default(),
            )
        })
        .chain(iter::once(ir::PortDef::new(
            ctx.cm.ids.out,
            u64::from(ctx.format.width),
            ir::Direction::Output,
            Default::default(),
        )))
        .collect();

    let signature = def.name.as_ref().map(|sym| (sym.id, ports.clone()));

    let mut component = ir::Component::new(name, ports, false, false, None);

    let mut builder = Builder {
        hir: ctx.hir,
        signatures: &ctx.signatures,
        libm: ctx.libm,
        format: ctx.format,
        reporter: ctx.reporter,
        builder: IrBuilder::new(&mut component, ctx.cm),
        stores: HashMap::new(),
    };

    let body = builder.compile_expression(def.body)?;

    let assign = ir::Assignment::new(
        component.signature.borrow().get(ctx.cm.ids.out),
        body.out,
    );

    component.continuous_assignments.extend(body.assignments);
    component.continuous_assignments.push(assign);

    component.is_comb = matches!(body.control, ir::Control::Empty(_));

    *component.control.borrow_mut() = body.control;

    if let Some((id, signature)) = signature {
        let prototype = Prototype {
            name: component.name,
            prefix_hint: ir::Id { id },
            signature,
            is_comb: component.is_comb,
        };

        ctx.signatures.insert(idx, prototype);
    }

    Some(component)
}

pub fn compile_hir(
    hir: &hir::Context,
    cfg: &Config,
    reporter: &mut Reporter,
) -> Option<Program> {
    let mut cm = ComponentManager::new();

    let libm = compile_math_library(hir, cfg, reporter, &mut cm)?;

    let names = hir
        .defs
        .values()
        .filter_map(|def| def.name.as_ref().map(|sym| sym.id))
        .collect();

    let mut ctx = CompileContext {
        hir,
        libm: &libm,
        format: &cfg.format,
        cm: &mut cm,
        reporter,
        names: NameGenerator::new(names),
        signatures: HashMap::new(),
    };

    for (idx, def) in &hir.defs {
        let component = compile_definition(idx, def, &mut ctx)?;

        ctx.cm.components.push(component);
    }

    Some(cm.into_program())
}
