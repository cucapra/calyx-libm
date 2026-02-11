use std::collections::{HashMap, HashSet};
use std::{io, iter, mem};

use calyx_ir as ir;
use itertools::Itertools;

use calyx_libm_hir as hir;
use calyx_libm_utils::rational::{FixedPoint, RoundBinary};
use calyx_libm_utils::{Config, Diagnostic, Format, Reporter};

use super::libm::{self, Prototype};
use super::stdlib::{ImportPaths, ImportSet, Primitive};
use super::{ComponentManager, IrBuilder};

pub struct Program {
    imports: ImportSet,
    context: ir::Context,
}

impl Program {
    pub fn write<W: io::Write>(&self, out: &mut W) -> io::Result<()> {
        for import in self.imports.paths() {
            writeln!(out, "import \"{}\";", import)?;
        }

        ir::Printer::write_context(&self.context, true, out)
    }

    pub fn write_with_paths<W: io::Write>(
        &self,
        paths: &ImportPaths,
        out: &mut W,
    ) -> io::Result<()> {
        for import in self.imports.paths_from(paths) {
            writeln!(out, "import \"{}\";", import)?;
        }

        ir::Printer::write_context(&self.context, true, out)
    }
}

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

struct Builder<'a, 'comp, 'src> {
    ctx: &'a hir::Context,
    signatures: &'a HashMap<hir::DefIdx, Prototype>,
    math_lib: &'a HashMap<hir::ExprIdx, Prototype>,
    format: &'a Format,

    cm: &'a mut ComponentManager,
    reporter: &'a mut Reporter<'src>,
    builder: &'a mut IrBuilder<'comp>,

    stores: HashMap<hir::VarIdx, ir::RRC<ir::Cell>>,
}

impl Builder<'_, '_, '_> {
    fn compile_number(&mut self, number: &hir::Number) -> Option<CompiledExpr> {
        let width = u64::from(self.format.width);
        let rounded = (&number.value).round_convergent(self.format.lsb());

        let Some(value) = rounded.to_fixed_point(self.format) else {
            self.reporter.emit(
                &Diagnostic::error()
                    .with_message("overflow")
                    .try_with_primary(
                        number.span,
                        "value overflows target format",
                    ),
            );

            return None;
        };

        let cell = self.builder.big_constant(&value, width, self.cm);
        let port = cell.borrow().get("out");

        Some(CompiledExpr::from_port(port))
    }

    fn compile_boolean(&mut self, value: bool) -> Option<CompiledExpr> {
        let params = [1, u64::from(value)];

        let cell = self.builder.add_primitive("c", "std_const", &params);
        let port = cell.borrow().get("out");

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

        let (control, args): (Vec<_>, Vec<_>) = self.ctx[args]
            .iter()
            .map(|&arg| {
                let expr = self.compile_expression(arg)?;

                assignments.extend(expr.assignments);

                Some((expr.control, expr.out))
            })
            .collect::<Option<_>>()?;

        let control = IrBuilder::collapse(control, ir::Control::par);

        let mut reduce = |left, right, decl: &Primitive| {
            let cell = self.builder.add_primitive(
                decl.prefix_hint,
                decl.name,
                &decl.build_params(self.format),
            );

            let cell = cell.borrow();

            assignments.push(ir::Assignment::new(cell.get("left"), left));
            assignments.push(ir::Assignment::new(cell.get("right"), right));

            cell.get("out")
        };

        let args = match op {
            hir::TestOp::Lt
            | hir::TestOp::Gt
            | hir::TestOp::Leq
            | hir::TestOp::Geq
            | hir::TestOp::Eq => {
                let decl = match op {
                    hir::TestOp::Lt => self.cm.importer.lt(self.format),
                    hir::TestOp::Gt => self.cm.importer.gt(self.format),
                    hir::TestOp::Leq => self.cm.importer.le(self.format),
                    hir::TestOp::Geq => self.cm.importer.ge(self.format),
                    hir::TestOp::Eq => self.cm.importer.eq(self.format),
                    _ => unreachable!(),
                };

                args.into_iter()
                    .tuple_windows()
                    .map(|(left, right)| reduce(left, right, decl))
                    .collect()
            }
            hir::TestOp::Neq => {
                let decl = self.cm.importer.neq(self.format);

                args.into_iter()
                    .tuple_combinations()
                    .map(|(left, right)| reduce(left, right, decl))
                    .collect()
            }
            hir::TestOp::And | hir::TestOp::Or => args,
            _ => unreachable!(),
        };

        let decl = if matches!(op, hir::TestOp::Or) {
            self.cm.importer.or()
        } else {
            self.cm.importer.and()
        };

        let out = args
            .into_iter()
            .tree_fold1(|left, right| reduce(left, right, decl))
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

        let (control, args): (Vec<_>, Vec<_>) = self.ctx[args]
            .iter()
            .map(|&arg| {
                let expr = self.compile_expression(arg)?;

                assignments.extend(expr.assignments);

                Some((expr.control, expr.out))
            })
            .collect::<Option<_>>()?;

        let control = IrBuilder::collapse(control, ir::Control::par);
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

            IrBuilder::collapse([control, invoke], ir::Control::seq)
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
            ir::Id::new("out"),
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
                let decl = match op {
                    hir::ArithOp::Add => self.cm.importer.add(self.format),
                    hir::ArithOp::Sub => self.cm.importer.sub(self.format),
                    hir::ArithOp::Mul => self.cm.importer.mul(self.format),
                    hir::ArithOp::Div => self.cm.importer.div(self.format),
                    hir::ArithOp::Neg => self.cm.importer.neg(self.format),
                    hir::ArithOp::Sqrt => self.cm.importer.sqrt(self.format),
                    hir::ArithOp::Abs => self.cm.importer.abs(self.format),
                    hir::ArithOp::Max => self.cm.importer.max(self.format),
                    hir::ArithOp::Min => self.cm.importer.min(self.format),
                    hir::ArithOp::Pow => {
                        unsupported();

                        return None;
                    }
                };

                self.compile_primitive_operation(decl, args)
            }
            hir::OpKind::Test(op) => {
                if matches!(op, hir::TestOp::Not) {
                    let decl = self.cm.importer.not();

                    self.compile_primitive_operation(decl, args)
                } else if op.is_variadic() {
                    self.compile_variadic_operation(op, args)
                } else {
                    unsupported();

                    None
                }
            }
            hir::OpKind::Sollya(_) => {
                self.compile_component_operation(&self.math_lib[&idx], args)
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

        let params = [true_branch.out.borrow().width];

        match (&true_branch.control, &false_branch.control) {
            (ir::Control::Empty(_), ir::Control::Empty(_)) => {
                let mux = self.builder.add_primitive("mux", "std_mux", &params);
                let out = mux.borrow().get("out");

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
                let reg = self.builder.add_primitive("r", "std_reg", &params);
                let out = reg.borrow().get("out");

                let store_true = self.builder.invoke_with(
                    reg.clone(),
                    vec![(ir::Id::new("in"), true_branch.out)],
                    "branch",
                    true_branch.assignments,
                );

                let store_false = self.builder.invoke_with(
                    reg,
                    vec![(ir::Id::new("in"), false_branch.out)],
                    "branch",
                    false_branch.assignments,
                );

                let group =
                    self.builder.add_comb_group("cond", cond.assignments);

                let conditional = ir::Control::if_(
                    cond.out,
                    Some(group),
                    Box::new(IrBuilder::collapse(
                        [true_branch.control, store_true],
                        ir::Control::seq,
                    )),
                    Box::new(IrBuilder::collapse(
                        [false_branch.control, store_false],
                        ir::Control::seq,
                    )),
                );

                let control = IrBuilder::collapse(
                    [cond.control, conditional],
                    ir::Control::seq,
                );

                Some(CompiledExpr {
                    control,
                    assignments: Vec::new(),
                    out,
                })
            }
        }
    }

    fn compile_let(&mut self, expr: &hir::Let) -> Option<CompiledExpr> {
        let (args, stores): (Vec<_>, Vec<_>) = self.ctx[expr.writes]
            .iter()
            .map(|&write| {
                let write = &self.ctx[write];
                let expr = self.compile_expression(write.val)?;

                let params = [expr.out.borrow().width];
                let reg = self.builder.add_primitive("r", "std_reg", &params);

                let invoke = self.builder.invoke_with(
                    reg.clone(),
                    vec![(ir::Id::new("in"), expr.out)],
                    "expr",
                    expr.assignments,
                );

                self.stores.insert(write.var, reg);

                Some((expr.control, invoke))
            })
            .collect::<Option<_>>()?;

        let body = self.compile_expression(expr.body)?;

        let control = if expr.sequential {
            IrBuilder::collapse(
                itertools::interleave(args, stores)
                    .chain(iter::once(body.control)),
                ir::Control::seq,
            )
        } else {
            IrBuilder::collapse(
                [
                    IrBuilder::collapse(args, ir::Control::par),
                    IrBuilder::collapse(stores, ir::Control::par),
                    body.control,
                ],
                ir::Control::seq,
            )
        };

        Some(CompiledExpr { control, ..body })
    }

    fn compile_while(&mut self, expr: &hir::While) -> Option<CompiledExpr> {
        let (inits, init_stores): (Vec<_>, Vec<_>) = self.ctx[expr.inits]
            .iter()
            .map(|&write| {
                let write = &self.ctx[write];
                let init = self.compile_expression(write.val)?;

                let params = [init.out.borrow().width];
                let reg = self.builder.add_primitive("r", "std_reg", &params);

                let invoke = self.builder.invoke_with(
                    reg.clone(),
                    vec![(ir::Id::new("in"), init.out)],
                    "init",
                    init.assignments,
                );

                self.stores.insert(write.var, reg);

                Some((init.control, invoke))
            })
            .collect::<Option<_>>()?;

        let cond = self.compile_expression(expr.cond)?;
        let body = self.compile_expression(expr.body)?;

        let (updates, update_stores): (Vec<_>, Vec<_>) = self.ctx[expr.updates]
            .iter()
            .map(|&write| {
                let write = &self.ctx[write];
                let update = self.compile_expression(write.val)?;
                let reg = self.stores[&write.var].clone();

                let invoke = self.builder.invoke_with(
                    reg,
                    vec![(ir::Id::new("in"), update.out)],
                    "update",
                    update.assignments,
                );

                Some((update.control, invoke))
            })
            .collect::<Option<_>>()?;

        let group = self.builder.add_comb_group("cond", cond.assignments);

        let control = if expr.sequential {
            IrBuilder::collapse(
                itertools::interleave(inits, init_stores).chain([
                    IrBuilder::clone_control(&cond.control),
                    ir::Control::while_(
                        cond.out,
                        Some(group),
                        Box::new(IrBuilder::collapse(
                            itertools::interleave(updates, update_stores)
                                .chain(iter::once(cond.control)),
                            ir::Control::seq,
                        )),
                    ),
                    body.control,
                ]),
                ir::Control::seq,
            )
        } else {
            IrBuilder::collapse(
                [
                    IrBuilder::collapse(inits, ir::Control::par),
                    IrBuilder::collapse(init_stores, ir::Control::par),
                    IrBuilder::clone_control(&cond.control),
                    ir::Control::while_(
                        cond.out,
                        Some(group),
                        Box::new(IrBuilder::collapse(
                            [
                                IrBuilder::collapse(updates, ir::Control::par),
                                IrBuilder::collapse(
                                    update_stores,
                                    ir::Control::par,
                                ),
                                cond.control,
                            ],
                            ir::Control::seq,
                        )),
                    ),
                    body.control,
                ],
                ir::Control::seq,
            )
        };

        Some(CompiledExpr { control, ..body })
    }

    fn compile_expression(
        &mut self,
        idx: hir::ExprIdx,
    ) -> Option<CompiledExpr> {
        let expr = &self.ctx[idx];

        match &expr.kind {
            hir::ExprKind::Num(idx) => self.compile_number(&self.ctx[*idx]),
            hir::ExprKind::Const(constant) => {
                self.compile_constant(*constant, expr.span)
            }
            hir::ExprKind::Var(_, hir::VarKind::Arg(arg)) => {
                let id = self.ctx[*arg].var.id;
                let port = self.builder.component.signature.borrow().get(id);

                Some(CompiledExpr::from_port(port))
            }
            hir::ExprKind::Var(var, _) => {
                let port = self.stores[var].borrow().get("out");

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
    ctx: &'a hir::Context,
    math_lib: &'a HashMap<hir::ExprIdx, Prototype>,
    format: &'a Format,

    cm: &'a mut ComponentManager,
    lib: &'a mut ir::LibrarySignatures,
    reporter: &'a mut Reporter<'src>,

    names: NameGenerator,
    signatures: HashMap<hir::DefIdx, Prototype>,
}

fn compile_definition(
    idx: hir::DefIdx,
    def: &hir::Definition,
    cc: &mut CompileContext,
) -> Option<ir::Component> {
    let name = def
        .name
        .as_ref()
        .map_or_else(|| cc.names.next(), |sym| ir::Id { id: sym.id });

    let ports = || {
        def.args
            .into_iter()
            .map(|arg| {
                ir::PortDef::new(
                    ir::Id {
                        id: cc.ctx[arg].var.id,
                    },
                    u64::from(cc.format.width),
                    ir::Direction::Input,
                    Default::default(),
                )
            })
            .chain(iter::once(ir::PortDef::new(
                "out",
                u64::from(cc.format.width),
                ir::Direction::Output,
                Default::default(),
            )))
            .collect()
    };

    let mut component = ir::Component::new(name, ports(), false, false, None);
    let mut builder = IrBuilder::new(&mut component, cc.lib);

    let mut compiler = Builder {
        ctx: cc.ctx,
        signatures: &cc.signatures,
        math_lib: cc.math_lib,
        format: cc.format,
        cm: cc.cm,
        reporter: cc.reporter,
        builder: &mut builder,
        stores: HashMap::new(),
    };

    let body = compiler.compile_expression(def.body)?;

    let assign = ir::Assignment::new(
        builder.component.signature.borrow().get("out"),
        body.out,
    );

    builder.add_continuous_assignments(body.assignments);
    builder.add_continuous_assignment(assign);

    component.is_comb = matches!(body.control, ir::Control::Empty(_));

    *component.control.borrow_mut() = body.control;

    if let Some(sym) = &def.name {
        let prototype = Prototype {
            name: component.name,
            prefix_hint: ir::Id { id: sym.id },
            signature: ports(),
            is_comb: component.is_comb,
        };

        cc.signatures.insert(idx, prototype);
    }

    Some(component)
}

pub fn compile_hir(
    ctx: &hir::Context,
    cfg: &Config,
    reporter: &mut Reporter,
    mut lib: ir::LibrarySignatures,
) -> Option<Program> {
    let mut cm = ComponentManager::new();

    let math_lib =
        libm::compile_math_library(ctx, cfg, reporter, &mut cm, &mut lib)?;

    let names = ctx
        .defs
        .values()
        .filter_map(|def| def.name.as_ref().map(|sym| sym.id))
        .collect();

    let mut cc = CompileContext {
        ctx,
        math_lib: &math_lib,
        format: &cfg.format,
        cm: &mut cm,
        lib: &mut lib,
        reporter,
        names: NameGenerator::new(names),
        signatures: HashMap::new(),
    };

    for (idx, def) in &ctx.defs {
        let component = compile_definition(idx, def, &mut cc)?;

        cc.cm.components.push(component);
    }

    Some(Program {
        imports: cm.importer.into_imports(),
        context: ir::Context {
            components: cm.components,
            lib,
            bc: Default::default(),
            entrypoint: ir::Id::new("main"),
            extra_opts: Vec::new(),
            metadata: None,
        },
    })
}
