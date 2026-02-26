use std::fmt::{self, Write};
use std::ops::Index;

use itertools::Itertools;

use calyx_libm_hir::{self as hir, Metadata, Visitor, arena::SecondaryMap};
use calyx_libm_utils::rational::Dyadic;
use calyx_libm_utils::sollya::{self, ScriptError};
use calyx_libm_utils::{Config, Diagnostic, Format, Reporter};

use super::Precondition;

const PROLOGUE: &str = "\
dieonerrormode = on!;
display = dyadic!;

s = parse(__argv[0]);

procedure rnd(x) {
    return [
        floor(inf(x) / 2^s) * 2^s;
        ceil(sup(x) / 2^s) * 2^s
    ];
};

procedure fmax(x, y) {
    return [max(inf(x), inf(y)); max(sup(x), sup(y))];
};

procedure fmin(x, y) {
    return [min(inf(x), inf(y)); min(sup(x), sup(y))];
};
";

const EPILOGUE: &str = "quit;\n";

pub struct RangeAnalysis {
    ranges: SecondaryMap<hir::ExprIdx, [hir::Rational; 2]>,
}

impl RangeAnalysis {
    pub fn new(
        ctx: &hir::Context,
        cfg: &Config,
        reporter: &mut Reporter,
    ) -> Option<RangeAnalysis> {
        let mut builder = Builder {
            reporter,
            script: String::from(PROLOGUE),
        };

        builder.visit_definitions(ctx).ok()?;

        builder.script.push_str(EPILOGUE);

        let ranges = run_script(&builder.script, &cfg.format)
            .map_err(|err| {
                builder.reporter.emit(&err.into_diagnostic(ctx));
            })
            .ok()?;

        Some(RangeAnalysis { ranges })
    }
}

fn run_script(
    script: &str,
    format: &Format,
) -> Result<SecondaryMap<hir::ExprIdx, [hir::Rational; 2]>, AnalysisError> {
    let result =
        sollya::sollya(script.as_bytes(), &[&format.scale.to_string()])?;

    result.lines().map(parse_response_line).collect()
}

fn parse_response_line(
    line: &str,
) -> Result<(hir::ExprIdx, [hir::Rational; 2]), AnalysisError> {
    let (idx, left, right) = line
        .split(' ')
        .collect_tuple()
        .ok_or(ScriptError::BadResponse)?;

    let idx = idx
        .parse()
        .map(hir::ExprIdx::from_u32)
        .map_err(|_| ScriptError::BadResponse)?;

    let left = parse_dyadic(left, idx)?;
    let right = parse_dyadic(right, idx)?;

    Ok((idx, [left, right]))
}

fn parse_dyadic(
    s: &str,
    idx: hir::ExprIdx,
) -> Result<hir::Rational, AnalysisError> {
    match s {
        "-infty" | "infty" => Err(AnalysisError::Unbounded(idx)),
        "NaN" => Err(AnalysisError::Undefined(idx)),
        _ => hir::Rational::from_dyadic(s).ok_or(AnalysisError::BAD_RESPONSE),
    }
}

impl Index<hir::ExprIdx> for RangeAnalysis {
    type Output = [hir::Rational; 2];

    fn index(&self, index: hir::ExprIdx) -> &Self::Output {
        &self.ranges[index]
    }
}

enum AnalysisError {
    Script(ScriptError),
    Unbounded(hir::ExprIdx),
    Undefined(hir::ExprIdx),
}

impl AnalysisError {
    const BAD_RESPONSE: AnalysisError =
        AnalysisError::Script(ScriptError::BadResponse);

    fn into_diagnostic(self, ctx: &hir::Context) -> Diagnostic {
        match self {
            AnalysisError::Script(err) => Diagnostic::from_sollya(err),
            AnalysisError::Unbounded(idx) => Diagnostic::error()
                .with_message("couldn't bound ranges")
                .try_with_primary(
                    ctx[idx].span,
                    "expression has unbounded range",
                ),
            AnalysisError::Undefined(idx) => Diagnostic::error()
                .with_message("couldn't bound ranges")
                .try_with_primary(ctx[idx].span, "expression not total"),
        }
    }
}

impl<T: Into<ScriptError>> From<T> for AnalysisError {
    fn from(err: T) -> Self {
        AnalysisError::Script(err.into())
    }
}

#[derive(Debug)]
struct BuilderError;

struct Builder<'a, 'src> {
    reporter: &'a mut Reporter<'src>,
    script: String,
}

impl Builder<'_, '_> {
    fn script_bool<D: SollyaVar>(&mut self, dst: D) {
        writeln!(self.script, "{} = [0;1];", dst.sollya_var()).unwrap();
    }

    fn script_point<D, N>(&mut self, dst: D, num: &N)
    where
        D: SollyaVar,
        N: fmt::Display,
    {
        writeln!(self.script, "{} = rnd([{num}]);", dst.sollya_var()).unwrap();
    }

    fn script_interval<D, N>(&mut self, dst: D, left: &N, right: &N)
    where
        D: SollyaVar,
        N: fmt::Display,
    {
        writeln!(self.script, "{} = rnd([{left};{right}]);", dst.sollya_var())
            .unwrap();
    }

    fn script_unary<D, S>(&mut self, dst: D, f: &str, src: S)
    where
        D: SollyaVar,
        S: SollyaVar,
    {
        writeln!(
            self.script,
            "{} = rnd({f}({}));",
            dst.sollya_var(),
            src.sollya_var(),
        )
        .unwrap();
    }

    fn script_binary<D, A, B>(&mut self, dst: D, f: &str, a: A, b: B)
    where
        D: SollyaVar,
        A: SollyaVar,
        B: SollyaVar,
    {
        writeln!(
            self.script,
            "{} = rnd({f}({}, {}));",
            dst.sollya_var(),
            a.sollya_var(),
            b.sollya_var(),
        )
        .unwrap();
    }

    fn script_infix<D, L, R>(&mut self, dst: D, left: L, op: &str, right: R)
    where
        D: SollyaVar,
        L: SollyaVar,
        R: SollyaVar,
    {
        writeln!(
            self.script,
            "{} = rnd({} {op} {});",
            dst.sollya_var(),
            left.sollya_var(),
            right.sollya_var(),
        )
        .unwrap();
    }

    fn script_union<D, L, R>(&mut self, dst: D, left: L, right: R)
    where
        D: SollyaVar,
        L: SollyaVar,
        R: SollyaVar,
    {
        writeln!(
            self.script,
            "{0} = [min(inf({1}), inf({2})); max(sup({1}), sup({2}))];",
            dst.sollya_var(),
            left.sollya_var(),
            right.sollya_var(),
        )
        .unwrap();
    }

    fn script_assign<D, S>(&mut self, dst: D, src: S)
    where
        D: SollyaVar,
        S: SollyaVar,
    {
        writeln!(self.script, "{} = {};", dst.sollya_var(), src.sollya_var())
            .unwrap();
    }

    fn script_print(&mut self, expr: hir::ExprIdx) {
        writeln!(
            self.script,
            "print(\"{0}\", inf({1}), sup({1}));",
            expr.as_u32(),
            expr.sollya_var(),
        )
        .unwrap();
    }
}

impl Visitor for Builder<'_, '_> {
    type Error = BuilderError;

    fn visit_definition(
        &mut self,
        _: hir::DefIdx,
        def: &hir::Definition,
        ctx: &hir::Context,
    ) -> Result<(), BuilderError> {
        let mut pre = Precondition::new();

        let preconditions = def.props(ctx).filter_map(|prop| match prop {
            hir::Property::Pre(expr) => Some(*expr),
            _ => None,
        });

        for expr in preconditions {
            pre.add_constraint(expr, ctx, self.reporter)
                .map_err(|_| BuilderError)?;
        }

        for arg in def.args {
            let (left, right) = pre
                .domains
                .get(&arg)
                .and_then(|domain| domain.bounds())
                .ok_or_else(|| {
                    self.reporter.emit(
                        &Diagnostic::error()
                            .with_message("precondition is too weak")
                            .with_primary(
                                ctx[arg].name.span,
                                "argument has unbounded domain",
                            ),
                    );

                    BuilderError
                })?;

            self.script_interval(arg, left, right);
        }

        self.visit_expression(def.body, ctx)
    }

    fn visit_expression(
        &mut self,
        idx: hir::ExprIdx,
        ctx: &hir::Context,
    ) -> Result<(), BuilderError> {
        let expr = &ctx[idx];

        match &expr.kind {
            hir::ExprKind::Num(num) => {
                self.script_point(idx, &ctx[*num].value);
            }
            hir::ExprKind::Const(hir::Constant::Math(_)) => {
                self.reporter.emit(
                    &Diagnostic::error()
                        .with_message("unsupported numeric constant")
                        .try_with_primary(expr.span, "unsupported constant"),
                );

                return Err(BuilderError);
            }
            hir::ExprKind::Const(hir::Constant::Bool(_)) => {
                self.script_bool(idx);
            }
            hir::ExprKind::Var(_, hir::VarKind::Arg(arg)) => {
                self.script_assign(idx, *arg);
            }
            hir::ExprKind::Var(_, hir::VarKind::Let(expr)) => {
                self.script_assign(idx, *expr);
            }
            hir::ExprKind::Var(_, hir::VarKind::Mut) => unreachable!(),
            hir::ExprKind::Op(
                hir::Operation {
                    kind: hir::OpKind::Arith(op),
                    ..
                },
                args,
            ) => {
                let args = &ctx[*args];

                for &arg in args {
                    self.visit_expression(arg, ctx)?;
                }

                enum Kind {
                    Unary,
                    Binary,
                    Infix,
                }

                let (op, kind) = match op {
                    hir::ArithOp::Add => ("+", Kind::Infix),
                    hir::ArithOp::Sub => ("-", Kind::Infix),
                    hir::ArithOp::Mul => ("*", Kind::Infix),
                    hir::ArithOp::Div => ("/", Kind::Infix),
                    hir::ArithOp::Neg => ("-", Kind::Unary),
                    hir::ArithOp::Pow => ("^", Kind::Infix),
                    hir::ArithOp::Sqrt => ("sqrt", Kind::Unary),
                    hir::ArithOp::Abs => ("abs", Kind::Unary),
                    hir::ArithOp::Max => ("fmax", Kind::Binary),
                    hir::ArithOp::Min => ("fmin", Kind::Binary),
                };

                match kind {
                    Kind::Unary => {
                        self.script_unary(idx, op, args[0]);
                    }
                    Kind::Binary => {
                        self.script_binary(idx, op, args[0], args[1]);
                    }
                    Kind::Infix => {
                        self.script_infix(idx, args[0], op, args[1]);
                    }
                }
            }
            hir::ExprKind::Op(
                hir::Operation {
                    kind: hir::OpKind::Test(_),
                    ..
                },
                args,
            ) => {
                for &arg in &ctx[*args] {
                    self.visit_expression(arg, ctx)?;
                }

                self.script_bool(idx);
            }
            hir::ExprKind::Op(
                hir::Operation {
                    kind: hir::OpKind::Sollya(op),
                    ..
                },
                args,
            ) => {
                let arg = ctx[*args][0];
                let op = format!("({})", op.sollya(ctx));

                self.visit_expression(arg, ctx)?;
                self.script_unary(idx, &op, arg);
            }
            hir::ExprKind::Op(
                hir::Operation {
                    kind: hir::OpKind::Def(def),
                    ..
                },
                args,
            ) => {
                for &arg in &ctx[*args] {
                    self.visit_expression(arg, ctx)?;
                }

                self.script_assign(idx, ctx[*def].body);
            }
            hir::ExprKind::If(expr) => {
                self.visit_expression(expr.cond, ctx)?;
                self.visit_expression(expr.if_true, ctx)?;
                self.visit_expression(expr.if_false, ctx)?;

                self.script_union(idx, expr.if_true, expr.if_false);
            }
            hir::ExprKind::Let(expr) => {
                for &write in &ctx[expr.writes] {
                    self.visit_expression(ctx[write].val, ctx)?;
                }

                self.visit_expression(expr.body, ctx)?;
                self.script_assign(idx, expr.body);
            }
            hir::ExprKind::While(_) => {
                self.reporter.emit(
                    &Diagnostic::error()
                        .with_message("range analysis does not support loops")
                        .try_with_primary(expr.span, "unsupported expression"),
                );

                return Err(BuilderError);
            }
        };

        self.script_print(idx);

        Ok(())
    }
}

trait SollyaVar {
    fn sollya_var(self) -> impl fmt::Display;
}

impl SollyaVar for hir::ArgIdx {
    fn sollya_var(self) -> impl fmt::Display {
        struct Adapter(hir::ArgIdx);

        impl fmt::Display for Adapter {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "a{:x}", self.0.as_u32())
            }
        }

        Adapter(self)
    }
}

impl SollyaVar for hir::ExprIdx {
    fn sollya_var(self) -> impl fmt::Display {
        struct Adapter(hir::ExprIdx);

        impl fmt::Display for Adapter {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "e{:x}", self.0.as_u32())
            }
        }

        Adapter(self)
    }
}
