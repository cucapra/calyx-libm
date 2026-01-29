//! Pretty-printing.

use std::fmt::{self, Write};

use pretty::{Arena, DocAllocator, DocBuilder};

use super::ast::*;
use super::metadata::*;

const STANDARD_INDENT: isize = 2;
const SPECIAL_INDENT: isize = 4;
const TOP_LEVEL_INDENT: isize = 1;

pub struct Printer<'ast> {
    defs: &'ast [FPCore],
    width: usize,
}

impl<'ast> Printer<'ast> {
    pub fn new(defs: &'ast [FPCore], width: usize) -> Printer<'ast> {
        Printer { defs, width }
    }
}

impl fmt::Display for Printer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, def) in self.defs.iter().enumerate() {
            if i > 0 {
                f.write_char('\n')?;
            }

            def.pretty::<_, ()>(&Arena::new())
                .render_fmt(self.width, f)?;
        }

        Ok(())
    }
}

impl FPCore {
    fn pretty<'a, D, A>(&'a self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        allocator
            .text("FPCore")
            .append(self.name.as_ref().map_or_else(
                || allocator.nil(),
                |name| allocator.space().append(name.pretty(allocator)),
            ))
            .append(allocator.space())
            .append(
                allocator
                    .intersperse(
                        self.args.iter().map(|arg| arg.pretty(allocator)),
                        allocator.line(),
                    )
                    .align()
                    .group()
                    .parens(),
            )
            .append(allocator.concat(
                self.props.iter().map(|prop| {
                    allocator.line().append(prop.pretty(allocator))
                }),
            ))
            .append(allocator.line())
            .append(self.body.pretty(allocator))
            .nest(TOP_LEVEL_INDENT)
            .group()
            .parens()
            .append(allocator.hardline())
    }
}

impl Dimension {
    fn pretty<'a, D, A>(&self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
    {
        match self {
            Dimension::Id(sym) => sym.pretty(allocator),
            Dimension::Num(num) => num.pretty(allocator),
        }
    }
}

impl Argument {
    fn pretty<'a, D, A>(&'a self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        if self.props.is_empty() && self.dims.is_empty() {
            return self.var.pretty(allocator);
        }

        let bang = if self.props.is_empty() {
            allocator.nil()
        } else {
            allocator.text("!").append(allocator.space())
        };

        bang.append(
            allocator
                .concat(self.props.iter().map(|prop| {
                    prop.pretty(allocator).append(allocator.line())
                }))
                .append(self.var.pretty(allocator))
                .append(allocator.concat(self.dims.iter().map(|dim| {
                    allocator.space().append(dim.pretty(allocator))
                })))
                .align()
                .group(),
        )
        .parens()
    }
}

impl Binding {
    fn pretty<'a, D, A>(&'a self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        self.var
            .pretty(allocator)
            .append(allocator.space())
            .append(self.expr.pretty(allocator))
            .brackets()
    }
}

impl MutableVar {
    fn pretty<'a, D, A>(&'a self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        self.var
            .pretty(allocator)
            .append(allocator.space())
            .append(self.init.pretty(allocator))
            .append(allocator.space())
            .append(self.update.pretty(allocator))
            .brackets()
    }
}

impl InductionVar {
    fn pretty<'a, D, A>(&'a self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        self.var
            .pretty(allocator)
            .append(allocator.space())
            .append(self.size.pretty(allocator))
            .brackets()
    }
}

impl Expression {
    fn pretty<'a, D, A>(&'a self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match &self.kind {
            ExprKind::Num(num) => num.pretty(allocator),
            ExprKind::Const(constant) => constant.pretty(allocator),
            ExprKind::Id(sym) => sym.pretty(allocator),
            ExprKind::Op(op, args) => op
                .pretty(allocator)
                .append(allocator.space())
                .append(
                    allocator
                        .intersperse(
                            args.iter().map(|arg| arg.pretty(allocator)),
                            allocator.line(),
                        )
                        .align()
                        .group(),
                )
                .parens(),
            ExprKind::If {
                cond,
                true_branch,
                false_branch,
            } => allocator
                .text("if")
                .append(allocator.space())
                .append(cond.pretty(allocator))
                .append(allocator.line())
                .append(true_branch.pretty(allocator))
                .append(allocator.line())
                .append(false_branch.pretty(allocator))
                .nest(SPECIAL_INDENT)
                .group()
                .parens(),
            ExprKind::Let {
                bindings,
                body,
                sequential,
            } => allocator
                .text(if *sequential { "let*" } else { "let" })
                .append(allocator.space())
                .append(
                    allocator
                        .intersperse(
                            bindings
                                .iter()
                                .map(|binding| binding.pretty(allocator)),
                            allocator.line(),
                        )
                        .align()
                        .group()
                        .parens(),
                )
                .append(allocator.line())
                .append(body.pretty(allocator))
                .nest(STANDARD_INDENT)
                .group()
                .parens(),
            ExprKind::While {
                cond,
                vars,
                body,
                sequential,
            } => allocator
                .text(if *sequential { "while*" } else { "while" })
                .append(allocator.space())
                .append(cond.pretty(allocator))
                .append(allocator.line())
                .append(
                    allocator
                        .intersperse(
                            vars.iter().map(|var| var.pretty(allocator)),
                            allocator.line(),
                        )
                        .align()
                        .group()
                        .parens(),
                )
                .append(allocator.line())
                .append(body.pretty(allocator))
                .nest(STANDARD_INDENT)
                .group()
                .parens(),
            ExprKind::For {
                indices,
                vars,
                body,
                ..
            }
            | ExprKind::TensorStar {
                indices,
                vars,
                body,
            } => allocator
                .text(match &self.kind {
                    ExprKind::For { sequential, .. } => {
                        if *sequential {
                            "for*"
                        } else {
                            "for"
                        }
                    }
                    _ => "tensor*",
                })
                .append(allocator.space())
                .append(
                    allocator
                        .intersperse(
                            indices.iter().map(|var| var.pretty(allocator)),
                            allocator.line(),
                        )
                        .align()
                        .group()
                        .parens(),
                )
                .append(allocator.line())
                .append(
                    allocator
                        .intersperse(
                            vars.iter().map(|var| var.pretty(allocator)),
                            allocator.line(),
                        )
                        .align()
                        .group()
                        .parens(),
                )
                .append(allocator.line())
                .append(body.pretty(allocator))
                .nest(STANDARD_INDENT)
                .group()
                .parens(),
            ExprKind::Tensor { indices, body } => allocator
                .text("tensor")
                .append(allocator.space())
                .append(
                    allocator
                        .intersperse(
                            indices.iter().map(|var| var.pretty(allocator)),
                            allocator.line(),
                        )
                        .align()
                        .group()
                        .parens(),
                )
                .append(allocator.line())
                .append(body.pretty(allocator))
                .nest(STANDARD_INDENT)
                .group()
                .parens(),
            ExprKind::Cast(expr) => allocator
                .text("cast")
                .append(allocator.softline())
                .append(expr.pretty(allocator).align())
                .nest(STANDARD_INDENT)
                .group()
                .parens(),
            ExprKind::Array(exprs) => allocator
                .text("array")
                .append(match exprs.is_empty() {
                    true => allocator.nil(),
                    false => allocator
                        .softline()
                        .append(
                            allocator
                                .intersperse(
                                    exprs
                                        .iter()
                                        .map(|expr| expr.pretty(allocator)),
                                    allocator.line(),
                                )
                                .align()
                                .group(),
                        )
                        .nest(STANDARD_INDENT)
                        .group(),
                })
                .parens(),
            ExprKind::Annotation { props, body } => allocator
                .text("!")
                .append(allocator.space())
                .append(
                    allocator
                        .concat(props.iter().map(|prop| {
                            prop.pretty(allocator).append(allocator.line())
                        }))
                        .append(body.pretty(allocator))
                        .align()
                        .group(),
                )
                .parens(),
        }
    }
}

impl Number {
    fn pretty<'a, D, A>(&self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
    {
        allocator.as_string(&self.value)
    }
}

impl Symbol {
    fn pretty<'a, D, A>(&self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
    {
        allocator.text(self.id.id.as_str())
    }
}

impl Operation {
    fn pretty<'a, D, A>(&self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
    {
        allocator.text::<&str>(match self.kind {
            OpKind::Math(op) => op.into(),
            OpKind::Test(op) => op.into(),
            OpKind::Tensor(op) => op.into(),
            OpKind::FPCore(op) => op.id.as_str(),
        })
    }
}

impl Constant {
    fn pretty<'a, D, A>(&self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
    {
        allocator.text(match self {
            Constant::Math(constant) => constant.into(),
            Constant::Bool(false) => "FALSE",
            Constant::Bool(true) => "TRUE",
        })
    }
}

impl Property {
    fn pretty<'a, D, A>(&'a self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match &self.kind {
            PropKind::Name(name) => allocator
                .text(":name")
                .append(allocator.space())
                .append(allocator.text(name).double_quotes()),
            PropKind::Description(desc) => allocator
                .text(":description")
                .append(allocator.space())
                .append(allocator.text(desc).double_quotes()),
            PropKind::Cite(symbols) => allocator
                .text(":cite")
                .append(allocator.softline())
                .append(
                    allocator
                        .intersperse(
                            symbols.iter().map(|arg| arg.pretty(allocator)),
                            allocator.softline(),
                        )
                        .align()
                        .parens(),
                )
                .nest(STANDARD_INDENT)
                .group(),
            PropKind::Precision(prec) => allocator
                .text(":precision")
                .append(allocator.line())
                .append(match prec {
                    Precision::Float { e, nbits } => allocator
                        .text("float")
                        .append(allocator.line())
                        .append(allocator.as_string(e))
                        .append(allocator.line())
                        .append(allocator.as_string(nbits))
                        .nest(STANDARD_INDENT)
                        .group()
                        .parens(),
                    Precision::Posit { es, nbits } => allocator
                        .text("posit")
                        .append(allocator.line())
                        .append(allocator.as_string(es))
                        .append(allocator.line())
                        .append(allocator.as_string(nbits))
                        .nest(STANDARD_INDENT)
                        .group()
                        .parens(),
                    Precision::Fixed { scale, nbits } => allocator
                        .text("fixed")
                        .append(allocator.line())
                        .append(allocator.as_string(scale))
                        .append(allocator.line())
                        .append(allocator.as_string(nbits))
                        .nest(STANDARD_INDENT)
                        .group()
                        .parens(),
                    Precision::Real => allocator.text("real"),
                    Precision::Integer => allocator.text("integer"),
                })
                .nest(STANDARD_INDENT)
                .group(),
            PropKind::Round(mode) => allocator
                .text(":round")
                .append(allocator.line())
                .append(allocator.text::<&str>(mode.into()))
                .nest(STANDARD_INDENT)
                .group(),
            PropKind::Overflow(mode) => allocator
                .text(":overflow")
                .append(allocator.line())
                .append(allocator.text::<&str>(mode.into()))
                .nest(STANDARD_INDENT)
                .group(),
            PropKind::Pre(expr) => allocator
                .text(":pre")
                .append(allocator.softline())
                .append(expr.pretty(allocator).align())
                .nest(STANDARD_INDENT)
                .group(),
            PropKind::Spec(expr) => allocator
                .text(":spec")
                .append(allocator.softline())
                .append(expr.pretty(allocator).align())
                .nest(STANDARD_INDENT)
                .group(),
            PropKind::Alt(expr) => allocator
                .text(":alt")
                .append(allocator.softline())
                .append(expr.pretty(allocator).align())
                .nest(STANDARD_INDENT)
                .group(),
            PropKind::MathLib(sym) => allocator
                .text(":math-library")
                .append(allocator.line())
                .append(sym.pretty(allocator))
                .nest(STANDARD_INDENT)
                .group(),
            PropKind::Example(bindings) => allocator
                .text(":example")
                .append(allocator.softline())
                .append(
                    allocator
                        .intersperse(
                            bindings
                                .iter()
                                .map(|binding| binding.pretty(allocator)),
                            allocator.line(),
                        )
                        .align()
                        .group()
                        .parens(),
                )
                .nest(STANDARD_INDENT)
                .group(),
            PropKind::CalyxDomain(CalyxDomain { left, right }) => allocator
                .text(":calyx-domain")
                .append(allocator.softline())
                .append(
                    left.pretty(allocator)
                        .append(allocator.line())
                        .append(right.pretty(allocator))
                        .align()
                        .group()
                        .parens(),
                )
                .nest(STANDARD_INDENT)
                .group(),
            PropKind::CalyxImpl(CalyxImpl::Lut { size }) => allocator
                .text(":calyx-impl")
                .append(allocator.line())
                .append(
                    allocator
                        .text("lut")
                        .append(allocator.line())
                        .append(allocator.as_string(size))
                        .nest(STANDARD_INDENT)
                        .group()
                        .parens(),
                )
                .nest(STANDARD_INDENT)
                .group(),
            PropKind::CalyxImpl(CalyxImpl::Poly { degree, error }) => allocator
                .text(":calyx-impl")
                .append(allocator.line())
                .append(
                    allocator
                        .text("poly")
                        .append(allocator.line())
                        .append(allocator.as_string(degree))
                        .append(error.as_ref().map_or_else(
                            || allocator.nil(),
                            |error| {
                                allocator.line().append(error.pretty(allocator))
                            },
                        ))
                        .nest(STANDARD_INDENT)
                        .group()
                        .parens(),
                )
                .nest(STANDARD_INDENT)
                .group(),
            PropKind::Unknown(name, data) => allocator
                .text(":")
                .append(name.pretty(allocator))
                .append(allocator.softline())
                .append(data.pretty(allocator))
                .nest(STANDARD_INDENT)
                .group(),
        }
    }
}

impl Data {
    fn pretty<'a, D, A>(&'a self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match self {
            Data::Symbol(sym) => sym.pretty(allocator),
            Data::Num(num) => num.pretty(allocator),
            Data::Str(string) => allocator.text(string).double_quotes(),
            Data::List(data) => allocator
                .intersperse(
                    data.iter().map(|data| data.pretty(allocator)),
                    allocator.line(),
                )
                .align()
                .group()
                .parens(),
        }
    }
}
