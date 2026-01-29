//! Parser for FPCore.

use std::str::FromStr;

use pest::error::{Error, ErrorVariant, InputLocation};
use pest_consume::{Parser, match_nodes};

use super::{ast, literals, metadata as meta};

#[derive(Parser)]
#[grammar = "syntax.pest"]
pub struct FPCoreParser;

impl FPCoreParser {
    pub fn parse_file(src: &str) -> Result<Vec<ast::FPCore>, Box<Error<Rule>>> {
        let nodes = FPCoreParser::parse(Rule::file, src)?;

        FPCoreParser::file(nodes.single()?).map_err(Box::new)
    }
}

type ParseResult<T> = Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[pest_consume::parser]
impl FPCoreParser {
    fn EOI(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn file(input: Node) -> ParseResult<Vec<ast::FPCore>> {
        Ok(match_nodes!(input.into_children();
            [fpcore(defs).., EOI(_)] => defs.collect(),
        ))
    }

    fn fpcore(input: Node) -> ParseResult<ast::FPCore> {
        Ok(match_nodes!(input.into_children();
            [fpcore_kw(_), symbol_opt(name), argument_list(args), property(props).., expr(body)] => {
                ast::FPCore {
                    name,
                    args,
                    props: props.collect(),
                    body,
                }
            },
        ))
    }

    fn symbol_opt(input: Node) -> ParseResult<Option<ast::Symbol>> {
        Ok(match_nodes!(input.into_children();
            [symbol(sym)] => Some(sym),
            [] => None,
        ))
    }

    fn argument_list(input: Node) -> ParseResult<Vec<ast::Argument>> {
        Ok(match_nodes!(input.into_children();
            [argument(args)..] => args.collect(),
        ))
    }

    fn dimension(input: Node) -> ParseResult<ast::Dimension> {
        Ok(match_nodes!(input.into_children();
            [number(num)] => ast::Dimension::Num(num),
            [symbol(sym)] => ast::Dimension::Id(sym),
        ))
    }

    fn argument(input: Node) -> ParseResult<ast::Argument> {
        Ok(match_nodes!(input.into_children();
            [symbol(var)] => ast::Argument {
                var,
                props: Vec::new(),
                dims: Vec::new(),
                uid: ast::NodeId::new(),
            },
            [annotation(props), symbol(var), dimension(dims)..] => {
                ast::Argument {
                    var,
                    props,
                    dims: dims.collect(),
                    uid: ast::NodeId::new(),
                }
            },
            [symbol(var), dimension(dims)..] => ast::Argument {
                var,
                props: Vec::new(),
                dims: dims.collect(),
                uid: ast::NodeId::new(),
            },
        ))
    }

    fn binding(input: Node) -> ParseResult<ast::Binding> {
        Ok(match_nodes!(input.into_children();
            [symbol(var), expr(expr)] => ast::Binding {
                var,
                expr,
                uid: ast::NodeId::new(),
            },
        ))
    }

    fn mut_var(input: Node) -> ParseResult<ast::MutableVar> {
        Ok(match_nodes!(input.into_children();
            [symbol(var), expr(init), expr(update)] => ast::MutableVar {
                var,
                init,
                update,
                uid: ast::NodeId::new(),
            },
        ))
    }

    fn index_var(input: Node) -> ParseResult<ast::InductionVar> {
        Ok(match_nodes!(input.into_children();
            [symbol(var), expr(size)] => ast::InductionVar {
                var,
                size,
                uid: ast::NodeId::new(),
            },
        ))
    }

    fn mut_vars(input: Node) -> ParseResult<Vec<ast::MutableVar>> {
        Ok(match_nodes!(input.into_children();
            [mut_var(vars)..] => vars.collect(),
        ))
    }

    fn index_vars(input: Node) -> ParseResult<Vec<ast::InductionVar>> {
        Ok(match_nodes!(input.into_children();
            [index_var(vars)..] => vars.collect(),
        ))
    }

    fn fpcore_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn if_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn let_kw(input: Node) -> ParseResult<bool> {
        Ok(input.as_str().ends_with('*'))
    }

    fn while_kw(input: Node) -> ParseResult<bool> {
        Ok(input.as_str().ends_with('*'))
    }

    fn for_kw(input: Node) -> ParseResult<bool> {
        Ok(input.as_str().ends_with('*'))
    }

    fn tensor_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn tensor_star_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn cast_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn array_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn bang_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn digits_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn expr(input: Node) -> ParseResult<ast::Expression> {
        let span = ast::Span::from_node(&input);

        let kind = match_nodes!(input.into_children();
            [number(num)] => ast::ExprKind::Num(num),
            [constant(constant)] => ast::ExprKind::Const(constant),
            [symbol(sym)] => ast::ExprKind::Id(sym),
            [operation(op), expr(args)..] => {
                let args: Vec<_> = args.collect();

                let op = match op.kind {
                    ast::OpKind::Math(ast::MathOp::Sub) if args.len() == 1 => {
                        ast::Operation {
                            kind: ast::OpKind::Math(ast::MathOp::Neg),
                            ..op
                        }
                    }
                    _ => op,
                };

                ast::ExprKind::Op(op, args)
            },
            [if_kw(_), expr(cond), expr(true_branch), expr(false_branch)] => {
                ast::ExprKind::If {
                    cond: Box::new(cond),
                    true_branch: Box::new(true_branch),
                    false_branch: Box::new(false_branch),
                }
            },
            [let_kw(sequential), binding(bindings).., expr(body)] => {
                ast::ExprKind::Let {
                    bindings: bindings.collect(),
                    body: Box::new(body),
                    sequential,
                }
            },
            [while_kw(sequential), expr(cond), mut_vars(vars), expr(body)] => {
                ast::ExprKind::While {
                    cond: Box::new(cond),
                    vars,
                    body: Box::new(body),
                    sequential,
                }
            },
            [for_kw(sequential), index_vars(indices), mut_vars(vars), expr(body)] => {
                ast::ExprKind::For {
                    indices,
                    vars,
                    body: Box::new(body),
                    sequential,
                }
            },
            [tensor_kw(_), index_vars(indices), expr(body)] => {
                ast::ExprKind::Tensor {
                    indices,
                    body: Box::new(body),
                }
            },
            [tensor_star_kw(_), index_vars(indices), mut_vars(vars), expr(body)] => {
                ast::ExprKind::TensorStar {
                    indices,
                    vars,
                    body: Box::new(body),
                }
            },
            [cast_kw(_), expr(body)] => ast::ExprKind::Cast(Box::new(body)),
            [array_kw(_), expr(elems)..] => {
                ast::ExprKind::Array(elems.collect())
            },
            [annotation(props), expr(body)] => ast::ExprKind::Annotation {
                props,
                body: Box::new(body),
            },
        );

        Ok(ast::Expression {
            kind,
            uid: ast::NodeId::new(),
            span,
        })
    }

    fn number(input: Node) -> ParseResult<ast::Number> {
        let span = ast::Span::from_node(&input);

        let value = match_nodes!(input.into_children();
            [rational(value)] => value,
            [decnum(value)] => value,
            [hexnum(value)] => value,
            [digits_kw(_), mantissa((sign, mantissa)), exponent(exponent), dec_digits(base)] => {
                literals::rational_from_digits(sign, mantissa, exponent, base)
                    .unwrap()
            },
        );

        Ok(ast::Number { value, span })
    }

    fn annotation(input: Node) -> ParseResult<Vec<ast::Property>> {
        Ok(match_nodes!(input.into_children();
            [bang_kw(_), property(props)..] => props.collect(),
        ))
    }

    fn operation(input: Node) -> ParseResult<ast::Operation> {
        Ok(match_nodes!(input.into_children();
            [operator(op)] => op,
            [symbol(sym)] => ast::Operation {
                kind: ast::OpKind::FPCore(sym.id),
                span: sym.span,
            },
        ))
    }

    fn name_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn description_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn cite_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn precision_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn round_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn overflow_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn pre_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn spec_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn alt_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn math_lib_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn example_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn domain_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn impl_kw(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn property_name(input: Node) -> ParseResult<ast::Symbol> {
        Ok(match_nodes!(input.into_children();
            [symbol(name)] => name,
        ))
    }

    fn property(input: Node) -> ParseResult<ast::Property> {
        let span = ast::Span::from_node(&input);

        let kind = match_nodes!(input.into_children();
            [name_kw(_), string(name)] => ast::PropKind::Name(name),
            [description_kw(_), string(description)] => {
                ast::PropKind::Description(description)
            },
            [cite_kw(_), symbol(symbols)..] => {
                ast::PropKind::Cite(symbols.collect())
            },
            [precision_kw(_), precision(precision)] => {
                ast::PropKind::Precision(precision)
            },
            [round_kw(_), rounding(round)] => {
                ast::PropKind::Round(round.parse().unwrap())
            },
            [overflow_kw(_), overflow(overflow)] => {
                ast::PropKind::Overflow(overflow.parse().unwrap())
            },
            [pre_kw(_), expr(pre)] => ast::PropKind::Pre(pre),
            [spec_kw(_), expr(spec)] => ast::PropKind::Spec(spec),
            [alt_kw(_), expr(alt)] => ast::PropKind::Alt(alt),
            [math_lib_kw(_), symbol(lib)] => ast::PropKind::MathLib(lib),
            [example_kw(_), binding(bindings)..] => {
                ast::PropKind::Example(bindings.collect())
            },
            [domain_kw(_), number(left), number(right)] => {
                ast::PropKind::CalyxDomain(meta::CalyxDomain {
                    left,
                    right,
                })
            },
            [impl_kw(_), strategy(strategy)] => {
                ast::PropKind::CalyxImpl(strategy)
            },
            [property_name(name), data(data)] => {
                ast::PropKind::Unknown(name, data)
            },
        );

        Ok(ast::Property { kind, span })
    }

    fn data(input: Node) -> ParseResult<meta::Data> {
        Ok(match_nodes!(input.into_children();
            [symbol(sym)] => meta::Data::Symbol(sym),
            [number(num)] => meta::Data::Num(num),
            [string(s)] => meta::Data::Str(s),
            [data(data)..] => meta::Data::List(data.collect()),
        ))
    }

    fn float(input: Node) -> ParseResult<(u32, u32)> {
        Ok(match_nodes!(input.into_children();
            [e, nbits] => (parse_node(&e)?, parse_node(&nbits)?),
        ))
    }

    fn posit(input: Node) -> ParseResult<(u32, u32)> {
        Ok(match_nodes!(input.into_children();
            [es, nbits] => (parse_node(&es)?, parse_node(&nbits)?),
        ))
    }

    fn fixed(input: Node) -> ParseResult<(i32, u32)> {
        Ok(match_nodes!(input.into_children();
            [scale, nbits] => (parse_node(&scale)?, parse_node(&nbits)?),
        ))
    }

    fn precision_shorthand(input: Node<'_>) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn precision(input: Node) -> ParseResult<meta::Precision> {
        Ok(match_nodes!(input.into_children();
            [float((e, nbits))] => meta::Precision::Float { e, nbits },
            [posit((es, nbits))] => meta::Precision::Posit { es, nbits },
            [fixed((scale, nbits))] => meta::Precision::Fixed { scale, nbits },
            [precision_shorthand(s)] => {
                meta::Precision::from_shorthand(s).unwrap()
            },
        ))
    }

    fn rounding(input: Node<'_>) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn overflow(input: Node<'_>) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn lut(input: Node) -> ParseResult<u32> {
        match_nodes!(input.into_children();
            [size] => parse_node(&size),
        )
    }

    fn poly(input: Node) -> ParseResult<meta::CalyxImpl> {
        Ok(match_nodes!(input.into_children();
            [degree] => meta::CalyxImpl::Poly {
                degree: parse_node(&degree)?,
                error: None,
            },
            [degree, number(error)] => meta::CalyxImpl::Poly {
                degree: parse_node(&degree)?,
                error: Some(error),
            },
        ))
    }

    fn strategy(input: Node) -> ParseResult<meta::CalyxImpl> {
        Ok(match_nodes!(input.into_children();
            [lut(size)] => meta::CalyxImpl::Lut { size },
            [poly(strategy)] => strategy,
        ))
    }

    fn pm_opt(input: Node) -> ParseResult<bool> {
        Ok(!matches!(input.as_str(), "-"))
    }

    fn dot(_input: Node) -> ParseResult<()> {
        Ok(())
    }

    fn dec_digits(input: Node<'_>) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn hex_digits(input: Node<'_>) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn nonzero(input: Node<'_>) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn rational(input: Node) -> ParseResult<ast::Rational> {
        Ok(match_nodes!(input.into_children();
            [pm_opt(sign), dec_digits(numerator), nonzero(denominator)] => {
                literals::rational_from_ratio_str(
                    sign, numerator, denominator, 10,
                )
                .unwrap()
            },
        ))
    }

    fn dec_mantissa(input: Node<'_>) -> ParseResult<(&str, &str)> {
        Ok(match_nodes!(input.into_children();
            [dec_digits(integer)] => (integer, "0"),
            [dec_digits(integer), dot(_), dec_digits(fraction)] => {
                (integer, fraction)
            },
            [dot(_), dec_digits(fraction)] => ("0", fraction),
        ))
    }

    fn hex_mantissa(input: Node<'_>) -> ParseResult<(&str, &str)> {
        Ok(match_nodes!(input.into_children();
            [hex_digits(integer)] => (integer, "0"),
            [hex_digits(integer), dot(_), hex_digits(fraction)] => {
                (integer, fraction)
            },
            [dot(_), hex_digits(fraction)] => ("0", fraction),
        ))
    }

    fn mantissa(input: Node<'_>) -> ParseResult<(bool, &str)> {
        Ok(match_nodes!(input.into_children();
            [pm_opt(sign), dec_digits(digits)] => (sign, digits),
        ))
    }

    fn exponent(input: Node<'_>) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn decnum(input: Node) -> ParseResult<ast::Rational> {
        Ok(match_nodes!(input.into_children();
            [pm_opt(sign), dec_mantissa((integer, fraction)), exponent(exponent)] => {
                literals::rational_from_scientific_str(
                    sign, integer, fraction, 10, 10, exponent,
                )
                .unwrap()
            },
            [pm_opt(sign), dec_mantissa((integer, fraction))] => {
                literals::rational_from_fixed_point_str(
                    sign, integer, fraction, 10,
                )
                .unwrap()
            },
        ))
    }

    fn hexnum(input: Node) -> ParseResult<ast::Rational> {
        Ok(match_nodes!(input.into_children();
            [pm_opt(sign), hex_mantissa((integer, fraction)), exponent(exponent)] => {
                literals::rational_from_scientific_str(
                    sign, integer, fraction, 16, 2, exponent,
                )
                .unwrap()
            },
            [pm_opt(sign), hex_mantissa((integer, fraction))] => {
                literals::rational_from_fixed_point_str(
                    sign, integer, fraction, 16,
                )
                .unwrap()
            },
        ))
    }

    fn symbol(input: Node) -> ParseResult<ast::Symbol> {
        Ok(ast::Symbol {
            id: input.as_str().into(),
            span: ast::Span::from_node(&input),
        })
    }

    fn printable(input: Node<'_>) -> ParseResult<&str> {
        Ok(match input.as_str() {
            "\\\\" => "\\",
            "\\\"" => "\"",
            s => s,
        })
    }

    fn string(input: Node) -> ParseResult<String> {
        Ok(match_nodes!(input.into_children();
            [printable(c)..] => c.collect(),
        ))
    }

    fn mathematical_op(input: Node<'_>) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn testing_op(input: Node<'_>) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn tensor_op(input: Node<'_>) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn operator(input: Node) -> ParseResult<ast::Operation> {
        let span = ast::Span::from_node(&input);

        let kind = match_nodes!(input.into_children();
            [mathematical_op(name)] => ast::OpKind::Math(name.parse().unwrap()),
            [testing_op(name)] => ast::OpKind::Test(name.parse().unwrap()),
            [tensor_op(name)] => ast::OpKind::Tensor(name.parse().unwrap()),
        );

        Ok(ast::Operation { kind, span })
    }

    fn mathematical_const(input: Node<'_>) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn boolean_const(input: Node<'_>) -> ParseResult<&str> {
        Ok(input.as_str())
    }

    fn constant(input: Node) -> ParseResult<ast::Constant> {
        Ok(match_nodes!(input.into_children();
            [mathematical_const(name)] => {
                ast::Constant::Math(name.parse().unwrap())
            },
            [boolean_const(name)] => ast::Constant::Bool(match name {
                "TRUE" => true,
                "FALSE" => false,
                _ => unreachable!(),
            }),
        ))
    }
}

#[allow(clippy::result_large_err)]
fn parse_node<T>(input: &Node) -> ParseResult<T>
where
    T: FromStr,
    T::Err: ToString,
{
    input.as_str().parse().map_err(|err: T::Err| {
        Error::new_from_span(
            ErrorVariant::CustomError {
                message: err.to_string(),
            },
            input.as_span(),
        )
    })
}

impl ast::Span {
    fn from_node(node: &Node) -> ast::Span {
        let span = node.as_span();

        ast::Span::new(span.start(), span.end())
    }
}

impl From<InputLocation> for ast::Span {
    fn from(value: InputLocation) -> Self {
        let (start, end) = match value {
            InputLocation::Pos(pos) => (pos, pos + 1),
            InputLocation::Span((start, end)) => (start, end),
        };

        ast::Span::new(start, end)
    }
}
