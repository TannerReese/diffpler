use std::collections::HashMap;
use std::fmt::Debug;
use std::str::FromStr;

use pest::pratt_parser::{Op, PrattParser};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

use crate::diff_eq::{Binary, Eqns, Expr, Unary, Var};

// Pratt Parser for parsing expressions with precedence
lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::Assoc::{Left, Right};
        use Rule::{add, div, flrdiv, max, min, mul, neg, pow, rem, sub};
        PrattParser::new()
            .op(Op::infix(max, Left))
            .op(Op::infix(min, Left))
            .op(Op::infix(add, Left) | Op::infix(sub, Left))
            .op(Op::infix(mul, Left)
                | Op::infix(div, Left)
                | Op::infix(rem, Left)
                | Op::infix(flrdiv, Left))
            .op(Op::infix(pow, Right))
            .op(Op::prefix(neg))
    };
}

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct SysParser;

fn const_from_str(name: &str) -> f64 {
    match name {
        "pi" => std::f64::consts::PI,
        "e" => std::f64::consts::E,
        _ => unreachable!(),
    }
}

fn unary_from_str(name: &str) -> Unary {
    match name {
        "sqrt" => |x| x.sqrt(),
        "cbrt" => |x| x.cbrt(),
        "abs" => |x| x.abs(),
        "frac" => |x| x - x.floor(),
        "floor" => |x| x.floor(),
        "ceil" => |x| x.ceil(),
        "round" => |x| x.round(),
        "sin" => |x| x.sin(),
        "cos" => |x| x.cos(),
        "tan" => |x| x.tan(),
        "sec" => |x| x.cos().recip(),
        "csc" => |x| x.sin().recip(),
        "cot" => |x| x.tan().recip(),
        "asin" => |x| x.asin(),
        "acos" => |x| x.acos(),
        "atan" => |x| x.atan(),
        "asec" => |x| x.recip().acos(),
        "acsc" => |x| x.recip().asin(),
        "acot" => |x| x.recip().atan(),
        "sinh" => |x| x.sinh(),
        "cosh" => |x| x.cosh(),
        "tanh" => |x| x.tanh(),
        "asinh" => |x| x.asinh(),
        "acosh" => |x| x.acosh(),
        "atanh" => |x| x.atanh(),
        "exp" => |x| x.exp(),
        "ln" => |x| x.ln(),
        "log2" => |x| x.log2(),
        "log10" => |x| x.log10(),
        _ => unreachable!(),
    }
}

fn binary_from_str(name: &str) -> Binary {
    match name {
        "atan2" => |y, x| y.atan2(x),
        "log" => |base, x| x.log(base),
        _ => unreachable!(),
    }
}

fn split<const N: usize>(pair: Pair<Rule>) -> [Pair<Rule>; N] {
    pair.into_inner()
        .collect::<Vec<Pair<Rule>>>()
        .try_into()
        .unwrap()
}

fn parse_var(var: Pair<Rule>) -> Var {
    let [name, order] = split(var);
    Var {
        name: name.as_str().into(),
        order: order.as_str().len(),
    }
}

fn parse_expr(expr: Pair<Rule>) -> Expr {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::expr => parse_expr(primary),
            Rule::cnst => Expr::Const(const_from_str(primary.as_str())),
            Rule::func => {
                let [name, arg] = split(primary);
                let unary = unary_from_str(name.as_str());
                let boxed_exp = Box::new(parse_expr(arg));
                Expr::Unary(unary, boxed_exp)
            }
            Rule::func2 => {
                let [name, arg1, arg2] = split(primary);
                let binary = binary_from_str(name.as_str());
                let boxed_exp1 = Box::new(parse_expr(arg1));
                let boxed_exp2 = Box::new(parse_expr(arg2));
                Expr::Binary(binary, boxed_exp1, boxed_exp2)
            }
            Rule::literal => Expr::Const(primary.as_str().parse().unwrap()),
            Rule::var => Expr::Var(parse_var(primary)),
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::neg => -rhs,
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::max => lhs.apply2(|x, y| x.max(y), rhs),
            Rule::min => lhs.apply2(|x, y| x.min(y), rhs),
            Rule::add => lhs + rhs,
            Rule::sub => lhs - rhs,
            Rule::mul => lhs * rhs,
            Rule::div => lhs / rhs,
            Rule::rem => lhs % rhs,
            Rule::flrdiv => lhs.apply2(|x, y| (x / y).floor(), rhs),
            Rule::pow => lhs.apply2(|x, y| x.powf(y), rhs),
            _ => unreachable!(),
        })
        .parse(expr.into_inner())
}

pub type ParseError = pest::error::Error<Rule>;

impl FromStr for Var {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(parse_var(SysParser::parse(Rule::var, s)?.next().unwrap()))
    }
}

impl FromStr for Eqns {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut pairs = SysParser::parse(Rule::system, s)?;
        let system = pairs.next().unwrap();
        let eqns = system
            .into_inner()
            .filter_map(|eqn| {
                if let Rule::eqn = eqn.as_rule() {
                    let [var, expr] = split(eqn);
                    Some((parse_var(var), parse_expr(expr)))
                } else {
                    None
                }
            })
            .collect::<HashMap<Var, Expr>>();
        Ok(Eqns(eqns))
    }
}
