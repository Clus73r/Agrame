use core::fmt;


use nom::{IResult, multi::separated_list1, character::complete::{alpha1, space0, space1, tab, newline}, bytes::complete::{tag, take_until}, sequence::{tuple, delimited}, branch::alt, combinator::not};

use crate::grammar::{Grammar, ProductionBuilder};

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Clone)]
pub struct ParseError;

#[derive(Debug, PartialEq, Eq)]
struct ParsedProductions {
    productions: Vec<ParsedProduction>,
}

impl fmt::Display for ParsedProductions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Sequence: {}", self.productions.iter().fold("".to_string(), |s, e| s + &format!("{e}")))
    }
}

#[derive(Debug, PartialEq, Eq)]
struct ParsedProduction {
    lhs: ProductionExpression,
    rhs: ProductionExpression,
}

impl fmt::Display for ParsedProduction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Production:\n\tlhs: {}\n\trhs: {}", self.lhs, self.rhs)
    }
}

#[derive(Debug, PartialEq, Eq)]
enum ProductionExpression {
    NonTerminal(String),
    Terminal(String),
    Sequence(Vec<ProductionExpression>),
}

impl fmt::Display for ProductionExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProductionExpression::NonTerminal(val) => write!(f, "NonTerminal: {}", val),
            ProductionExpression::Terminal(val) => write!(f, "Terminal: {}", val),
            ProductionExpression::Sequence(seq) => {
                write!(f, "Sequence: {}", seq.iter().fold("".to_string(), |s, e| s + &format!("{e}")))
            }
        }
    }
}

fn parse_non_terminal(input: &str) -> IResult<&str, ProductionExpression> {
    let (rem, p) = alpha1(input)?;
    Ok((rem, ProductionExpression::NonTerminal(p.to_string())))
    // let (rem, p) = terminated(alpha1, space0)(input)?;
    // Ok((rem, ProductionExpression::NonTerminal(p.to_string())))
}

fn parse_arrow(input: &str) -> IResult<&str, &str> {
    delimited(space0, tag("->"), space0)(input)
}

fn parse_terminal(input: &str) -> IResult<&str, ProductionExpression> {
    let (rem, p) = delimited(
        nom::character::complete::char('"'),
        take_until("\""),
        nom::character::complete::char('"'))(input)?;
    Ok((rem, ProductionExpression::Terminal(p.to_string())))
}

fn parse_expression(input: &str) -> IResult<&str, ProductionExpression> {
    alt((
        parse_terminal,
        parse_non_terminal,
    ))(input)
}

fn parse_sequence(input: &str) -> IResult<&str, ProductionExpression> {
    let (rem, seq) = separated_list1(space1, parse_expression)(input)?;
    Ok((rem, ProductionExpression::Sequence(seq)))
}

fn parse_production(input: &str) -> IResult<&str, ParsedProduction> {
    let res = tuple((
        parse_non_terminal,
        parse_arrow,
        parse_sequence,
    ))(input);
    match res {
        Ok((rem, (id0, _, id1))) => Ok((rem, ParsedProduction{lhs: id0, rhs: id1})),
        Err(err) => Err(err),
    }
}

fn parse_productions(input: &str) -> IResult<&str, ParsedProductions> {
    let (rem, seq) = separated_list1(tuple((newline, not(tab))), parse_production)(input)?;
    Ok((rem, ParsedProductions{ productions: seq }))
}

fn into_grammar(parse: ParsedProductions) -> Grammar {
    let mut grammar = Grammar::new();

    fn process_expression(expr: &ProductionExpression, grammar: &mut Grammar) -> ProductionBuilder {
        match expr {
            ProductionExpression::NonTerminal(non_terminal) => {
                grammar.add_non_terminal(non_terminal);
                ProductionBuilder::NonTerminal(non_terminal.to_string())
            },
            ProductionExpression::Terminal(terminal) => {
                grammar.add_terminal(terminal);
                ProductionBuilder::Terminal(terminal.to_string())
            },
            ProductionExpression::Sequence(seq) => {
                let exprs: Vec<ProductionBuilder> = seq.iter().map(|e| process_expression(e, grammar)).collect();
                ProductionBuilder::Sequence(exprs)
                // for production_expression in seq {
                //     process_expression(production_expression, grammar);
                // }
                // ProductionBuilder::Sequence(())
            },
        }
    }

    for parsed_production in parse.productions {
        if let ProductionExpression::NonTerminal(lhs) = parsed_production.lhs {
            grammar.add_non_terminal(&lhs);
            let prod = process_expression(&parsed_production.rhs, &mut grammar);
            let _ = grammar.add_production(&lhs, prod);
        }
        else{
            panic!("Faulty grammar");
        }
    }

    grammar
}

pub fn parse(input: &str) -> Result<Grammar> {
    let p = parse_productions(input);
    match p {
        Ok((_rem, val)) => Ok(into_grammar(val)),
        Err(_) => Err(ParseError),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_non_terminal0() {
        assert_eq!(super::parse_non_terminal("abc"),
            IResult::Ok(("", ProductionExpression::NonTerminal("abc".to_string()))));
    }

    #[test]
    fn parse_non_terminal1() {
        assert_eq!(super::parse_non_terminal("abc kfjd"),
            IResult::Ok((" kfjd", ProductionExpression::NonTerminal("abc".to_string()))));
    }

    #[test]
    fn parse_non_terminal2() {
        assert!(super::parse_non_terminal("").is_err());
    }

    #[test]
    fn parse_terminal0() {
        assert_eq!(super::parse_terminal("\"abc\""),
            IResult::Ok(("", ProductionExpression::Terminal("abc".to_string()))));
    }

    #[test]
    fn parse_terminal1() {
        assert_eq!(super::parse_terminal("\"abc\" bca"),
            IResult::Ok((" bca", ProductionExpression::Terminal("abc".to_string()))));
    }

    #[test]
    fn parse_terminal2() {
        assert!(super::parse_terminal("abc bca").is_err());
    }

    #[test]
    fn parse_expression0() {
        assert_eq!(super::parse_expression("abc"),
            IResult::Ok(("", ProductionExpression::NonTerminal("abc".to_string()))));
    }

    #[test]
    fn parse_expression1() {
        assert_eq!(super::parse_expression("\"abc\""),
            IResult::Ok(("", ProductionExpression::Terminal("abc".to_string()))));
    }

    #[test]
    fn parse_production0() {
        assert_eq!(super::parse_production("a->a"),
            IResult::Ok(("", ParsedProduction{
                lhs: ProductionExpression::NonTerminal("a".to_string()),
                rhs: ProductionExpression::Sequence(vec![
                    ProductionExpression::NonTerminal("a".to_string()),
                ])
            })));
    }

    #[test]
    fn parse_production1() {
        assert_eq!(super::parse_production("a -> a b"),
            IResult::Ok(("", ParsedProduction{
                lhs: ProductionExpression::NonTerminal("a".to_string()),
                rhs: ProductionExpression::Sequence(vec![
                    ProductionExpression::NonTerminal("a".to_string()),
                    ProductionExpression::NonTerminal("b".to_string()),
                ])
            })));
    }

    #[test]
    fn parse_production2() {
        assert_eq!(super::parse_production("a -> \"a\" b"),
            IResult::Ok(("", ParsedProduction{
                lhs: ProductionExpression::NonTerminal("a".to_string()),
                rhs: ProductionExpression::Sequence(vec![
                    ProductionExpression::Terminal("a".to_string()),
                    ProductionExpression::NonTerminal("b".to_string()),
                ])
            })));
    }

    #[test]
    fn parse_productions0() {
        assert_eq!(super::parse_productions("a -> a b\nb -> \"b\""),
            IResult::Ok(("", ParsedProductions {
                productions: vec![
                    ParsedProduction {
                        lhs: ProductionExpression::NonTerminal("a".to_string()),
                        rhs: ProductionExpression::Sequence(vec![
                            ProductionExpression::NonTerminal("a".to_string()),
                            ProductionExpression::NonTerminal("b".to_string()),
                        ])
                    },
                    ParsedProduction {
                        lhs: ProductionExpression::NonTerminal("b".to_string()),
                        rhs: ProductionExpression::Sequence(vec![
                            ProductionExpression::Terminal("b".to_string()),
                        ])
                    },
                ]
            })));
    }
}
