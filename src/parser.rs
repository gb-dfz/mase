use nom::{
    branch::alt,
    combinator::{map, cut, value},
    multi::many0,
    character::complete::{alpha1, multispace0, multispace1, char},
    bytes::complete::{escaped_transform, is_not, tag},
    sequence::{delimited, preceded, terminated},
    IResult,
    error::{VerboseError, context},
    Parser,
};

#[derive(Debug, Eq, PartialEq)]
enum Atom {
    Symbol(String),
    String(String),
    Var(String),
}

#[derive(Debug, Eq, PartialEq)]
enum Element {
    Atom(Atom),
    List(Box<List>),
}

type List = Vec<Element>;

fn parse_word(i: &str) -> IResult<&str, Atom, VerboseError<&str>> {
    map(alpha1,
        |w: &str| Atom::Symbol(w.to_string()),
    ).parse(i)
}

fn parse_string(i: &str) -> IResult<&str, Atom, VerboseError<&str>> {
    map(delimited(
            char('"'),
            context("inner of string",cut(
            escaped_transform(
                is_not("\\\""),
                '\\',
                alt((
                    value("\\", tag("\\")),
                    value("\"", tag("\"")),
                ))
            ))),
            context("closing double quote", cut(char('"'))),
        ),
        |s: String| Atom::String(s),
    ).parse(i)
}

fn parse_atom(i: &str) -> IResult<&str, Atom, VerboseError<&str>> {
    alt((parse_word, parse_string))
        .parse(i)
}

fn parse_element(i: &str) -> IResult<&str, Element, VerboseError<&str>> {
    alt((map(parse_atom,
             |a: Atom| Element::Atom(a)),
         map(parse_list,
             |l: List| Element::List(Box::new(l))),
    )).parse(i)
}

fn parse_list(i: &str) -> IResult<&str, List, VerboseError<&str>> {
    delimited(
        char('('),
        preceded(multispace0, 
            context("inner of list",
                    cut(many0(terminated(parse_element, multispace0)))),
        ),
        context("closing parenthesis", cut(char(')'))),
    )(i)
}

fn parse_lists(i: &str) -> IResult<&str, Vec<List>, VerboseError<&str>> {
    preceded(multispace0, many0(terminated(parse_list, multispace0)))(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn word1() {
        assert_eq!(parse_word("abc"), Ok(("", Atom::Symbol("abc".to_string()))));
    }

    #[test]
    fn word2() {
        assert_eq!(parse_word("abc def"), Ok((" def", Atom::Symbol("abc".to_string()))));
    }

    #[test]
    fn string1() {
        assert_eq!(parse_string("\"abc def\"ghi"), Ok(("ghi", Atom::String("abc def".to_string()))));
    }

    #[test]
    fn string2() {
        assert_eq!(parse_string("\"abc \\\"def\"ghi"), Ok(("ghi", Atom::String("abc \"def".to_string()))));
    }

    #[test]
    fn list() {
        assert_eq!(
            parse_list("(ab (\nc d e ) fg \"abc\\\\\" )"),
            Ok(("", vec![Element::Atom(Atom::Symbol("ab".to_string())),
                         Element::List(Box::new(vec![Element::Atom(Atom::Symbol("c".to_string())),
                                                     Element::Atom(Atom::Symbol("d".to_string())),
                                                     Element::Atom(Atom::Symbol("e".to_string()))])),
                         Element::Atom(Atom::Symbol("fg".to_string())),
                         Element::Atom(Atom::String("abc\\".to_string()))]
            )),
        );
    }

    #[test]
    fn lists() {
        assert_eq!(
            parse_lists("  (ab (\nc e ) fg \"abc\\\\\" )\n  \t(ab (\nc e ))"),
            Ok(("", vec![vec![Element::Atom(Atom::Symbol("ab".to_string())),
                              Element::List(Box::new(vec![Element::Atom(Atom::Symbol("c".to_string())),
                                                          Element::Atom(Atom::Symbol("e".to_string()))])),
                              Element::Atom(Atom::Symbol("fg".to_string())),
                              Element::Atom(Atom::String("abc\\".to_string()))],
                         vec![Element::Atom(Atom::Symbol("ab".to_string())),
                              Element::List(Box::new(vec![Element::Atom(Atom::Symbol("c".to_string())),
                                                          Element::Atom(Atom::Symbol("e".to_string()))]))],
                        ]
            )),
        );
    }
}
