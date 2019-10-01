use std::iter::Enumerate;
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};

use nom::branch::alt;
use nom::bytes::complete::{escaped, tag, take, take_while};
use nom::bytes::complete::{is_a, take_until};
use nom::character::complete::{
    alphanumeric1, char, digit1, line_ending, multispace0, multispace1, none_of, one_of, space1,
};
use nom::character::{is_alphabetic, is_space};
use nom::combinator::{cut, iterator, map, map_res, verify};
use nom::error::context;
use nom::lib::std::str::FromStr;
use nom::multi::separated_list;
use nom::sequence::{preceded, terminated};
use nom::{FindToken, IResult, InputIter, InputLength, InputTake, Slice};

use crate::lexer::token::Token;

pub mod token;

type Input<'a> = &'a str;
type Error = ();
type Result<'a, Output> = IResult<Input<'a>, Output>;

fn sym<'a>(s: &'static str, t: Token<'a>) -> impl Fn(Input<'a>) -> Result<Token<'a>> {
    map(tag(s), move |_| t.clone())
}

fn delimiter(i: Input) -> Result<Token> {
    alt((
        sym("(", Token::OpenParen),
        sym(")", Token::CloseParen),
        sym("{", Token::OpenBracket),
        sym("}", Token::CloseBracket),
        sym("[", Token::OpenSquare),
        sym("]", Token::CloseSquare),
        sym(",", Token::Comma),
        sym(";", Token::Semicolon),
        sym(":", Token::Colon),
        sym("?", Token::Question),
        sym(".", Token::Dot),
    ))(i)
}

fn parse_str(i: Input) -> Result<Input> {
    escaped(none_of(r#""\"#), '\\', one_of(r#""'n\"#))(i)
}

fn string_literal(i: Input) -> Result<Token> {
    map(
        context(
            "string",
            preceded(char('\"'), cut(terminated(parse_str, char('\"')))),
        ),
        Token::StringLiteral,
    )(i)
}

fn logical(i: Input) -> Result<Token> {
    alt((
        sym("&=", Token::BitAndAssign),
        sym("&&", Token::And),
        sym("&", Token::BitAnd),
        sym("|=", Token::BitOrAssign),
        sym("||", Token::Or),
        sym("|", Token::BitOr),
    ))(i)
}

fn relational(i: Input) -> Result<Token> {
    alt((
        sym("!=", Token::Distinct),
        sym("!", Token::Exclamation),
        sym("=>", Token::Arrow),
        sym("==", Token::Equal),
        sym("=", Token::Assign),
        sym("<<=", Token::SLeftAssign),
        sym("<<", Token::SLeft),
        sym("<=", Token::LessEqual),
        sym("<", Token::Less),
        sym(">>=", Token::SRightAssign),
        sym(">>", Token::SRight),
        sym(">=", Token::GreaterEqual),
        sym(">", Token::Greater),
    ))(i)
}

fn arithmetical(i: Input) -> Result<Token> {
    alt((
        sym("++", Token::Inc),
        sym("+=", Token::AddAssign),
        sym("+", Token::Add),
        sym("--", Token::Dec),
        sym("-=", Token::SubAssign),
        sym("-", Token::Sub),
        sym("*=", Token::MulAssign),
        sym("*", Token::Mul),
        sym("/=", Token::DivAssign),
        sym("/", Token::Div),
    ))(i)
}

fn operator(i: Input) -> Result<Token> {
    alt((logical, relational, arithmetical))(i)
}

fn line_comment(i: Input) -> Result<Token> {
    map(
        preceded(tag("//"), take_while(|c| c != '\n')),
        Token::LineComment,
    )(i)
}

fn parse_word(i: Input) -> Result<Input> {
    verify(alphanumeric1, |s: &str| {
        s.chars().next().filter(|c| !c.is_numeric()).is_some()
    })(i)
}

fn symbol(i: Input) -> Result<Token> {
    map(preceded(tag(":"), parse_word), Token::Symbol)(i)
}

fn word(i: Input) -> Result<Token> {
    map(parse_word, |s| match s {
        "and" => Token::And,
        "as" => Token::As,
        "break" => Token::Break,
        "case" => Token::Case,
        "class" => Token::Class,
        "const" => Token::Const,
        "continue" => Token::Continue,
        "default" => Token::Default,
        "do" => Token::Do,
        "if" => Token::If,
        "else" => Token::Else,
        "extends" => Token::Extends,
        "enum" => Token::Enum,
        "false" => Token::False,
        "for" => Token::For,
        "function" => Token::Function,
        "instanceof" => Token::InstanceOf,
        "me" | "self" => Token::Me,
        "null" => Token::Null,
        "or" => Token::Or,
        "public" => Token::Public,
        "private" => Token::Private,
        "protected" | "hidden" => Token::Protected,
        "module" => Token::Module,
        "new" => Token::New,
        "return" => Token::Return,
        "static" => Token::Static,
        "switch" => Token::Switch,
        "true" => Token::True,
        "throw" => Token::Throw,
        "try" => Token::Try,
        "catch" => Token::Catch,
        "finally" => Token::Finally,
        "using" => Token::Using,
        "var" => Token::Var,
        "while" => Token::While,
        _ => Token::Ident(s),
    })(i)
}

fn illegal(i: Input) -> Result<Token> {
    map(take(1usize), Token::Illegal)(i)
}

fn int_literal(i: Input) -> Result<Token> {
    map(map_res(digit1, i64::from_str), Token::IntLiteral)(i)
}

fn token(i: Input) -> Result<Token> {
    preceded(
        multispace0,
        alt((
            line_comment,
            symbol,
            delimiter,
            string_literal,
            operator,
            word,
            int_literal,
            illegal,
        )),
    )(i)
}

fn lex_tokens(i: Input) -> Result<Vec<Token>> {
    let mut it = iterator(i, token);
    let tokens = it
        // remove all comments
        .filter(|t| {
            if let Token::LineComment(_) = t {
                false
            } else {
                true
            }
        })
        .collect::<Vec<Token>>();
    let f = it.finish();
    f.map(|(o, _)| (o, tokens))
}

pub struct Lexer {}

impl Lexer {
    pub fn tokenize(i: Input) -> Result<Vec<Token>> {
        lex_tokens(i)
    }
}

mod tests {
    use nom::combinator::iterator;

    use super::*;

    #[test]
    fn parse_ident() {
        let code = r#"
using Toybox.Lang as Lang;

module Foo
{
    function operation() {
        // Do something
    }
}
function moduleSample() {
    var v = new Lang.Method(Foo, "hello");
    var sym = a == b? x : :ImASymbol;
    v.invoke();
}
        "#;

        assert_eq!(lex_tokens(code), Ok(("nice", vec![])));
    }
}
