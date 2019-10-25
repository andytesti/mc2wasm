use std::iter::Enumerate;
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};

use nom::branch::alt;
use nom::bytes::complete::{escaped, tag, tag_no_case, take, take_while};
use nom::bytes::complete::{is_a, take_until};
use nom::character::complete::{
    alpha0, alphanumeric0, alphanumeric1, char, digit0, digit1, hex_digit1, line_ending,
    multispace0, multispace1, none_of, oct_digit0, oct_digit1, one_of, space1,
};
use nom::character::{is_alphabetic, is_hex_digit, is_space};
use nom::combinator::{cut, iterator, map, map_res, opt, recognize, verify};
use nom::error::{context, ErrorKind};
use nom::lib::std::str::FromStr;
use nom::multi::{many1, separated_list};
use nom::sequence::{pair, preceded, terminated, tuple};
use nom::{
    AsChar, FindToken, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Slice,
};

use crate::lexer::token::Token;
use nom::number::complete::recognize_float;

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
        sym("{", Token::OpenBrace),
        sym("}", Token::CloseBrace),
        sym("[", Token::OpenBracket),
        sym("]b", Token::CloseBracketB),
        sym("]", Token::CloseBracket),
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
        sym("^=", Token::BitXorAssign),
        sym("^", Token::BitXor),
        sym("$", Token::Bling),
        sym("~", Token::Tilde),
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
        sym("%=", Token::ModuleAssign),
        sym("%", Token::Module),
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

pub fn ident_char(input: Input) -> Result<Input> {
    // TODO add more specific error
    input.split_at_position1_complete(
        |item| !(item.is_alphanum() || item == '_' || item >= '\u{0080}' && item <= '\u{fffe}'),
        ErrorKind::Alpha,
    )
}

// Id: ([a-zA-Z_]|'\u0080'..'\ufffe')([a-zA-Z0-9_]|'\u0080'..'\ufffe')*
fn parse_word(i: Input) -> Result<Input> {
    verify(ident_char, |s: &str| {
        s.chars().next().filter(|c| !c.is_numeric()).is_some()
    })(i)
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
        "has" => Token::Has,
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
        _ => Token::Id(s),
    })(i)
}

fn illegal(i: Input) -> Result<Token> {
    map(take(1usize), Token::Illegal)(i)
}

fn long_sigil(i: Input) -> Result<char> {
    one_of("lL")(i)
}

fn is_long(i: Input) -> Result<bool> {
    map(opt(long_sigil), |long| long.is_some())(i)
}

fn positive_digit(i: Input) -> Result<char> {
    one_of("123456789")(i)
}

// IntNumber: [0][lL]? |([1-9][0-9]*)[lL]? ;
fn int_number(i: Input) -> Result<Token> {
    let long_zero = map(terminated(tag("0"), long_sigil), Token::LongNumber);
    let digits = recognize(pair(positive_digit, digit0));
    let positive = map(pair(digits, is_long), |(number, long)| {
        if long {
            Token::LongNumber(number)
        } else {
            Token::IntNumber(number)
        }
    });
    alt((long_zero, positive))(i)
}

// HexNumber: '0x'[0-9a-fA-F]+[lL]?;
fn hex_number(i: Input) -> Result<Token> {
    let digits = preceded(tag("0x"), hex_digit1);
    map(pair(digits, is_long), |(n, long)| {
        if long {
            Token::HexLongNumber(n)
        } else {
            Token::HexIntNumber(n)
        }
    })(i)
}

// OctalNumber: [0][0-9]*[lL]?;
fn octal_number(i: Input) -> Result<Token> {
    map(
        preceded(char('0'), pair(oct_digit1, is_long)),
        |(n, long)| {
            if long {
                Token::OctalLongNumber(n)
            } else {
                Token::OctalIntNumber(n)
            }
        },
    )(i)
}

fn float_exponent(i: Input) -> Result<Input> {
    recognize(pair(opt(pair(one_of("Ee"), one_of("+-"))), digit1))(i)
}

fn float_sigil(i: Input) -> Result<char> {
    one_of("fd")(i)
}

fn is_double_sigil(c: char) -> bool {
    c == 'd'
}

fn is_double(i: Input) -> Result<bool> {
    map(opt(float_sigil), |c| {
        c.filter(|c| is_double_sigil(*c)).is_some()
    })(i)
}

fn float_token((number, sigil): (Input, char)) -> Token {
    if is_double_sigil(sigil) {
        Token::DoubleNumber(number)
    } else {
        Token::FloatNumber(number)
    }
}

fn fraction(i: Input) -> Result<Input> {
    recognize(tuple((char('.'), digit1, opt(float_exponent))))(i)
}

//FloatNumber
//: ( ([0])('f'|'d')? | ([1-9][0-9]*))([\\.][0-9]+)?FloatExponent?('f'|'d')?
//| [\\.] [0-9]+ FloatExponent?('f'|'d')?
fn float_number(i: Input) -> Result<Token> {
    //fragment FloatExponent: [Ee][+-]?[0-9]+;
    let zero_token = map(pair(tag("0"), float_sigil), float_token);
    let positive = recognize(pair(digit1, fraction));
    let positive_token = map(
        pair(alt((positive, fraction)), is_double),
        |(number, double)| {
            if double {
                Token::DoubleNumber(number)
            } else {
                Token::FloatNumber(number)
            }
        },
    );
    alt((zero_token, positive_token))(i)
}

fn number(i: Input) -> Result<Token> {
    alt((float_number, int_number, hex_number, octal_number))(i)
}

fn token(i: Input) -> Result<Token> {
    preceded(
        multispace0,
        alt((
            line_comment,
            number,
            delimiter,
            string_literal,
            operator,
            word,
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

#[cfg(test)]
mod tests {
    use nom::combinator::iterator;

    use super::*;
    use token::Token::*;

    #[test]
    fn parse_ident() {
        assert_eq!(
            lex_tokens("ident ident1 _ident"),
            Ok(("", vec![Id("ident"), Id("ident1"), Id("_ident")]))
        );
    }

    #[test]
    fn parse_numbers() {
        assert_eq!(
            lex_tokens("12 15L 0x40 0x12FL 0234 012l 0f 0d .12 12.0E-12d 0.14f"),
            Ok((
                "",
                vec![
                    IntNumber("12"),
                    LongNumber("15"),
                    HexIntNumber("40"),
                    HexLongNumber("12F"),
                    OctalIntNumber("234"),
                    OctalLongNumber("12"),
                    FloatNumber("0"),
                    DoubleNumber("0"),
                    FloatNumber(".12"),
                    DoubleNumber("12.0E-12"),
                    FloatNumber("0.14")
                ]
            ))
        );
    }

    #[test]
    fn parse_ternary() {
        assert_eq!(
            lex_tokens("ident_1?_2ident: _"),
            Ok((
                "",
                vec![Id("ident_1"), Question, Id("_2ident"), Colon, Id("_")]
            ))
        );
    }
}
