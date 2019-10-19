use std::iter::Enumerate;
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};

use nom::branch::alt;
use nom::bytes::complete::{escaped, is_a, tag, take, take_until, take_while};
use nom::character::{
    complete::{alphanumeric1, char, digit1, line_ending, multispace0, none_of, one_of, space1},
    is_alphabetic, is_space,
};
use nom::combinator::{cut, map, map_res, verify};
use nom::error::{context, ErrorKind};
use nom::lib::std::str::FromStr;
use nom::sequence::{preceded, terminated};
use nom::Err;
use nom::{Compare, CompareResult, FindToken, IResult, InputIter, InputLength, InputTake, Slice};

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
    EOF,
    Illegal(&'a str),

    Id(&'a str),
    IntNumber(&'a str),
    LongNumber(&'a str),
    HexIntNumber(&'a str),
    HexLongNumber(&'a str),
    OctalIntNumber(&'a str),
    OctalLongNumber(&'a str),
    FloatNumber(&'a str),
    DoubleNumber(&'a str),

    Break,
    Class,
    Case,
    Default,
    Const,
    Continue,
    Do,
    If,
    Else,
    Enum,
    Extends,
    While,
    For,
    Function,
    Has,
    As,
    Using,
    Var,
    Module,
    ModuleAssign,
    New,

    Public,
    Private,
    Protected,

    Return,
    Hidden,
    Native,
    Static,
    Throw,
    Try,
    Catch,
    Finally,
    Switch,

    Me,
    Null,

    False,
    True,

    Inc,
    Dec,

    Arrow,

    Add,
    AddAssign,
    Sub,
    SubAssign,
    Mul,
    MulAssign,
    Div,
    DivAssign,

    And,
    BitAnd,
    BitAndAssign,
    Tilde,

    Or,
    BitOr,
    BitOrAssign,

    BitXor,
    BitXorAssign,

    Dot,

    // Punctuation
    Colon,
    Comma,
    Semicolon,

    Assign,
    InstanceOf,
    Equal,

    Distinct,
    Exclamation,
    Question,

    SLeftAssign,
    SLeft,
    LessEqual,
    Less,
    SRightAssign,
    SRight,
    GreaterEqual,
    Greater,

    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    CloseSquareB,

    StringLiteral(&'a str),
    CharacterLiteral(char),
    Bling,

    LineComment(&'a str),

    UnknownOperator(&'a str),
}

impl<'a> InputLength for Token<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a> Compare<Token<'a>> for Tokens<'a> {
    fn compare(&self, t: Token<'a>) -> CompareResult {
        if let Some(first) = self.iter_elements().next() {
            if *first == t {
                return CompareResult::Ok;
            }
        }
        CompareResult::Error
    }

    fn compare_no_case(&self, t: Token<'a>) -> CompareResult {
        self.compare(t)
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(C)]
pub struct Tokens<'a> {
    pub tok: &'a [Token<'a>],
    pub start: usize,
    pub end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(vec: &'a Vec<Token<'a>>) -> Self {
        Tokens {
            tok: vec.as_slice(),
            start: 0,
            end: vec.len(),
        }
    }

    pub fn pop_token<T: 'a>(
        self,
        extract: impl Fn(&'a Token<'a>) -> Option<T>,
    ) -> IResult<Self, T> {
        let (i1, t1) = take(1usize)(self)?;
        if t1.tok.is_empty() {
            Err(Err::Error((i1, ErrorKind::Tag)))
        } else {
            if let Some(r) = extract(&t1.tok[0]) {
                Ok((i1, r))
            } else {
                Err(Err::Error((i1, ErrorKind::Tag)))
            }
        }
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tok.len()
    }
}

impl<'a> InputTake for Tokens<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        Tokens {
            tok: &self.tok[0..count],
            start: 0,
            end: count,
        }
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tok.split_at(count);
        let first = Tokens {
            tok: prefix,
            start: 0,
            end: prefix.len(),
        };
        let second = Tokens {
            tok: suffix,
            start: 0,
            end: suffix.len(),
        };
        (second, first)
    }
}

impl<'a> Slice<Range<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: Range<usize>) -> Self {
        Tokens {
            tok: &self.tok[range.start..range.end],
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> Slice<RangeFrom<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a> Slice<RangeFull> for Tokens<'a> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        Tokens {
            tok: self.tok,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a Token<'a>;
    type Iter = Enumerate<::std::slice::Iter<'a, Token<'a>>>;
    type IterElem = ::std::slice::Iter<'a, Token<'a>>;

    #[inline]
    fn iter_indices(&self) -> Enumerate<::std::slice::Iter<'a, Token<'a>>> {
        self.tok.iter().enumerate()
    }

    #[inline]
    fn iter_elements(&self) -> ::std::slice::Iter<'a, Token<'a>> {
        self.tok.iter()
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tok.iter().position(|b| predicate(b))
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Option<usize> {
        if self.tok.len() >= count {
            Some(count)
        } else {
            None
        }
    }
}
