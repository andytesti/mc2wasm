use super::{Input, Result};
use crate::lexer::token::Token;
use crate::parser::ast::*;
use crate::parser::{between_brackets, between_parens, between_squares};
use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::{cut, map, map_opt, map_res, opt};
use nom::error::{context, ErrorKind};
use nom::lib::std::str::FromStr;
use nom::multi::{fold_many0, separated_list};
use nom::sequence::{delimited, pair, preceded, separated_pair, tuple};
use std::path::Component::Prefix;
use std::process::exit;

fn string_lit(i: Input) -> Result<&str> {
    i.pop_token(|t| match t {
        Token::StringLiteral(s) => Some(*s),
        _ => None,
    })
}

pub fn symbol_ref_lit(i: Input) -> Result<Id> {
    preceded(tag(Token::Colon), symbol)(i)
}

pub fn symbol(i: Input) -> Result<Id> {
    i.pop_token(|t| match t {
        Token::Id(v) => Some(*v),
        _ => None,
    })
}

fn number_lit(i: Input) -> Result<i64> {
    i.pop_token(|t| match t {
        // TODO use parsing error and distinguish long from int, hex and oct
        Token::IntNumber(i) => i64::from_str(i).ok(),
        _ => None,
    })
}

fn bool_lit(i: Input) -> Result<bool> {
    alt((
        map(tag(Token::False), |_| false),
        map(tag(Token::True), |_| true),
    ))(i)
}
// literal
//    : symbol
//    | symbolref
//    | number
//    | string
//    | bool
//    | nullReference
//    | character
//    | BlingToken
pub fn literal(i: Input) -> Result<Literal> {
    alt((
        map(symbol, Literal::Symbol),
        map(symbol_ref_lit, Literal::SymbolRef),
        map(number_lit, Literal::Integer),
        map(string_lit, Literal::String),
        map(bool_lit, Literal::Boolean),
        map(tag(Token::Null), |_| Literal::Null),
        map(tag(Token::Bling), |_| Literal::Global),
        map(tag(Token::Me), |_| Literal::Me),
    ))(i)
}

// invocation
//    : '(' argc+=expression (',' argc+=expression)* ')'
//    | '(' ')'
fn invocation(i: Input) -> Result<Vec<Expr>> {
    between_parens(separated_list(tag(Token::Comma), expr))(i)
}

pub fn call(i: Input) -> Result<CallExpr> {
    map(pair(symbol, invocation), |(name, arguments)| CallExpr {
        name,
        arguments,
    })(i)
}

// op='{' (keyValuePair ',')* keyValuePair? '}'
fn dictionary_creation(i: Input) -> Result<Expr> {
    // keyValuePair
    //    : key=expression '=>' value=expression
    let key_value_pair = separated_pair(expr, tag(Token::Arrow), expr);
    map(
        between_brackets(separated_list(tag(Token::Comma), key_value_pair)),
        Expr::NewDictionary,
    )(i)
}

// op='[' (expression ',')* expression? ']'
fn array_creation(i: Input) -> Result<Expr> {
    map(
        between_squares(separated_list(tag(Token::Comma), expr)),
        Expr::NewArray,
    )(i)
}

// op='[' (expression ',')* expression? ']b'
fn byte_array_creation(i: Input) -> Result<Expr> {
    map(
        delimited(
            tag(Token::OpenSquare),
            separated_list(tag(Token::Comma), expr),
            tag(Token::CloseSquareB),
        ),
        Expr::NewByteArray,
    )(i)
}

// op='new' '[' expression ']'
fn empty_array_creation(i: Input) -> Result<Expr> {
    map(preceded(tag(Token::New), between_squares(expr)), |size| {
        Expr::NewEmptyArray(Box::new(size))
    })(i)
}

// op='new' '[' expression ']b'
fn empty_byte_array_creation(i: Input) -> Result<Expr> {
    map(
        preceded(
            tag(Token::New),
            delimited(tag(Token::OpenSquare), expr, tag(Token::CloseSquareB)),
        ),
        |size| Expr::NewEmptyByteArray(Box::new(size)),
    )(i)
}

#[derive(Debug, PartialEq, Clone)]
enum Scope {
    Local,
    Global,
}

#[derive(Debug, PartialEq, Clone)]
enum Lvalue<'a> {
    Select {
        scope: Scope,
        name: Id<'a>,
        extension: Option<LvalueExtension<'a>>,
    },
    Invoke {
        scope: Scope,
        name: Id<'a>,
        arguments: Vec<Expr<'a>>,
        extension: LvalueExtension<'a>,
    },
}

/* lValues are the left hand value of an assignment */
//lvalue
//    : (BlingToken op='.')? symbol lvalueExtension?
//    | (BlingToken op='.')? symbol invocation lvalueExtension
fn lvalue(i: Input) -> Result<Lvalue> {
    use Lvalue::*;
    fn scope(i: Input) -> Result<Scope> {
        map(opt(preceded(tag(Token::Dot), tag(Token::Bling))), |g| {
            if g.is_some() {
                Scope::Global
            } else {
                Scope::Local
            }
        })(i)
    }
    let select = map(
        tuple((scope, symbol, opt(lvalue_extension))),
        |(scope, name, extension)| Select {
            scope,
            name,
            extension,
        },
    );
    let invoke = map(
        tuple((scope, symbol, invocation, lvalue_extension)),
        |(scope, name, arguments, extension)| Invoke {
            scope,
            name,
            arguments,
            extension,
        },
    );
    alt((invoke, select))(i)
}

#[derive(Debug, PartialEq, Clone)]
enum LvalueExtension<'a> {
    Select(Id<'a>, Option<Box<LvalueExtension<'a>>>),
    Index(Expr<'a>, Option<Box<LvalueExtension<'a>>>),
    Invoke(Id<'a>, Vec<Expr<'a>>, Box<LvalueExtension<'a>>),
}

// lvalueExtension
//    : op='.' symbol lvalueExtension?
//    | op='[' expression ']' lvalueExtension?
//    | op='.' symbol invocation lvalueExtension
fn lvalue_extension(i: Input) -> Result<LvalueExtension> {
    use LvalueExtension::*;
    let select = map(
        preceded(tag(Token::Dot), pair(symbol, opt(lvalue_extension))),
        |(sym, tail)| Select(sym, tail.map(Box::new)),
    );
    let index = map(
        pair(between_squares(expr), opt(lvalue_extension)),
        |(idx, tail)| Index(idx, tail.map(Box::new)),
    );
    let invoke = map(
        preceded(
            tag(Token::Dot),
            tuple((symbol, invocation, lvalue_extension)),
        ),
        |(field, args, tail)| Invoke(field, args, Box::new(tail)),
    );
    alt((invoke, select, index))(i)
}

/* lValues are the left hand value of an assignment */
//lvalue
//    : (BlingToken op='.')? symbol lvalueExtension?
//    | (BlingToken op='.')? symbol invocation lvalueExtension
//    ;

//creation
//    : op='new' lvalue invocation
//    | op='new' '[' expression ']'
//    | op='new' '[' expression ']b'
//    | op='[' (expression ',')* expression? ']'
//    | op='[' (expression ',')* expression? ']b'
//    | op='{' (keyValuePair ',')* keyValuePair? '}'
//    ;
fn creation(i: Input) -> Result<Expr> {
    alt((
        empty_array_creation,
        empty_byte_array_creation,
        array_creation,
        byte_array_creation,
        dictionary_creation,
    ))(i)
}

pub fn group(i: Input) -> Result<Expr> {
    between_parens(expr)(i)
}

// primary
//    : literal
//    | '(' expression ')'
//    | symbol invocation
//    | primary op='.' symbol invocation?
//    | primary arrayAccess
//    | creation
fn primary(i: Input) -> Result<Expr> {
    let call_expr = map(call, Expr::Call);
    let literal_expr = map(literal, Expr::Literal);

    alt((literal_expr, group, call_expr, creation))(i)
}

fn access(i: Input) -> Result<Expr> {
    enum Access<'a> {
        Index(Expr<'a>),
        Select(Id<'a>),
        Invoke(Id<'a>, Vec<Expr<'a>>),
    }

    let index = map(between_squares(expr), Access::Index);
    let select = map(symbol, Access::Select);
    let invoke = map(pair(symbol, invocation), |(method, arguments)| {
        Access::Invoke(method, arguments)
    });

    let selection = preceded(tag(Token::Dot), alt((invoke, select)));

    let (i, init) = primary(i)?;
    fold_many0(
        alt((index, selection)),
        init,
        |receiver, access| match access {
            Access::Index(offset) => Expr::Index(Box::new(Index { receiver, offset })),
            Access::Select(field) => Expr::Select(Box::new(Select { receiver, field })),
            Access::Invoke(method, arguments) => Expr::Invoke(Box::new(InvokeExpr {
                receiver,
                method,
                arguments,
            })),
        },
    )(i)
}

//factor: primary | op='~' factor | op='!' factor
fn factor(i: Input) -> Result<Expr> {
    let not = map(tag(Token::Exclamation), |_| PrefixOp::Not);
    let bit_not = map(tag(Token::Tilde), |_| PrefixOp::BitNot);
    let plus = map(tag(Token::Add), |_| PrefixOp::Plus);
    let minus = map(tag(Token::Sub), |_| PrefixOp::Minus);
    let unary_ops = alt((not, bit_not, plus, minus));
    map(pair(opt(unary_ops), access), |(op, value)| {
        if let Some(op) = op {
            Expr::PrefixOp(op, Box::new(value))
        } else {
            value
        }
    })(i)
}

// term: factor ( op=termOps factor )*
fn term(i: Input) -> Result<Expr> {
    // termOps: '*' | '/' | '%' | '&' | '<<' | '>>' | InstanceOf | Has
    let mul = map(tag(Token::Mul), |_| BinOp::Mul);
    let div = map(tag(Token::Div), |_| BinOp::Div);
    let module = map(tag(Token::Module), |_| BinOp::Module);
    let bit_and = map(tag(Token::BitAnd), |_| BinOp::BitAnd);
    let shift_left = map(tag(Token::SLeft), |_| BinOp::SLeft);
    let shift_right = map(tag(Token::SRight), |_| BinOp::SRight);
    let instance_of = map(tag(Token::InstanceOf), |_| BinOp::InstanceOf);
    let has = map(tag(Token::Has), |_| BinOp::Has);

    let term_ops = alt((
        mul,
        div,
        module,
        bit_and,
        shift_left,
        shift_right,
        instance_of,
        has,
    ));

    let (i, init) = factor(i)?;

    fold_many0(pair(term_ops, factor), init, |left, (op, right)| {
        Expr::BinOp(op, Box::new((left, right)))
    })(i)
}

// simpleExpression: pn=( '+' | '-')? term ( simpleExpressionOps term )*
fn simple_expr(i: Input) -> Result<Expr> {
    // simpleExpressionOps: '+' | '-' | '|' | '^'
    let add = map(tag(Token::Add), |_| BinOp::Add);
    let sub = map(tag(Token::Sub), |_| BinOp::Sub);
    let bit_or = map(tag(Token::BitOr), |_| BinOp::BitOr);
    let bit_xor = map(tag(Token::BitXor), |_| BinOp::BitXor);

    let simple_expression_ops = alt((add, sub, bit_or, bit_xor));

    let (i, init) = term(i)?;
    fold_many0(
        pair(simple_expression_ops, term),
        init,
        |left, (op, right)| Expr::BinOp(op, Box::new((left, right))),
    )(i)
}

// equalityExpression
//    : simpleExpression ( op=expressionOps simpleExpression)*
fn equality_expr(i: Input) -> Result<Expr> {
    // expressionOps: '==' | '<' | '<=' | '>' | '>=' | '!='
    let eq = map(tag(Token::Equal), |_| BinOp::Equal);
    let lt = map(tag(Token::Less), |_| BinOp::Less);
    let le = map(tag(Token::LessEqual), |_| BinOp::LessEqual);
    let gt = map(tag(Token::Greater), |_| BinOp::Greater);
    let ge = map(tag(Token::GreaterEqual), |_| BinOp::GreaterEqual);
    let ne = map(tag(Token::Distinct), |_| BinOp::Distinct);

    let expression_ops = alt((eq, lt, le, gt, ge, ne));

    let (i, init) = simple_expr(i)?;

    fold_many0(
        pair(expression_ops, simple_expr),
        init,
        |left, (op, right)| Expr::BinOp(op, Box::new((left, right))),
    )(i)
}

// conditionalAndExpression
//   : equalityExpression
//   | conditionalAndExpression op=('&&' | 'and') conditionalAndExpression
fn conditional_and_expr(i: Input) -> Result<Expr> {
    let (i, init) = equality_expr(i)?;

    fold_many0(
        preceded(tag(Token::And), equality_expr),
        init,
        |left, right| Expr::BinOp(BinOp::And, Box::new((left, right))),
    )(i)
}

// conditionalOrExpression
//   : conditionalAndExpression
//   | conditionalOrExpression op=('||' | 'or') conditionalOrExpression
fn conditional_or_expr(i: Input) -> Result<Expr> {
    let (i, init) = conditional_and_expr(i)?;

    fold_many0(
        preceded(tag(Token::Or), conditional_and_expr),
        init,
        |left, right| Expr::BinOp(BinOp::Or, Box::new((left, right))),
    )(i)
}

// conditionalExpression
// : conditionalOrExpression
// | conditionalOrExpression op='?' expression ':' conditionalExpression
fn conditional_expr(i: Input) -> Result<Expr> {
    let branches = preceded(
        tag(Token::Question),
        separated_pair(expr, tag(Token::Colon), conditional_expr),
    );
    alt((
        map(
            pair(conditional_or_expr, branches),
            |(condition, (then_branch, else_branch))| {
                Expr::Ternary(Box::new(Ternary {
                    condition,
                    then_branch,
                    else_branch,
                }))
            },
        ),
        conditional_or_expr,
    ))(i)
}

pub fn expr(i: Input) -> Result<Expr> {
    conditional_expr(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_parsing {
        ($parser:ident, $source:expr, $expected:expr) => {{
            let (_, token_input) = crate::lexer::Lexer::tokenize($source).unwrap();
            let tokens = crate::lexer::token::Tokens::new(&token_input);
            let (_, actual) = $parser(tokens).unwrap();

            assert_eq!(actual, $expected);
        }};

        ($source:expr, $expected:expr) => {
            assert_parsing!(expr, $source, $expected)
        };
    }

    #[test]
    fn parse_symbol() {
        assert_parsing!(
            "_Symbolifier1",
            Expr::Literal(Literal::Symbol("_Symbolifier1"))
        )
    }

    #[test]
    fn parse_integer_literal() {
        assert_parsing!("12", Expr::Literal(Literal::Integer(12)))
    }

    #[test]
    fn parse_lvalue_extension() {
        use LvalueExtension::*;
        assert_parsing!(
            lvalue_extension,
            ".bb.cc[12].dd(:sym).cc",
            Select(
                "bb",
                Some(Box::new(Select(
                    "cc",
                    Some(Box::new(Index(
                        Expr::Literal(Literal::Integer(12)),
                        Some(Box::new(Invoke(
                            "dd",
                            vec![Expr::Literal(Literal::SymbolRef("sym"))],
                            Box::new(Select("cc", None))
                        )))
                    )))
                )))
            )
        )
    }

    #[test]
    fn parse_new_dictionary_expr() {
        assert_parsing!(
            r#"
            {
                :x => "hello",
                12 => false
            }
            "#,
            Expr::NewDictionary(vec![
                (
                    Expr::Literal(Literal::SymbolRef("x")),
                    Expr::Literal(Literal::String("hello"))
                ),
                (
                    Expr::Literal(Literal::Integer(12)),
                    Expr::Literal(Literal::Boolean(false))
                )
            ])
        )
    }

    #[test]
    fn parse_index() {
        assert_parsing!(
            "a[10]",
            Expr::Index(Box::new(Index {
                receiver: Expr::Literal(Literal::Symbol("a")),
                offset: Expr::Literal(Literal::Integer(10))
            }))
        )
    }

    #[test]
    fn parse_double_index() {
        assert_parsing!(
            "a[10][20]",
            Expr::Index(Box::new(Index {
                receiver: Expr::Index(Box::new(Index {
                    receiver: Expr::Literal(Literal::Symbol("a")),
                    offset: Expr::Literal(Literal::Integer(10))
                })),
                offset: Expr::Literal(Literal::Integer(20))
            }))
        )
    }

    #[test]
    fn parse_select() {
        assert_parsing!(
            "a.b.c(10).d(11)",
            Expr::Invoke(Box::new(InvokeExpr {
                receiver: Expr::Invoke(Box::new(InvokeExpr {
                    receiver: Expr::Select(Box::new(Select {
                        receiver: Expr::Literal(Literal::Symbol("a")),
                        field: "b"
                    })),
                    method: "c",
                    arguments: vec![Expr::Literal(Literal::Integer(10))]
                })),
                method: "d",
                arguments: vec![Expr::Literal(Literal::Integer(11))]
            }))
        )
    }

    #[test]
    fn parse_global_select() {
        assert_parsing!(
            "$.b",
            Expr::Select(Box::new(Select {
                receiver: Expr::Literal(Literal::Global),
                field: "b",
            }))
        )
    }

    #[test]
    fn parse_multiplicative() {
        assert_parsing!(
            "-a * 12 / c",
            Expr::BinOp(
                BinOp::Div,
                Box::new((
                    Expr::BinOp(
                        BinOp::Mul,
                        Box::new((
                            Expr::PrefixOp(
                                PrefixOp::Minus,
                                Box::new(Expr::Literal(Literal::Symbol("a")))
                            ),
                            Expr::Literal(Literal::Integer(12))
                        ))
                    ),
                    Expr::Literal(Literal::Symbol("c"))
                )),
            )
        )
    }

    #[test]
    fn parse_additive() {
        assert_parsing!(
            "a + b * -c - 4 / d",
            Expr::BinOp(
                BinOp::Sub,
                Box::new((
                    Expr::BinOp(
                        BinOp::Add,
                        Box::new((
                            Expr::Literal(Literal::Symbol("a")),
                            Expr::BinOp(
                                BinOp::Mul,
                                Box::new((
                                    Expr::Literal(Literal::Symbol("b")),
                                    Expr::PrefixOp(
                                        PrefixOp::Minus,
                                        Box::new(Expr::Literal(Literal::Symbol("c")))
                                    )
                                ))
                            )
                        )),
                    ),
                    Expr::BinOp(
                        BinOp::Div,
                        Box::new((
                            Expr::Literal(Literal::Integer(4)),
                            Expr::Literal(Literal::Symbol("d"))
                        ))
                    )
                ))
            )
        )
    }

    #[test]
    fn parse_conditional_expr() {
        assert_parsing!(
            "a? b: c?d :e",
            Expr::Ternary(Box::new(Ternary {
                condition: Expr::Literal(Literal::Symbol("a")),
                then_branch: Expr::Literal(Literal::Symbol("b")),
                else_branch: Expr::Ternary(Box::new(Ternary {
                    condition: Expr::Literal(Literal::Symbol("c")),
                    then_branch: Expr::Literal(Literal::Symbol("d")),
                    else_branch: Expr::Literal(Literal::Symbol("e"))
                }))
            }))
        )
    }
}
