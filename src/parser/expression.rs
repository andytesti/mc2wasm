use super::{Input, Result};
use crate::lexer::token::Token;
use crate::parser::ast::*;
use crate::parser::{between_brackets, between_parens, between_squares};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{cut, map, map_opt, map_res, opt};
use nom::error::{context, ErrorKind};
use nom::multi::{fold_many0, separated_list};
use nom::sequence::{pair, preceded, separated_pair};

fn string_literal(i: Input) -> Result<&str> {
    i.pop_token(|t| match t {
        Token::StringLiteral(s) => Some(*s),
        _ => None,
    })
}

pub fn symbol_literal(i: Input) -> Result<Ident> {
    i.pop_token(|t| match t {
        Token::Symbol(s) => Some(Ident(*s)),
        _ => None,
    })
}

pub fn ident(i: Input) -> Result<Ident> {
    i.pop_token(|t| match t {
        Token::Ident(v) => Some(Ident(v)),
        _ => None,
    })
}

fn integer_literal(i: Input) -> Result<i64> {
    i.pop_token(|t| match t {
        Token::IntLiteral(i) => Some(*i),
        _ => None,
    })
}

fn boolean_literal(i: Input) -> Result<bool> {
    alt((
        map(tag(Token::False), |_| false),
        map(tag(Token::True), |_| true),
    ))(i)
}

pub fn literal(i: Input) -> Result<Literal> {
    alt((
        map(integer_literal, Literal::Integer),
        map(string_literal, Literal::String),
        map(symbol_literal, Literal::Symbol),
        map(boolean_literal, Literal::Boolean),
    ))(i)
}

pub fn path(i: Input) -> Result<Path> {
    map(separated_list(tag(Token::Dot), ident), Path)(i)
}

fn args(i: Input) -> Result<Vec<Expr>> {
    between_parens(separated_list(tag(Token::Comma), expr))(i)
}

pub fn call(i: Input) -> Result<CallExpr> {
    map(pair(ident, args), |(name, arguments)| CallExpr {
        name,
        arguments,
    })(i)
}

fn new_dictionary(i: Input) -> Result<Expr> {
    let entry = separated_pair(expr, tag(Token::Arrow), expr);
    map(
        between_brackets(separated_list(tag(Token::Comma), entry)),
        Expr::NewDictionary,
    )(i)
}

fn new_object(i: Input) -> Result<Expr> {
    map(
        preceded(tag(Token::New), pair(path, args)),
        |(path, args)| Expr::NewObject(NewObject { path, args }),
    )(i)
}

fn new_array(i: Input) -> Result<Expr> {
    map(
        between_squares(separated_list(tag(Token::Comma), expr)),
        Expr::NewArray,
    )(i)
}

fn new_empty_array(i: Input) -> Result<Expr> {
    map(preceded(tag(Token::New), between_squares(expr)), |size| {
        Expr::NewEmptyArray(Box::new(size))
    })(i)
}

pub fn group(i: Input) -> Result<Expr> {
    between_parens(expr)(i)
}

fn node(i: Input) -> Result<Expr> {
    let me_expr = map(tag(Token::Me), |_| Expr::Me);
    let null_expr = map(tag(Token::Null), |_| Expr::Null);
    let call_expr = map(call, Expr::Call);
    let ident_expr = map(ident, Expr::Ident);
    let literal_expr = map(literal, Expr::Literal);

    alt((
        group,
        me_expr,
        null_expr,
        new_dictionary,
        new_array,
        new_empty_array,
        new_object,
        call_expr,
        ident_expr,
        literal_expr,
    ))(i)
}

fn access(i: Input) -> Result<Expr> {
    enum Access<'a> {
        Index(Expr<'a>),
        Select(Ident<'a>),
        Invoke(Ident<'a>, Vec<Expr<'a>>),
    }

    let index = map(between_squares(expr), Access::Index);
    let select = map(preceded(tag(Token::Dot), ident), Access::Select);
    let invoke = map(
        preceded(tag(Token::Dot), pair(ident, args)),
        |(name, args)| Access::Invoke(name, args),
    );

    let (i, init) = node(i)?;
    fold_many0(
        alt((index, invoke, select)),
        init,
        |receiver, operation| match operation {
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

fn unary(i: Input) -> Result<Expr> {
    let not = map(tag(Token::Exclamation), |_| PrefixOp::Not);
    let plus = map(tag(Token::Add), |_| PrefixOp::Plus);
    let minus = map(tag(Token::Sub), |_| PrefixOp::Minus);

    map(pair(opt(alt((not, plus, minus))), access), |(op, value)| {
        if let Some(op) = op {
            Expr::PrefixOp(op, Box::new(value))
        } else {
            value
        }
    })(i)
}

fn multiplicative(i: Input) -> Result<Expr> {
    let mul = map(tag(Token::Mul), |_| BinOp::Mul);
    let div = map(tag(Token::Div), |_| BinOp::Div);

    let (i, init) = unary(i)?;

    fold_many0(pair(alt((mul, div)), unary), init, |left, (op, right)| {
        Expr::BinOp(op, Box::new((left, right)))
    })(i)
}

fn additive(i: Input) -> Result<Expr> {
    let add = map(tag(Token::Add), |_| BinOp::Add);
    let sub = map(tag(Token::Sub), |_| BinOp::Sub);

    let (i, init) = multiplicative(i)?;

    fold_many0(
        pair(alt((add, sub)), multiplicative),
        init,
        |left, (op, right)| Expr::BinOp(op, Box::new((left, right))),
    )(i)
}

fn relational(i: Input) -> Result<Expr> {
    let eq = map(tag(Token::Equal), |_| BinOp::Equal);
    let ne = map(tag(Token::Distinct), |_| BinOp::Distinct);
    let lt = map(tag(Token::Less), |_| BinOp::Less);
    let le = map(tag(Token::LessEqual), |_| BinOp::LessEqual);
    let gt = map(tag(Token::Greater), |_| BinOp::Greater);
    let ge = map(tag(Token::GreaterEqual), |_| BinOp::GreaterEqual);

    let operator = alt((eq, ne, lt, le, gt, ge));

    let (i, init) = additive(i)?;

    fold_many0(pair(operator, additive), init, |left, (op, right)| {
        Expr::BinOp(op, Box::new((left, right)))
    })(i)
}

fn logical_and(i: Input) -> Result<Expr> {
    let (i, init) = relational(i)?;

    fold_many0(
        preceded(tag(Token::And), relational),
        init,
        |left, right| Expr::BinOp(BinOp::And, Box::new((left, right))),
    )(i)
}

fn logical_or(i: Input) -> Result<Expr> {
    let (i, init) = logical_and(i)?;

    fold_many0(
        preceded(tag(Token::Or), logical_and),
        init,
        |left, right| Expr::BinOp(BinOp::Or, Box::new((left, right))),
    )(i)
}

fn ternary(i: Input) -> Result<Expr> {
    let branches = preceded(
        tag(Token::Question),
        separated_pair(expr, tag(Token::Colon), expr),
    );
    map(pair(logical_or, opt(branches)), |(condition, branches)| {
        if let Some((then_branch, else_branch)) = branches {
            Expr::Ternary(Box::new(Ternary {
                condition,
                then_branch,
                else_branch,
            }))
        } else {
            condition
        }
    })(i)
}

pub fn assignment(i: Input) -> Result<Expr> {
    let simple = map(tag(Token::Assign), |_| AssignmentOp::Simple);
    let add = map(tag(Token::AddAssign), |_| AssignmentOp::Add);
    let sub = map(tag(Token::SubAssign), |_| AssignmentOp::Sub);
    let mul = map(tag(Token::MulAssign), |_| AssignmentOp::Mul);
    let div = map(tag(Token::DivAssign), |_| AssignmentOp::Div);
    let sleft = map(tag(Token::SLeftAssign), |_| AssignmentOp::SLeft);
    let sright = map(tag(Token::SRightAssign), |_| AssignmentOp::SRight);

    let operator = alt((simple, add, sub, mul, div, sleft, sright));
    map_opt(
        pair(ternary, opt(pair(operator, expr))),
        |(assignee, right)| {
            if let Some((operator, value)) = right {
                let assignee = match assignee {
                    Expr::Ident(var) => Assignee::Var(var),
                    Expr::Index(i) => Assignee::Index(*i),
                    Expr::Select(s) => Assignee::Select(*s),
                    // TODO add specific failure
                    _ => None?,
                };
                Some(Expr::Assignment(Box::new(Assignment {
                    operator,
                    assignee,
                    value,
                })))
            } else {
                Some(assignee)
            }
        },
    )(i)
}

pub fn expr(i: Input) -> Result<Expr> {
    assignment(i)
}

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
    fn parse_ident() {
        assert_parsing!("_identifier1", Expr::Ident(Ident("_identifier1")))
    }

    #[test]
    fn parse_integer_literal() {
        assert_parsing!("12", Expr::Literal(Literal::Integer(12)))
    }

    #[test]
    fn parse_path() {
        assert_parsing!(path, "a.b", Path(vec![Ident("a"), Ident("b")]))
    }

    #[test]
    fn parse_new_object() {
        assert_parsing!(
            "new Lang.Object(12)",
            Expr::NewObject(NewObject {
                path: Path(vec![Ident("Lang"), Ident("Object")]),
                args: vec![Expr::Literal(Literal::Integer(12))],
            })
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
                    Expr::Literal(Literal::Symbol(Ident("x"))),
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
                receiver: Expr::Ident(Ident("a")),
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
                    receiver: Expr::Ident(Ident("a")),
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
                        receiver: Expr::Ident(Ident("a")),
                        field: Ident("b")
                    })),
                    method: Ident("c"),
                    arguments: vec![Expr::Literal(Literal::Integer(10))]
                })),
                method: Ident("d"),
                arguments: vec![Expr::Literal(Literal::Integer(11))]
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
                            Expr::PrefixOp(PrefixOp::Minus, Box::new(Expr::Ident(Ident("a")))),
                            Expr::Literal(Literal::Integer(12))
                        ))
                    ),
                    Expr::Ident(Ident("c"))
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
                            Expr::Ident(Ident("a")),
                            Expr::BinOp(
                                BinOp::Mul,
                                Box::new((
                                    Expr::Ident(Ident("b")),
                                    Expr::PrefixOp(
                                        PrefixOp::Minus,
                                        Box::new(Expr::Ident(Ident("c")))
                                    )
                                ))
                            )
                        )),
                    ),
                    Expr::BinOp(
                        BinOp::Div,
                        Box::new((Expr::Literal(Literal::Integer(4)), Expr::Ident(Ident("d"))))
                    )
                ))
            )
        )
    }

    #[test]
    fn parse_ternary() {
        assert_parsing!(
            "a? b: c",
            Expr::Ternary(Box::new(Ternary {
                condition: Expr::Ident(Ident("a")),
                then_branch: Expr::Ident(Ident("b")),
                else_branch: Expr::Ident(Ident("c"))
            }))
        )
    }

    #[test]
    fn parse_assign() {
        assert_parsing!(
            "a = b[10] += c.d -= e",
            Expr::Ternary(Box::new(Ternary {
                condition: Expr::Ident(Ident("a")),
                then_branch: Expr::Ident(Ident("b")),
                else_branch: Expr::Ident(Ident("c"))
            }))
        )
    }
}
