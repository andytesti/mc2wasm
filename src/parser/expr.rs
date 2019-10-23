use super::{Input, Result};
use crate::lexer::token::Token;
use crate::parser::ast::*;
use crate::parser::{between_brackets, between_parens, between_squares, map_from};
use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::{cut, flat_map, map, map_opt, map_res, opt, verify};
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

// symbolref
//    : ':' symbolrefId
pub fn symbol_ref(i: Input) -> Result<SymbolRef> {
    preceded(tag(Token::Colon), symbolref_id)(i)
}

// symbolrefId
//    : Id
//    | Id '(' symbol ')'
//    | Id '(' string ')'
//    | Id '(' bool ')'
//    | Id '(' nullReference ')'
//    | Id '(' number ')'
//    | Id '(' character ')'
//    | symbolrefIdArray
fn symbolref_id(i: Input) -> Result<SymbolRef> {
    use SymbolRefValue::*;
    let value = between_parens(alt((
        map(symbol, Symbol),
        map(string_lit, String),
        map(bool_lit, Bool),
        map(tag(Token::Null), |_| Null),
        map(number_lit, Number),
        map(symbolref_id_array, Array),
    )));
    map(pair(symbol, opt(value)), |(id, value)| SymbolRef {
        id,
        value,
    })(i)
}

// symbolrefIdArray
//    : Id '(' '[' symbol (',' symbol)* ']' ')'
fn symbolref_id_array(i: Input) -> Result<Vec<Id>> {
    between_squares(separated_list(tag(Token::Comma), symbol))(i)
}

pub fn symbol(i: Input) -> Result<Id> {
    i.pop_token(|t| match t {
        Token::Id(v) => Some(Id(*v)),
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
        map(symbol_ref, Literal::SymbolRef),
        map_from(number_lit),
        map_from(string_lit),
        map_from(bool_lit),
        map(tag(Token::Null), |_| Literal::Null),
        map(tag(Token::Bling), |_| Literal::Global),
        map(tag(Token::Me), |_| Literal::Me),
    ))(i)
}

// invocation
//    : '(' argc+=expression (',' argc+=expression)* ')'
//    | '(' ')'
fn invocation(i: Input) -> Result<Vec<Expression>> {
    between_parens(separated_list(tag(Token::Comma), expression))(i)
}

pub fn call(i: Input) -> Result<Call> {
    map(pair(symbol, invocation), |(name, arguments)| Call {
        name,
        arguments,
    })(i)
}

// op='{' (keyValuePair ',')* keyValuePair? '}'
fn dictionary_creation(i: Input) -> Result<Creation> {
    // keyValuePair
    //    : key=expression '=>' value=expression
    let key_value_pair = separated_pair(expression, tag(Token::Arrow), expression);
    map(
        between_brackets(separated_list(tag(Token::Comma), key_value_pair)),
        Creation::Dictionary,
    )(i)
}

// op='[' (expression ',')* expression? ']'
fn array_creation(i: Input) -> Result<Creation> {
    map(
        between_squares(separated_list(tag(Token::Comma), expression)),
        Creation::Array,
    )(i)
}

// op='[' (expression ',')* expression? ']b'
fn byte_array_creation(i: Input) -> Result<Creation> {
    map(
        delimited(
            tag(Token::OpenSquare),
            separated_list(tag(Token::Comma), expression),
            tag(Token::CloseSquareB),
        ),
        Creation::ByteArray,
    )(i)
}

// op='new' '[' expression ']'
fn empty_array_creation(i: Input) -> Result<Creation> {
    map(
        preceded(tag(Token::New), between_squares(expression)),
        |size| Creation::EmptyArray(size),
    )(i)
}

// op='new' '[' expression ']b'
fn empty_byte_array_creation(i: Input) -> Result<Creation> {
    map(
        preceded(
            tag(Token::New),
            delimited(tag(Token::OpenSquare), expression, tag(Token::CloseSquareB)),
        ),
        |size| Creation::EmptyByteArray(size),
    )(i)
}

//lvalue
//    : (BlingToken op='.')? symbol lvalueExtension?
//    | (BlingToken op='.')? symbol invocation lvalueExtension
pub fn lvalue(i: Input) -> Result<LValue> {
    fn scope(i: Input) -> Result<Scope> {
        map(opt(pair(tag(Token::Bling), tag(Token::Dot))), |g| {
            if g.is_some() {
                Scope::Global
            } else {
                Scope::Local
            }
        })(i)
    }
    let select = map(
        tuple((scope, symbol, opt(lvalue_extension))),
        |(scope, field, extension)| LValue {
            scope,
            extension: LValueExtension::Select(field, extension.map(Box::new)),
        },
    );
    let invoke = map(
        tuple((scope, symbol, invocation, lvalue_extension)),
        |(scope, method, arguments, extension)| LValue {
            scope,
            extension: LValueExtension::Invoke(method, arguments, Box::new(extension)),
        },
    );
    alt((select, invoke))(i)
}

// lvalueExtension
//    : op='.' symbol lvalueExtension?
//    | op='[' expression ']' lvalueExtension?
//    | op='.' symbol invocation lvalueExtension
pub fn lvalue_extension(i: Input) -> Result<LValueExtension> {
    let select = map(
        preceded(tag(Token::Dot), pair(symbol, opt(lvalue_extension))),
        |(field, extension)| LValueExtension::Select(field, extension.map(Box::new)),
    );
    let index = map(
        pair(between_squares(expression), opt(lvalue_extension)),
        |(offset, extension)| LValueExtension::Index(offset, extension.map(Box::new)),
    );
    let invoke = map(
        preceded(
            tag(Token::Dot),
            tuple((symbol, invocation, lvalue_extension)),
        ),
        |(method, arguments, extension)| {
            LValueExtension::Invoke(method, arguments, Box::new(extension))
        },
    );
    alt((select, index, invoke))(i)
}

fn object_creation(i: Input) -> Result<Creation> {
    map(
        preceded(tag(Token::New), pair(lvalue, invocation)),
        |(lvalue, args)| Creation::Object { lvalue, args },
    )(i)
}

//creation
//    : op='new' lvalue invocation
//    | op='new' '[' expression ']'
//    | op='new' '[' expression ']b'
//    | op='[' (expression ',')* expression? ']'
//    | op='[' (expression ',')* expression? ']b'
//    | op='{' (keyValuePair ',')* keyValuePair? '}'
pub fn creation(i: Input) -> Result<Creation> {
    alt((
        object_creation,
        empty_array_creation,
        empty_byte_array_creation,
        array_creation,
        byte_array_creation,
        dictionary_creation,
    ))(i)
}

pub fn group(i: Input) -> Result<Expression> {
    between_parens(expression)(i)
}

// primary
//    : literal
//    | '(' expression ')'
//    | symbol invocation
//    | primary op='.' symbol invocation?
//    | primary arrayAccess
//    | creation
fn primary(i: Input) -> Result<Expression> {
    alt((map_from(literal), group, map_from(call), map_from(creation)))(i)
}

fn access(i: Input) -> Result<Expression> {
    enum Message<'a> {
        Index(Expression<'a>),
        Select(Id<'a>),
        Invoke(Id<'a>, Invocation<'a>),
    }

    let index = map(between_squares(expression), Message::Index);

    let select = map(symbol, Message::Select);
    let invoke = map(pair(symbol, invocation), |(method, arguments)| {
        Message::Invoke(method, arguments)
    });
    let selection = preceded(tag(Token::Dot), alt((invoke, select)));

    let (i, init) = primary(i)?;
    fold_many0(
        alt((index, selection)),
        init,
        |receiver, message| match message {
            Message::Index(offset) => Index { receiver, offset }.into(),
            Message::Select(field) => Select { receiver, field }.into(),
            Message::Invoke(method, arguments) => Invoke {
                receiver,
                method,
                arguments,
            }
            .into(),
        },
    )(i)
}

//factor: primary | op='~' factor | op='!' factor
pub fn factor(i: Input) -> Result<Expression> {
    let not = map(tag(Token::Exclamation), |_| PrefixOp::Not);
    let bit_not = map(tag(Token::Tilde), |_| PrefixOp::BitNot);
    let plus = map(tag(Token::Add), |_| PrefixOp::Plus);
    let minus = map(tag(Token::Sub), |_| PrefixOp::Minus);
    let unary_ops = alt((not, bit_not, plus, minus));
    map(pair(opt(unary_ops), access), |(op, value)| {
        if let Some(op) = op {
            Expression::PrefixOp(op, Box::new(value))
        } else {
            value
        }
    })(i)
}

// term: factor ( op=termOps factor )*
fn term(i: Input) -> Result<Expression> {
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
        Expression::BinOp(op, Box::new((left, right)))
    })(i)
}

// simpleExpression: pn=( '+' | '-')? term ( simpleExpressionOps term )*
fn simple_expr(i: Input) -> Result<Expression> {
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
        |left, (op, right)| Expression::BinOp(op, Box::new((left, right))),
    )(i)
}

// equalityExpression
//    : simpleExpression ( op=expressionOps simpleExpression)*
fn equality_expr(i: Input) -> Result<Expression> {
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
        |left, (op, right)| Expression::BinOp(op, Box::new((left, right))),
    )(i)
}

// conditionalAndExpression
//   : equalityExpression
//   | conditionalAndExpression op=('&&' | 'and') conditionalAndExpression
fn conditional_and_expr(i: Input) -> Result<Expression> {
    let (i, init) = equality_expr(i)?;

    fold_many0(
        preceded(tag(Token::And), equality_expr),
        init,
        |left, right| Expression::BinOp(BinOp::And, Box::new((left, right))),
    )(i)
}

// conditionalOrExpression
//   : conditionalAndExpression
//   | conditionalOrExpression op=('||' | 'or') conditionalOrExpression
fn conditional_or_expr(i: Input) -> Result<Expression> {
    let (i, init) = conditional_and_expr(i)?;

    fold_many0(
        preceded(tag(Token::Or), conditional_and_expr),
        init,
        |left, right| Expression::BinOp(BinOp::Or, Box::new((left, right))),
    )(i)
}

// conditionalExpression
// : conditionalOrExpression
// | conditionalOrExpression op='?' expression ':' conditionalExpression
fn conditional_expr(i: Input) -> Result<Expression> {
    let branches = preceded(
        tag(Token::Question),
        separated_pair(expression, tag(Token::Colon), conditional_expr),
    );
    alt((
        map(
            pair(conditional_or_expr, branches),
            |(condition, (then_branch, else_branch))| {
                Ternary {
                    condition,
                    then_branch,
                    else_branch,
                }
                .into()
            },
        ),
        conditional_or_expr,
    ))(i)
}

// expression
//    : conditionalExpression
pub fn expression(i: Input) -> Result<Expression> {
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
            assert_parsing!(expression, $source, $expected.into())
        };
    }

    #[test]
    fn parse_symbol() {
        let text = "_Symbolifier1";
        let tree = Id("_Symbolifier1");
        assert_parsing!(text, tree)
    }

    #[test]
    fn parse_integer_literal() {
        let text = "12";
        let tree = 12;
        assert_parsing!(text, tree)
    }

    #[test]
    fn parse_lvalue() {
        let text = "Lang.Integer";
        let tree = LValue {
            scope: Scope::Local,
            extension: LValueExtension::Select(
                Id("Lang"),
                Some(Box::new(LValueExtension::Select(Id("Integer"), None))),
            ),
        };
        assert_parsing!(lvalue, text, tree)
    }

    #[test]
    fn parse_object_creation() {
        let text = "new Lang.Integer()";
        let tree = Creation::Object {
            lvalue: LValue {
                scope: Scope::Local,
                extension: LValueExtension::Select(
                    Id("Lang"),
                    Some(Box::new(LValueExtension::Select(Id("Integer"), None))),
                ),
            },
            args: Default::default(),
        };
        assert_parsing!(text, tree)
    }

    #[test]
    fn parse_dictionary_creation() {
        let text = r#"
            {
                :x => "hello",
                12 => false
            }
            "#;
        let tree = Creation::Dictionary(vec![
            (
                Literal::SymbolRef(SymbolRef {
                    id: Id("x"),
                    value: None,
                })
                .into(),
                "hello".into(),
            ),
            (12.into(), false.into()),
        ]);
        assert_parsing!(creation, text, tree)
    }

    #[test]
    fn parse_array_creation() {
        let text = r#"[:x, "hello", 12, false]"#;
        let tree = Creation::Array(vec![
            Literal::SymbolRef(SymbolRef {
                id: Id("x"),
                value: None,
            })
            .into(),
            "hello".into(),
            12.into(),
            false.into(),
        ]);
        assert_parsing!(creation, text, tree)
    }

    #[test]
    fn parse_empty_array_creation() {
        let text = "new [12]";
        let tree = Creation::EmptyArray(12.into());
        assert_parsing!(creation, text, tree)
    }

    #[test]
    fn parse_byte_array_creation() {
        let text = r#"[:x, "hello", 12, false]b"#;
        let tree = Creation::ByteArray(vec![
            Literal::SymbolRef(SymbolRef {
                id: Id("x"),
                value: None,
            })
            .into(),
            "hello".into(),
            12.into(),
            false.into(),
        ]);
        assert_parsing!(creation, text, tree)
    }

    #[test]
    fn parse_empty_byte_array_creation() {
        let text = "new [12]b";
        let tree = Creation::EmptyByteArray(12.into());
        assert_parsing!(creation, text, tree)
    }

    #[test]
    fn parse_index() {
        let text = "a[10]";
        let tree = Index {
            receiver: Id("a").into(),
            offset: 10.into(),
        };
        assert_parsing!(text, tree)
    }

    #[test]
    fn parse_double_index() {
        let text = "a[10][20]";
        let tree = Index {
            receiver: Index {
                receiver: Id("a").into(),
                offset: 10.into(),
            }
            .into(),
            offset: 20.into(),
        };
        assert_parsing!(text, tree)
    }

    #[test]
    fn parse_select() {
        let text = "a.b.c(10).d(11)";
        let tree = Invoke {
            receiver: Invoke {
                receiver: Select {
                    receiver: Id("a").into(),
                    field: Id("b"),
                }
                .into(),
                method: Id("c"),
                arguments: vec![10.into()],
            }
            .into(),
            method: Id("d"),
            arguments: vec![11.into()],
        };
        assert_parsing!(text, tree)
    }

    #[test]
    fn parse_global_select() {
        let text = "$.b";
        let tree = Select {
            receiver: Literal::Global.into(),
            field: Id("b"),
        };
        assert_parsing!(text, tree)
    }

    #[test]
    fn parse_multiplicative() {
        let text = "-a * 12 / c";
        let tree = Expression::BinOp(
            BinOp::Div,
            Box::new((
                Expression::BinOp(
                    BinOp::Mul,
                    Box::new((
                        Expression::PrefixOp(PrefixOp::Minus, Box::new(Id("a").into())),
                        12.into(),
                    )),
                ),
                Id("c").into(),
            )),
        );
        assert_parsing!(text, tree)
    }

    #[test]
    fn parse_additive() {
        let text = "a + b * -c - 4 / d";
        let tree = Expression::BinOp(
            BinOp::Sub,
            Box::new((
                Expression::BinOp(
                    BinOp::Add,
                    Box::new((
                        Id("a").into(),
                        Expression::BinOp(
                            BinOp::Mul,
                            Box::new((
                                Id("b").into(),
                                Expression::PrefixOp(PrefixOp::Minus, Box::new(Id("c").into())),
                            )),
                        ),
                    )),
                ),
                Expression::BinOp(BinOp::Div, Box::new((4.into(), Id("d").into()))),
            )),
        );
        assert_parsing!(text, tree)
    }

    #[test]
    fn parse_conditional_expr() {
        let text = "a? b: c?d :e";
        let tree = Ternary {
            condition: Id("a").into(),
            then_branch: Id("b").into(),
            else_branch: Ternary {
                condition: Id("c").into(),
                then_branch: Id("d").into(),
                else_branch: Id("e").into(),
            }
            .into(),
        };
        assert_parsing!(text, tree)
    }
}
