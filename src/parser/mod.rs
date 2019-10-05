use std::collections::HashMap;
use std::result::Result::*;

use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::{cut, map, map_opt, map_parser, map_res, opt};
use nom::error::{context, ErrorKind};
use nom::multi::{many0, many1, separated_list};
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use nom::Err;
use nom::IResult;

use crate::lexer::token::{Token, Tokens};
use crate::parser::ast::{
    Assignment, CallExpr, CaseBlock, CaseLabel, CatchGuard, CatchStmt, ClassDef, ClassMember,
    ConstDef, DecoratedDef, DoWhileStmt, EnumDef, Expr, ForStmt, FunctionDef, Group, Ident, IfStmt,
    InitializableDef, InvokeExpr, Literal, ModuleDef, ModuleMember, NewObject, Path, RootModuleDef,
    Scope, Stmt, StmtBlock, SwitchStmt, TryStmt, Using, VarDef, Visibility, WhileStmt,
};
use crate::parser::expression::{call, expr, group, ident, literal, path, symbol_literal};

pub mod ast;
mod expression;

type Input<'a> = Tokens<'a>;
type Result<'a, Output> = IResult<Input<'a>, Output>;

fn semicolon_terminated<'a, T, F>(term: F) -> impl Fn(Input<'a>) -> Result<T>
where
    T: 'a,
    F: Fn(Input<'a>) -> Result<T>,
{
    terminated(term, tag(Token::Semicolon))
}

fn between<'a, T, F>(open: Token<'a>, term: F, close: Token<'a>) -> impl Fn(Input<'a>) -> Result<T>
where
    T: 'a,
    F: Fn(Input<'a>) -> Result<T>,
{
    delimited(tag(open), term, tag(close))
}

fn between_parens<'a, T: 'a>(
    term: impl Fn(Input<'a>) -> Result<T>,
) -> impl Fn(Input<'a>) -> Result<T> {
    between(Token::OpenParen, term, Token::CloseParen)
}

fn between_squares<'a, T: 'a>(
    term: impl Fn(Input<'a>) -> Result<T>,
) -> impl Fn(Input<'a>) -> Result<T> {
    between(Token::OpenSquare, term, Token::CloseSquare)
}

fn between_brackets<'a, T: 'a>(
    term: impl Fn(Input<'a>) -> Result<T>,
) -> impl Fn(Input<'a>) -> Result<T> {
    between(Token::OpenBracket, term, Token::CloseBracket)
}

fn using(i: Input) -> Result<Using> {
    map(
        preceded(
            tag(Token::Using),
            pair(path, opt(preceded(tag(Token::As), ident))),
        ),
        |(path, alias)| Using { path, alias },
    )(i)
}

fn return_stmt(i: Input) -> Result<Stmt> {
    map(preceded(tag(Token::Return), expr), Stmt::Return)(i)
}

fn initializable_def(i: Input) -> Result<InitializableDef> {
    map(
        pair(ident, opt(preceded(tag(Token::Assign), expr))),
        |(name, init)| InitializableDef { name, init },
    )(i)
}

fn enum_def(i: Input) -> Result<EnumDef> {
    map(
        preceded(
            tag(Token::Enum),
            between_brackets(separated_list(tag(Token::Comma), initializable_def)),
        ),
        EnumDef,
    )(i)
}

fn var_def(i: Input) -> Result<VarDef> {
    preceded(tag(Token::Var), initializable_def)(i)
}

fn const_def(i: Input) -> Result<ConstDef> {
    map(
        preceded(
            tag(Token::Const),
            separated_pair(ident, tag(Token::Assign), expr),
        ),
        |(name, init)| ConstDef { name, init },
    )(i)
}

fn call_stmt(i: Input) -> Result<Stmt> {
    map(call, Stmt::Call)(i)
}

fn var_def_stmt(i: Input) -> Result<Stmt> {
    map(var_def, Stmt::VarDef)(i)
}

fn break_stmt(i: Input) -> Result<Stmt> {
    map(tag(Token::Break), |_| Stmt::Break)(i)
}

fn expr_stmt(i: Input) -> Result<Stmt> {
    map_opt(expr, |e| {
        println!("expr = {:?}", e);
        match e {
            Expr::Invoke(i) => Some(Stmt::Invoke(*i)),
            Expr::Call(c) => Some(Stmt::Call(c)),
            Expr::Assignment(a) => Some(Stmt::Assignment(*a)),
            // TODO add specific failure
            _ => None,
        }
    })(i)
}

fn terminal_stmt(i: Input) -> Result<Stmt> {
    alt((
        break_stmt,
        var_def_stmt,
        return_stmt,
        do_while_stmt,
        expr_stmt,
    ))(i)
}

fn while_stmt(i: Input) -> Result<Stmt> {
    map(
        preceded(tag(Token::While), pair(between_parens(expr), stmt)),
        |(cond, body)| Stmt::WhileStmt(Box::new(WhileStmt { cond, body })),
    )(i)
}

fn do_while_stmt(i: Input) -> Result<Stmt> {
    map(
        preceded(
            tag(Token::Do),
            pair(
                stmt_block,
                preceded(tag(Token::While), between_parens(expr)),
            ),
        ),
        |(body, cond)| Stmt::DoWhileStmt(DoWhileStmt { cond, body }),
    )(i)
}

fn for_stmt(i: Input) -> Result<Stmt> {
    map(
        preceded(
            tag(Token::For),
            pair(
                between_parens(tuple((
                    semicolon_terminated(opt(terminal_stmt)),
                    semicolon_terminated(opt(expr)),
                    opt(terminal_stmt),
                ))),
                stmt,
            ),
        ),
        |((init, cond, inc), body)| {
            Stmt::ForStmt(Box::new(ForStmt {
                init,
                cond,
                inc,
                body,
            }))
        },
    )(i)
}

fn structured_stmt(i: Input) -> Result<Stmt> {
    alt((
        stmt_block_stmt,
        if_stmt,
        for_stmt,
        while_stmt,
        switch_stmt,
        try_stmt,
        throw_stmt,
    ))(i)
}

fn stmt(i: Input) -> Result<Stmt> {
    let line_stmt = semicolon_terminated(terminal_stmt);
    alt((line_stmt, structured_stmt))(i)
}

fn stmt_block(i: Input) -> Result<StmtBlock> {
    between_brackets(many0(stmt))(i)
}

fn stmt_block_stmt(i: Input) -> Result<Stmt> {
    map(stmt_block, Stmt::StmtBlock)(i)
}

fn if_stmt(i: Input) -> Result<Stmt> {
    map(
        tuple((
            preceded(tag(Token::If), group),
            stmt,
            opt(preceded(tag(Token::Else), stmt)),
        )),
        |(cond, true_branch, false_branch)| {
            Stmt::IfStmt(Box::new(IfStmt {
                cond,
                then_branch: true_branch,
                else_branch: false_branch,
            }))
        },
    )(i)
}

fn case_label(i: Input) -> Result<CaseLabel> {
    terminated(
        alt((
            map(preceded(tag(Token::Case), literal), CaseLabel::Literal),
            map(
                preceded(pair(tag(Token::Case), tag(Token::InstanceOf)), path),
                CaseLabel::InstanceOf,
            ),
            map(tag(Token::Default), |_| CaseLabel::Default),
        )),
        tag(Token::Colon),
    )(i)
}

fn case_block(i: Input) -> Result<CaseBlock> {
    map(
        pair(many1(case_label), many1(stmt)),
        |(labels, statements)| CaseBlock { labels, statements },
    )(i)
}

fn switch_stmt(i: Input) -> Result<Stmt> {
    map(
        preceded(
            tag(Token::Switch),
            pair(between_parens(expr), between_brackets(many1(case_block))),
        ),
        |(cond, case_blocks)| Stmt::SwitchStmt(SwitchStmt { cond, case_blocks }),
    )(i)
}

fn params(i: Input) -> Result<Vec<Ident>> {
    between_parens(separated_list(tag(Token::Comma), ident))(i)
}

fn function_def(i: Input) -> Result<FunctionDef> {
    map(
        tuple((preceded(tag(Token::Function), ident), params, stmt_block)),
        |(name, params, body)| FunctionDef { name, params, body },
    )(i)
}

fn enum_member_def(i: Input) -> Result<ClassMember> {
    map(enum_def, ClassMember::EnumDef)(i)
}

fn function_member_def(i: Input) -> Result<ClassMember> {
    map(function_def, ClassMember::FunctionDef)(i)
}

fn static_scope(i: Input) -> Result<Scope> {
    map(tag(Token::Static), |_| Scope::Static)(i)
}

fn public_visibility(i: Input) -> Result<Visibility> {
    map(tag(Token::Public), |_| Visibility::Public)(i)
}

fn private_visibility(i: Input) -> Result<Visibility> {
    map(tag(Token::Private), |_| Visibility::Private)(i)
}

fn protected_visibility(i: Input) -> Result<Visibility> {
    map(tag(Token::Protected), |_| Visibility::Protected)(i)
}

fn visibility(i: Input) -> Result<Visibility> {
    alt((public_visibility, private_visibility, protected_visibility))(i)
}

fn modifiers(i: Input) -> Result<(Scope, Visibility)> {
    map(
        opt(alt((
            map(pair(visibility, static_scope), |(v, s)| (s, v)),
            map(pair(static_scope, visibility), |(s, v)| (s, v)),
            map(visibility, |v| (Scope::Instance, v)),
            map(static_scope, |s| (s, Visibility::Public)),
        ))),
        |m| m.unwrap_or((Scope::Instance, Visibility::Public)),
    )(i)
}

fn var_member_def(i: Input) -> Result<ClassMember> {
    map(semicolon_terminated(var_def), ClassMember::VarDef)(i)
}

fn const_member_def(i: Input) -> Result<ClassMember> {
    map(semicolon_terminated(const_def), ClassMember::ConstDef)(i)
}

fn annotations(i: Input) -> Result<Vec<Ident>> {
    many0(between_parens(symbol_literal))(i)
}

fn decorated_def<'a, T: 'a>(
    definition: impl Fn(Input<'a>) -> Result<T>,
) -> impl Fn(Input<'a>) -> Result<DecoratedDef<'a, T>> {
    map(
        tuple((annotations, modifiers, definition)),
        |(annotations, (scope, visibility), definition)| DecoratedDef {
            annotations,
            scope,
            visibility,
            definition,
        },
    )
}

fn class_def(i: Input) -> Result<ClassDef> {
    let class_member_def = decorated_def(alt((
        var_member_def,
        const_member_def,
        function_member_def,
        enum_member_def,
    )));
    map(
        tuple((
            preceded(tag(Token::Class), ident),
            opt(preceded(tag(Token::Extends), path)),
            between_brackets(many0(class_member_def)),
        )),
        |(name, extends, members)| ClassDef {
            name,
            extends,
            members,
        },
    )(i)
}

fn module_member(i: Input) -> Result<DecoratedDef<ModuleMember>> {
    let module_def = decorated_def(map(module_def, ModuleMember::ModuleDef));
    let class_def = decorated_def(map(class_def, ModuleMember::ClassDef));
    let enum_def = decorated_def(map(enum_def, ModuleMember::EnumDef));
    let const_def = semicolon_terminated(decorated_def(map(const_def, ModuleMember::ConstDef)));
    let var_def = semicolon_terminated(decorated_def(map(var_def, ModuleMember::VarDef)));
    let function_def = decorated_def(map(function_def, ModuleMember::FunctionDef));

    alt((
        module_def,
        class_def,
        enum_def,
        const_def,
        var_def,
        function_def,
    ))(i)
}

fn module_def(i: Input) -> Result<ModuleDef> {
    map(
        preceded(
            tag(Token::Module),
            pair(ident, between_brackets(many0(module_member))),
        ),
        |(name, members)| ModuleDef { name, members },
    )(i)
}

fn root_module_def(i: Input) -> Result<RootModuleDef> {
    map(many0(module_member), |members| RootModuleDef { members })(i)
}

fn catch_stmt(i: Input) -> Result<CatchStmt> {
    let variable = map(ident, CatchGuard::Variable);
    let instanceof = map(
        separated_pair(ident, tag(Token::InstanceOf), path),
        |(var, class)| CatchGuard::InstanceOf(var, class),
    );
    let catch_guard = between_parens(alt((instanceof, variable)));
    map(
        preceded(tag(Token::Catch), pair(catch_guard, stmt_block)),
        |(guard, body)| CatchStmt { guard, body },
    )(i)
}

fn throw_stmt(i: Input) -> Result<Stmt> {
    map(preceded(tag(Token::Throw), expr), Stmt::Throw)(i)
}

fn try_stmt(i: Input) -> Result<Stmt> {
    let try_block = preceded(tag(Token::Try), stmt_block);
    let catch_block = many0(catch_stmt);
    let finally_block = opt(preceded(tag(Token::Finally), stmt_block));

    map(
        tuple((try_block, catch_block, finally_block)),
        |(body, catch_body, finally_body)| {
            Stmt::TryStmt(TryStmt {
                body,
                catch_body,
                finally_body,
            })
        },
    )(i)
}

mod tests {
    use crate::lexer::Lexer;

    use super::*;
    use crate::parser::ast::BinOp;

    #[test]
    fn parse_using() {
        let (_, token_input) = Lexer::tokenize("using a.b").unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = using(tokens).unwrap();

        assert_eq!(
            lit,
            Using {
                path: Path(vec![Ident("a"), Ident("b")]),
                alias: None,
            }
        )
    }

    #[test]
    fn parse_using_as() {
        let (_, token_input) = Lexer::tokenize("using a.b as d").unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = using(tokens).unwrap();

        assert_eq!(
            lit,
            Using {
                path: Path(vec![Ident("a"), Ident("b")]),
                alias: Some(Ident("d")),
            }
        )
    }

    #[test]
    fn parse_return_stmt() {
        let (_, token_input) = Lexer::tokenize("return 12").unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = return_stmt(tokens).unwrap();

        assert_eq!(lit, Stmt::Return(Expr::Literal(Literal::Integer(12))))
    }

    #[test]
    fn parse_throw_stmt() {
        let (_, token_input) = Lexer::tokenize("throw 12").unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = throw_stmt(tokens).unwrap();

        assert_eq!(lit, Stmt::Throw(Expr::Literal(Literal::Integer(12))))
    }

    #[test]
    fn parse_var_def() {
        let (_, token_input) = Lexer::tokenize("var a").unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = var_def(tokens).unwrap();

        assert_eq!(
            lit,
            VarDef {
                name: Ident("a"),
                init: None,
            }
        )
    }

    #[test]
    fn parse_const_def() {
        let (_, token_input) = Lexer::tokenize("const AGE = 20").unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = const_def(tokens).unwrap();

        assert_eq!(
            lit,
            ConstDef {
                name: Ident("AGE"),
                init: Expr::Literal(Literal::Integer(20)),
            }
        )
    }

    #[test]
    fn parse_var_init() {
        let (_, token_input) = Lexer::tokenize("var a = 12").unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = var_def(tokens).unwrap();

        assert_eq!(
            lit,
            VarDef {
                name: Ident("a"),
                init: Some(Expr::Literal(Literal::Integer(12))),
            }
        )
    }

    #[test]
    fn parse_if_stmt() {
        let def = r#"
        if(x > 10) {
            return 5;
        }
        "#;

        let (_, token_input) = Lexer::tokenize(def).unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = stmt(tokens).unwrap();

        assert_eq!(
            lit,
            Stmt::IfStmt(Box::new(IfStmt {
                cond: Expr::BinOp(
                    BinOp::Greater,
                    Box::new((Expr::Ident(Ident("x")), Expr::Literal(Literal::Integer(10))))
                ),
                then_branch: Stmt::StmtBlock(vec![Stmt::Return(Expr::Literal(Literal::Integer(
                    5
                )))]),
                else_branch: None
            }))
        )
    }

    #[test]
    fn parse_switch_stmt() {
        let def = r#"
        switch(x) {
            case 1:
                sayHello("Mike");
                break;
            case "bye":
            case instanceof Toybox.Lang.Object:
                sayBye("Ralf");
                break;
            default:
                print("Nothing to do");
        }
        "#;

        let (_, token_input) = Lexer::tokenize(def).unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = switch_stmt(tokens).unwrap();

        assert_eq!(
            lit,
            Stmt::SwitchStmt(SwitchStmt {
                cond: Expr::Ident(Ident("x")),
                case_blocks: vec![
                    CaseBlock {
                        labels: vec![CaseLabel::Literal(Literal::Integer(1))],
                        statements: vec![
                            Stmt::Call(CallExpr {
                                name: Ident("sayHello"),
                                arguments: vec![Expr::Literal(Literal::String("Mike"))],
                            }),
                            Stmt::Break
                        ],
                    },
                    CaseBlock {
                        labels: vec![
                            CaseLabel::Literal(Literal::String("bye")),
                            CaseLabel::InstanceOf(Path(vec![
                                Ident("Toybox"),
                                Ident("Lang"),
                                Ident("Object")
                            ]))
                        ],
                        statements: vec![
                            Stmt::Call(CallExpr {
                                name: Ident("sayBye"),
                                arguments: vec![Expr::Literal(Literal::String("Ralf"))],
                            }),
                            Stmt::Break
                        ],
                    },
                    CaseBlock {
                        labels: vec![CaseLabel::Default],
                        statements: vec![Stmt::Call(CallExpr {
                            name: Ident("print"),
                            arguments: vec![Expr::Literal(Literal::String("Nothing to do"))],
                        })],
                    }
                ],
            })
        )
    }

    #[test]
    fn parse_var_catch_stmt() {
        let def = r#"
        catch(x) {
            break;
        }
        "#;

        let (_, token_input) = Lexer::tokenize(def).unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = catch_stmt(tokens).unwrap();

        assert_eq!(
            lit,
            CatchStmt {
                guard: CatchGuard::Variable(Ident("x")),
                body: vec![Stmt::Break],
            }
        )
    }

    #[test]
    fn parse_instanceof_catch_stmt() {
        let def = r#"
        catch(n instanceof Toybox.Lang.Number) {
            break;
        }
        "#;

        let (_, token_input) = Lexer::tokenize(def).unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = catch_stmt(tokens).unwrap();

        assert_eq!(
            lit,
            CatchStmt {
                guard: CatchGuard::InstanceOf(
                    Ident("n"),
                    Path(vec![Ident("Toybox"), Ident("Lang"), Ident("Number")]),
                ),
                body: vec![Stmt::Break],
            }
        )
    }

    #[test]
    fn parse_try_stmt() {
        let def = r#"
        try {
            // Code to execute
        }
        catch( ex instanceof AnExceptionClass ) {
            // Code to handle the throw of AnExceptionClass
        }
        catch( ex ) {
            // Code to catch all execeptions
        }
        finally {
            // Code to execute when
        }
        "#;

        let (_, token_input) = Lexer::tokenize(def).unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = try_stmt(tokens).unwrap();

        assert_eq!(
            lit,
            Stmt::TryStmt(TryStmt {
                body: vec![],
                catch_body: vec![
                    CatchStmt {
                        guard: CatchGuard::InstanceOf(
                            Ident("ex"),
                            Path(vec![Ident("AnExceptionClass")]),
                        ),
                        body: vec![],
                    },
                    CatchStmt {
                        guard: CatchGuard::Variable(Ident("ex")),
                        body: vec![],
                    }
                ],
                finally_body: Some(vec![]),
            })
        )
    }

    #[test]
    fn parse_enum_def() {
        let def = r#"
            enum {
                x = 1337, // x = 1337
                y,        // y = 1338
                z,        // z = 1339
                a = 0,    // a = 0
                b,        // b = 1
                c         // c = 2
            }
        "#;

        let (_, token_input) = Lexer::tokenize(def).unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = enum_def(tokens).unwrap();

        assert_eq!(
            lit,
            EnumDef(vec![
                InitializableDef {
                    name: Ident("x"),
                    init: Some(Expr::Literal(Literal::Integer(1337))),
                },
                InitializableDef {
                    name: Ident("y"),
                    init: None,
                },
                InitializableDef {
                    name: Ident("z"),
                    init: None,
                },
                InitializableDef {
                    name: Ident("a"),
                    init: Some(Expr::Literal(Literal::Integer(0))),
                },
                InitializableDef {
                    name: Ident("b"),
                    init: None,
                },
                InitializableDef {
                    name: Ident("c"),
                    init: None,
                }
            ])
        )
    }

    #[test]
    fn parse_class_def() {
        let def = r#"
        class MyProjectApp extends App.AppBase
        {
            private const APK = "1232130023";

            enum {
                red,
                green,
                blue
            }

            private var name = "MyApp";

            // onStart() is called on application start up
            function onStart(state) {
            }

            // onStop() is called when your application is exiting
            function onStop(state) {
            }

            // Return the initial view of your application here
            private static function getInitialView() {
                return [ new MyProjectView("Hello world") ];
            }

            // This function is available only in debug mode
            (:debug) function whenDebugging() {
            }
        }
        "#;

        let (_, token_input) = Lexer::tokenize(def).unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = class_def(tokens).unwrap();

        assert_eq!(
            lit,
            ClassDef {
                name: Ident("MyProjectApp"),
                extends: Some(Path(vec![Ident("App"), Ident("AppBase")])),
                members: vec![
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Private,
                        definition: ClassMember::ConstDef(ConstDef {
                            name: Ident("APK"),
                            init: Expr::Literal(Literal::String("1232130023")),
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ClassMember::EnumDef(EnumDef(vec![
                            InitializableDef {
                                name: Ident("red"),
                                init: None,
                            },
                            InitializableDef {
                                name: Ident("green"),
                                init: None,
                            },
                            InitializableDef {
                                name: Ident("blue"),
                                init: None,
                            }
                        ])),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Private,
                        definition: ClassMember::VarDef(VarDef {
                            name: Ident("name"),
                            init: Some(Expr::Literal(Literal::String("MyApp"))),
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ClassMember::FunctionDef(FunctionDef {
                            name: Ident("onStart"),
                            params: vec![Ident("state")],
                            body: vec![],
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ClassMember::FunctionDef(FunctionDef {
                            name: Ident("onStop"),
                            params: vec![Ident("state")],
                            body: vec![],
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Static,
                        visibility: Visibility::Private,
                        definition: ClassMember::FunctionDef(FunctionDef {
                            name: Ident("getInitialView"),
                            params: vec![],
                            body: vec![Stmt::Return(Expr::NewArray(vec![Expr::NewObject(
                                NewObject {
                                    path: Path(vec![Ident("MyProjectView")]),
                                    args: vec![Expr::Literal(Literal::String("Hello world"))],
                                }
                            )]))],
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![Ident("debug")],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ClassMember::FunctionDef(FunctionDef {
                            name: Ident("whenDebugging"),
                            params: vec![],
                            body: vec![],
                        }),
                    }
                ],
            }
        )
    }

    #[test]
    fn parse_module_def() {
        let def = r#"
        module ModuleA
        {
            private const APK = "1232130023";

            enum {
                red,
                green,
                blue
            }

            private var name = "MyApp";

            // This function is available only in debug mode
            (:debug) function whenDebugging() {
            }

            class SomeClass
            {
            }

            module ModuleB
            {
            }
        }
        "#;

        let (_, token_input) = Lexer::tokenize(def).unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = module_def(tokens).unwrap();

        assert_eq!(
            lit,
            ModuleDef {
                name: Ident("ModuleA"),
                members: vec![
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Private,
                        definition: ModuleMember::ConstDef(ConstDef {
                            name: Ident("APK"),
                            init: Expr::Literal(Literal::String("1232130023")),
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::EnumDef(EnumDef(vec![
                            InitializableDef {
                                name: Ident("red"),
                                init: None,
                            },
                            InitializableDef {
                                name: Ident("green"),
                                init: None,
                            },
                            InitializableDef {
                                name: Ident("blue"),
                                init: None,
                            }
                        ])),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Private,
                        definition: ModuleMember::VarDef(VarDef {
                            name: Ident("name"),
                            init: Some(Expr::Literal(Literal::String("MyApp"))),
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![Ident("debug")],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::FunctionDef(FunctionDef {
                            name: Ident("whenDebugging"),
                            params: vec![],
                            body: vec![],
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::ClassDef(ClassDef {
                            name: Ident("SomeClass"),
                            extends: None,
                            members: vec![],
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::ModuleDef(ModuleDef {
                            name: Ident("ModuleB"),
                            members: vec![],
                        }),
                    },
                ],
            }
        )
    }

    #[test]
    fn parse_root_module_def() {
        let def = r#"
          private const APK = "1232130023";

          enum {
             red,
             green,
             blue
          }

          private var name = "MyApp";

          // This function is available only in debug mode
          (:debug) function whenDebugging() {
          }

          class SomeClass
          {
          }

          module ModuleB
          {
          }
        "#;

        let (_, token_input) = Lexer::tokenize(def).unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = root_module_def(tokens).unwrap();

        assert_eq!(
            lit,
            RootModuleDef {
                members: vec![
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Private,
                        definition: ModuleMember::ConstDef(ConstDef {
                            name: Ident("APK"),
                            init: Expr::Literal(Literal::String("1232130023")),
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::EnumDef(EnumDef(vec![
                            InitializableDef {
                                name: Ident("red"),
                                init: None,
                            },
                            InitializableDef {
                                name: Ident("green"),
                                init: None,
                            },
                            InitializableDef {
                                name: Ident("blue"),
                                init: None,
                            }
                        ])),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Private,
                        definition: ModuleMember::VarDef(VarDef {
                            name: Ident("name"),
                            init: Some(Expr::Literal(Literal::String("MyApp"))),
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![Ident("debug")],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::FunctionDef(FunctionDef {
                            name: Ident("whenDebugging"),
                            params: vec![],
                            body: vec![],
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::ClassDef(ClassDef {
                            name: Ident("SomeClass"),
                            extends: None,
                            members: vec![],
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::ModuleDef(ModuleDef {
                            name: Ident("ModuleB"),
                            members: vec![],
                        }),
                    },
                ]
            }
        )
    }
}
