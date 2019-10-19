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
use crate::parser::ast::*;
use crate::parser::expression::{call, expr, group, literal, symbol, symbol_ref_lit};

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

// usingdef
//    : 'using' usingModule=moduleId ('as' name=symbol)? ';'
fn usingdef(i: Input) -> Result<UsingDef> {
    map(
        delimited(
            tag(Token::Using),
            pair(module_id, opt(preceded(tag(Token::As), symbol))),
            tag(Token::Semicolon),
        ),
        |(using_module, name)| UsingDef { using_module, name },
    )(i)
}

// moduleId
//    : symbol
//    | moduleId '.' symbol
pub fn module_id(i: Input) -> Result<ModuleId> {
    map(separated_list(tag(Token::Dot), symbol), ModuleId)(i)
}

fn return_stmt(i: Input) -> Result<Stmt> {
    map(preceded(tag(Token::Return), expr), Stmt::Return)(i)
}

// enumDeclaration
//    : id=symbol  ( '=' initializer=variabledefInitializers)?
fn enum_declaration(i: Input) -> Result<EnumDeclaration> {
    map(
        pair(symbol, opt(preceded(tag(Token::Assign), expr))),
        |(id, initializer)| EnumDeclaration { id, initializer },
    )(i)
}

//enumdef
//    : EnumToken '{' enumDeclaration (',' enumDeclaration)* ','? '}'
fn enumdef(i: Input) -> Result<EnumDef> {
    map(
        preceded(
            tag(Token::Enum),
            between_brackets(separated_list(tag(Token::Comma), enum_declaration)),
        ),
        EnumDef,
    )(i)
}

// variabledefPair
//    : id=symbol ( '=' initializer=variabledefInitializers )?
fn variabledef_pair(i: Input) -> Result<VariableDefPair> {
    map(
        pair(symbol, opt(preceded(tag(Token::Assign), expr))),
        |(name, value)| VariableDefPair { name, value },
    )(i)
}

// variabledef
//    : type=VarToken variabledefPair ( ',' variabledefPair ) * ';'
//    | type=ConstToken variabledefPair ( ',' variabledefPair ) * ';'
//    ;
fn variabledef(i: Input) -> Result<VariableDef> {
    let variable_kind = alt((
        map(tag(Token::Var), |_| VariableKind::Var),
        map(tag(Token::Const), |_| VariableKind::Const),
    ));

    map(
        terminated(
            pair(
                variable_kind,
                separated_list(tag(Token::Comma), variabledef_pair),
            ),
            tag(Token::Semicolon),
        ),
        |(kind, pairs)| VariableDef { kind, pairs },
    )(i)
}

fn call_stmt(i: Input) -> Result<Stmt> {
    map(call, Stmt::Call)(i)
}

fn var_def_stmt(i: Input) -> Result<Stmt> {
    map(variabledef, Stmt::VarDef)(i)
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
        preceded(tag(Token::While), pair(between_parens(expr), statement)),
        |(cond, body)| Stmt::WhileStmt(Box::new(WhileStmt { cond, body })),
    )(i)
}

fn do_while_stmt(i: Input) -> Result<Stmt> {
    map(
        preceded(
            tag(Token::Do),
            pair(
                code_block,
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
                statement,
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
        map(if_statement, Stmt::IfStmt),
        for_stmt,
        while_stmt,
        switch_stmt,
        try_stmt,
        throw_stmt,
    ))(i)
}

// statementSequenceEntry
//    : statementSequence?
//    | codeBlock

fn statement(i: Input) -> Result<Stmt> {
    let line_stmt = semicolon_terminated(terminal_stmt);
    alt((line_stmt, structured_stmt))(i)
}

//statementSequence
//    : statement statementSequence?
//    | codeBlock statementSequence?
fn statement_sequence(i: Input) -> Result<CodeBlock> {
    many1(statement)(i)
}

//codeBlock
//    :  '{' '}'
//    | '{' statementSequence '}'
fn code_block(i: Input) -> Result<CodeBlock> {
    between_brackets(statement_sequence)(i)
}

// ifStatement
//    : IfToken '(' expression ')'
//        ifCase = codeBlock ( ElseToken ( elseIfCase=ifStatement | elseCase=codeBlock ) )?
fn if_statement(i: Input) -> Result<IfStatement> {
    let else_case = alt((
        map(if_statement, |stmt| vec![Stmt::IfStmt(stmt)]),
        code_block,
    ));
    map(
        tuple((
            preceded(tag(Token::If), group),
            code_block,
            opt(preceded(tag(Token::Else), else_case)),
        )),
        |(cond, true_branch, false_branch)| IfStatement {
            cond,
            true_branch,
            false_branch: false_branch.unwrap_or_default(),
        },
    )(i)
}

fn case_label(i: Input) -> Result<CaseLabel> {
    terminated(
        alt((
            map(preceded(tag(Token::Case), literal), CaseLabel::Literal),
            map(
                preceded(pair(tag(Token::Case), tag(Token::InstanceOf)), module_id),
                CaseLabel::InstanceOf,
            ),
            map(tag(Token::Default), |_| CaseLabel::Default),
        )),
        tag(Token::Colon),
    )(i)
}

fn case_block(i: Input) -> Result<CaseBlock> {
    map(
        pair(many1(case_label), many1(statement)),
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

// paramDecl
//    : '(' ')'
//    | '('  Id ( ',' Id )*  ')'
fn param_decl(i: Input) -> Result<Vec<Id>> {
    between_parens(separated_list(tag(Token::Comma), symbol))(i)
}

// functiondef
//    : FunctionToken id=symbol paramDecl (codeBlock | ';')
fn functiondef(i: Input) -> Result<FunctionDef> {
    map(
        tuple((
            preceded(tag(Token::Function), symbol),
            param_decl,
            code_block,
        )),
        |(name, params, body)| FunctionDef {
            id: name,
            params,
            body,
        },
    )(i)
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

fn annotations(i: Input) -> Result<Vec<Id>> {
    many0(between_parens(symbol_ref_lit))(i)
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

// declarationFlags
//    : 'hidden'
//    | 'static'
//    | 'native'
//    | 'public'
//    | 'private'
//    | 'protected'
fn declaration_flags(i: Input) -> Result<DeclarationFlag> {
    alt((
        map(tag(Token::Hidden), |_| DeclarationFlag::Hidden),
        map(tag(Token::Static), |_| DeclarationFlag::Static),
        map(tag(Token::Native), |_| DeclarationFlag::Native),
        map(tag(Token::Public), |_| DeclarationFlag::Public),
        map(tag(Token::Private), |_| DeclarationFlag::Private),
        map(tag(Token::Protected), |_| DeclarationFlag::Protected),
    ))(i)
}

// classDeclaration
//    : annotations? flags=declarationFlags* (classdef | functiondef | variabledef | enumdef )
//    | usingdef
fn class_declaration(i: Input) -> Result<ClassDeclaration> {
    let class_member = alt((
        map(classdef, ClassMember::ClassDef),
        map(functiondef, ClassMember::FunctionDef),
        map(variabledef, ClassMember::VariableDef),
        map(enumdef, ClassMember::EnumDef),
    ));
    let decorated_class_declaration = map(
        tuple((annotations, many0(declaration_flags), class_member)),
        |(annotations, flags, def)| ClassDeclaration::ClassMember {
            annotations,
            flags,
            def,
        },
    );
    let usingdef_class_declaration = map(usingdef, ClassDeclaration::UsingDef);
    alt((decorated_class_declaration, usingdef_class_declaration))(i)
}

// classdef
//    : ClassToken id=symbol ( 'extends' extendsValue=moduleId )? '{' classDeclaration* '}'
fn classdef(i: Input) -> Result<ClassDef> {
    map(
        tuple((
            preceded(tag(Token::Class), symbol),
            opt(preceded(tag(Token::Extends), module_id)),
            between_brackets(many0(class_declaration)),
        )),
        |(id, extends_value, members)| ClassDef {
            id,
            extends_value,
            members,
        },
    )(i)
}

fn module_member(i: Input) -> Result<DecoratedDef<ModuleMember>> {
    let module_def = decorated_def(map(module_def, ModuleMember::ModuleDef));
    let class_def = decorated_def(map(classdef, ModuleMember::ClassDef));
    let enum_def = decorated_def(map(enumdef, ModuleMember::EnumDef));
    let var_def = semicolon_terminated(decorated_def(map(variabledef, ModuleMember::VarDef)));
    let function_def = decorated_def(map(functiondef, ModuleMember::FunctionDef));

    alt((module_def, class_def, enum_def, var_def, function_def))(i)
}

fn module_def(i: Input) -> Result<ModuleDef> {
    map(
        preceded(
            tag(Token::Module),
            pair(symbol, between_brackets(many0(module_member))),
        ),
        |(name, members)| ModuleDef { name, members },
    )(i)
}

fn root_module_def(i: Input) -> Result<RootModuleDef> {
    map(many0(module_member), |members| RootModuleDef { members })(i)
}

fn catch_stmt(i: Input) -> Result<CatchStmt> {
    let variable = map(symbol, CatchGuard::Variable);
    let instanceof = map(
        separated_pair(symbol, tag(Token::InstanceOf), module_id),
        |(var, class)| CatchGuard::InstanceOf(var, class),
    );
    let catch_guard = between_parens(alt((instanceof, variable)));
    map(
        preceded(tag(Token::Catch), pair(catch_guard, code_block)),
        |(guard, body)| CatchStmt { guard, body },
    )(i)
}

fn throw_stmt(i: Input) -> Result<Stmt> {
    map(preceded(tag(Token::Throw), expr), Stmt::Throw)(i)
}

fn try_stmt(i: Input) -> Result<Stmt> {
    let try_block = preceded(tag(Token::Try), code_block);
    let catch_block = many0(catch_stmt);
    let finally_block = opt(preceded(tag(Token::Finally), code_block));

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
    fn parse_using() {
        let text = "using a.b;";
        let tree = UsingDef {
            using_module: ModuleId(vec![("a"), ("b")]),
            name: None,
        };
        assert_parsing!(usingdef, text, tree)
    }

    #[test]
    fn parse_using_as() {
        let text = "using a.b as d;";
        let tree = UsingDef {
            using_module: ModuleId(vec!["a", "b"]),
            name: Some(("d")),
        };
        assert_parsing!(usingdef, text, tree)
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
    fn parse_uninitialized_var() {
        let text = "var alpha;";
        let tree = VariableDef {
            kind: VariableKind::Var,
            pairs: vec![VariableDefPair {
                name: "alpha",
                value: None,
            }],
        };
        assert_parsing!(variabledef, text, tree);
    }

    #[test]
    fn parse_uninitialized_const() {
        // Monkey C allows this weirdness
        let text = "const AGE;";
        let tree = VariableDef {
            kind: VariableKind::Const,
            pairs: vec![VariableDefPair {
                name: "AGE",
                value: None,
            }],
        };
        assert_parsing!(variabledef, text, tree);
    }

    #[test]
    fn parse_initialized_var() {
        let text = "var alpha = 12, beta;";
        let tree = VariableDef {
            kind: VariableKind::Var,
            pairs: vec![
                VariableDefPair {
                    name: "alpha",
                    value: Some(Expr::Literal(Literal::Integer(12))),
                },
                VariableDefPair {
                    name: "beta",
                    value: None,
                },
            ],
        };
        assert_parsing!(variabledef, text, tree);
    }

    #[test]
    fn parse_if_statement() {
        let text = r#"
            if (x > 10) {
                return 5;
            }
        "#;
        let tree = IfStatement {
            cond: Expr::BinOp(
                BinOp::Greater,
                Box::new((
                    Expr::Literal(Literal::Symbol("x")),
                    Expr::Literal(Literal::Integer(10)),
                )),
            ),
            true_branch: vec![Stmt::Return(Expr::Literal(Literal::Integer(5)))],
            false_branch: vec![],
        };
        assert_parsing!(if_statement, text, tree);
    }

    #[test]
    fn parse_if_else_statement() {
        let text = r#"
            if (x > 10) {
                return 5;
            } else {
                return 6;
            }
        "#;
        let tree = IfStatement {
            cond: Expr::BinOp(
                BinOp::Greater,
                Box::new((
                    Expr::Literal(Literal::Symbol("x")),
                    Expr::Literal(Literal::Integer(10)),
                )),
            ),
            true_branch: vec![Stmt::Return(Expr::Literal(Literal::Integer(5)))],
            false_branch: vec![Stmt::Return(Expr::Literal(Literal::Integer(6)))],
        };
        assert_parsing!(if_statement, text, tree);
    }

    #[test]
    fn parse_if_else_if_statement() {
        let text = r#"
            if (x > 10) {
                return 5;
            } else if (y == 12) {
                return 6;
            }
        "#;
        let tree = IfStatement {
            cond: Expr::BinOp(
                BinOp::Greater,
                Box::new((
                    Expr::Literal(Literal::Symbol("x")),
                    Expr::Literal(Literal::Integer(10)),
                )),
            ),
            true_branch: vec![Stmt::Return(Expr::Literal(Literal::Integer(5)))],
            false_branch: vec![Stmt::IfStmt(IfStatement {
                cond: Expr::BinOp(
                    BinOp::Equal,
                    Box::new((
                        Expr::Literal(Literal::Symbol("y")),
                        Expr::Literal(Literal::Integer(12)),
                    )),
                ),
                true_branch: vec![Stmt::Return(Expr::Literal(Literal::Integer(6)))],
                false_branch: vec![],
            })],
        };
        assert_parsing!(if_statement, text, tree);
    }

    /*
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
                cond: Expr::Literal(Literal::Symbol("x")),
                case_blocks: vec![
                    CaseBlock {
                        labels: vec![CaseLabel::Literal(Literal::Integer(1))],
                        statements: vec![
                            Stmt::Call(CallExpr {
                                name: "sayHello",
                                arguments: vec![Expr::Literal(Literal::String("Mike"))],
                            }),
                            Stmt::Break
                        ],
                    },
                    CaseBlock {
                        labels: vec![
                            CaseLabel::Literal(Literal::String("bye")),
                            CaseLabel::InstanceOf(Path(vec!["Toybox", "Lang", "Object"]))
                        ],
                        statements: vec![
                            Stmt::Call(CallExpr {
                                name: "sayBye",
                                arguments: vec![Expr::Literal(Literal::String("Ralf"))],
                            }),
                            Stmt::Break
                        ],
                    },
                    CaseBlock {
                        labels: vec![CaseLabel::Default],
                        statements: vec![Stmt::Call(CallExpr {
                            name: "print",
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
                guard: CatchGuard::Variable("x"),
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
                guard: CatchGuard::InstanceOf("n", Path(vec!["Toybox", "Lang", "Number"]),),
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
                        guard: CatchGuard::InstanceOf("ex", Path(vec!["AnExceptionClass"]),),
                        body: vec![],
                    },
                    CatchStmt {
                        guard: CatchGuard::Variable("ex"),
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
                    name: "x",
                    init: Some(Expr::Literal(Literal::Integer(1337))),
                },
                InitializableDef {
                    name: "y",
                    init: None,
                },
                InitializableDef {
                    name: "z",
                    init: None,
                },
                InitializableDef {
                    name: "a",
                    init: Some(Expr::Literal(Literal::Integer(0))),
                },
                InitializableDef {
                    name: "b",
                    init: None,
                },
                InitializableDef {
                    name: "c",
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
                red ,
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

            class Inner {
            }
        }
        "#;

        let (_, token_input) = Lexer::tokenize(def).unwrap();
        let tokens = Tokens::new(&token_input);
        let (_, lit) = class_def(tokens).unwrap();

        assert_eq!(
            lit,
            ClassDef {
                id: "MyProjectApp",
                extends_value: Some(Path(vec!["App", "AppBase"])),
                members: vec![
                    ClassDeclaration::ClassMember {
                        annotations: vec![],
                        flags: vec![DeclarationFlag::Private],
                        def: ClassMember::(ConstDef {
                            name: "APK",
                            init: Expr::Literal(Literal::String("1232130023")),
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ClassDeclaration::EnumDef(EnumDef(vec![
                            InitializableDef {
                                name: "red",
                                init: None,
                            },
                            InitializableDef {
                                name: "green",
                                init: None,
                            },
                            InitializableDef {
                                name: "blue",
                                init: None,
                            }
                        ])),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Private,
                        definition: ClassDeclaration::VariableDef(VariableDef(vec![InitializableDef {
                            name: "name",
                            init: Some(Expr::Literal(Literal::String("MyApp"))),
                        }])),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ClassDeclaration::FunctionDef(FunctionDef {
                            name: ("onStart"),
                            params: vec!["state"],
                            body: vec![],
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ClassDeclaration::FunctionDef(FunctionDef {
                            name: ("onStop"),
                            params: vec![("state")],
                            body: vec![],
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Static,
                        visibility: Visibility::Private,
                        definition: ClassDeclaration::FunctionDef(FunctionDef {
                            name: ("getInitialView"),
                            params: vec![],
                            body: vec![Stmt::Return(Expr::NewArray(vec![Expr::NewObject(
                                NewObject {
                                    path: Path(vec![("MyProjectView")]),
                                    args: vec![Expr::Literal(Literal::String("Hello world"))],
                                }
                            )]))],
                        }),
                    },
                    DecoratedDef {
                        annotations: vec!["debug"],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ClassDeclaration::FunctionDef(FunctionDef {
                            name: "whenDebugging",
                            params: vec![],
                            body: vec![],
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ClassDeclaration(ClassDef {
                            id: "Inner",
                            extends_value: None,
                            members: vec![]
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
                name: "ModuleA",
                members: vec![
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Private,
                        definition: ModuleMember::ConstDef(ConstDef {
                            name: "APK",
                            init: Expr::Literal(Literal::String("1232130023")),
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::EnumDef(EnumDef(vec![
                            InitializableDef {
                                name: "red",
                                init: None,
                            },
                            InitializableDef {
                                name: "green",
                                init: None,
                            },
                            InitializableDef {
                                name: "blue",
                                init: None,
                            }
                        ])),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Private,
                        definition: ModuleMember::VarDef(VariableDef(vec![InitializableDef {
                            name: ("name"),
                            init: Some(Expr::Literal(Literal::String("MyApp"))),
                        }])),
                    },
                    DecoratedDef {
                        annotations: vec!["debug"],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::FunctionDef(FunctionDef {
                            name: "whenDebugging",
                            params: vec![],
                            body: vec![],
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::ClassDef(ClassDef {
                            id: "SomeClass",
                            extends_value: None,
                            members: vec![],
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::ModuleDef(ModuleDef {
                            name: "ModuleB",
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
                            name: ("APK"),
                            init: Expr::Literal(Literal::String("1232130023")),
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::EnumDef(EnumDef(vec![
                            InitializableDef {
                                name: "red",
                                init: None,
                            },
                            InitializableDef {
                                name: "green",
                                init: None,
                            },
                            InitializableDef {
                                name: "blue",
                                init: None,
                            }
                        ])),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Private,
                        definition: ModuleMember::VarDef(VariableDef(vec![InitializableDef {
                            name: "name",
                            init: Some(Expr::Literal(Literal::String("MyApp"))),
                        }])),
                    },
                    DecoratedDef {
                        annotations: vec!["debug"],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::FunctionDef(FunctionDef {
                            name: "whenDebugging",
                            params: vec![],
                            body: vec![],
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::ClassDef(ClassDef {
                            id: "SomeClass",
                            extends_value: None,
                            members: vec![],
                        }),
                    },
                    DecoratedDef {
                        annotations: vec![],
                        scope: Scope::Instance,
                        visibility: Visibility::Public,
                        definition: ModuleMember::ModuleDef(ModuleDef {
                            name: "ModuleB",
                            members: vec![],
                        }),
                    },
                ]
            }
        )
    }
    */
}
