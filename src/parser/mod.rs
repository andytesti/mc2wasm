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
use crate::parser::ast::BinOp::BitAnd;
use crate::parser::ast::*;
use crate::parser::expr::{
    call, creation, expression, factor, group, literal, lvalue, symbol, symbol_ref,
};

pub mod ast;
mod expr;

type Input<'a> = Tokens<'a>;
type Result<'a, Output> = IResult<Input<'a>, Output>;

pub fn map_from<'a, P, F, T>(p: P) -> impl Fn(Input<'a>) -> Result<'a, T>
where
    P: Fn(Input<'a>) -> Result<F>,
    T: 'a + From<F>,
    F: 'a,
{
    map(p, T::from)
}

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

// returnStatement
//    : 'return' op=expression? ';'
fn return_statement(i: Input) -> Result<Statement> {
    map(
        semicolon_terminated(preceded(tag(Token::Return), opt(expression))),
        Statement::Return,
    )(i)
}

// enumDeclaration
//    : id=symbol  ( '=' initializer=variabledefInitializers)?
fn enum_declaration(i: Input) -> Result<EnumDeclaration> {
    map(
        pair(symbol, opt(preceded(tag(Token::Assign), expression))),
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
        pair(symbol, opt(preceded(tag(Token::Assign), expression))),
        |(name, value)| VariableDefPair { name, value },
    )(i)
}

// varDeclaration
//    : VarToken varDef ( ',' varDef )*
fn var_declaration(i: Input) -> Result<VariableDef> {
    map(
        preceded(
            tag(Token::Var),
            separated_list(tag(Token::Comma), variabledef_pair),
        ),
        VariableDef,
    )(i)
}

// variabledef
//    : type=VarToken variabledefPair ( ',' variabledefPair ) * ';'
//    | type=ConstToken variabledefPair ( ',' variabledefPair ) * ';'
//    ;
fn variabledef(i: Input) -> Result<VariableDef> {
    semicolon_terminated(var_declaration)(i)
}

fn const_declaration(i: Input) -> Result<ConstantDef> {
    map(
        preceded(
            tag(Token::Const),
            separated_list(tag(Token::Comma), variabledef_pair),
        ),
        ConstantDef,
    )(i)
}

fn constantdef(i: Input) -> Result<ConstantDef> {
    semicolon_terminated(const_declaration)(i)
}

fn var_def_stmt(i: Input) -> Result<Statement> {
    map(variabledef, Statement::VarDeclaration)(i)
}

// continueStatement
//    : 'continue'
fn continue_statement(i: Input) -> Result<Statement> {
    map(tag(Token::Continue), |_| Statement::Continue)(i)
}

// breakStatement
//    : 'break'
fn break_statement(i: Input) -> Result<Statement> {
    map(tag(Token::Break), |_| Statement::Break)(i)
}

// whileStatement
//     :  WhileToken '(' expression')' whileCode=codeBlock
fn while_statement(i: Input) -> Result<WhileStatement> {
    map(
        preceded(
            tag(Token::While),
            pair(between_parens(expression), code_block),
        ),
        |(cond, body)| WhileStatement { cond, body },
    )(i)
}

// doStatement
//    : DoToken whileCode=codeBlock WhileToken '(' expression ')'
fn do_statement(i: Input) -> Result<DoStatement> {
    map(
        preceded(
            tag(Token::Do),
            pair(
                code_block,
                preceded(tag(Token::While), between_parens(expression)),
            ),
        ),
        |(body, cond)| DoStatement { cond, body },
    )(i)
}

// forStatement
//    : ForToken '(' initialize=forInitialize? ';' test=expression? ';' increment=forIncrement? ')' forCode=codeBlock
fn for_statement(i: Input) -> Result<ForStatement> {
    let for_header = between_parens(tuple((
        semicolon_terminated(opt(for_initialize)),
        semicolon_terminated(opt(expression)),
        for_increment,
    )));
    map(
        pair(preceded(tag(Token::For), for_header), code_block),
        |((initialize, test, increment), body)| ForStatement {
            initialize,
            test,
            increment,
            body,
        },
    )(i)
}

// forInitialize
//    : assignment ( ',' assignment )*
//    | varDeclaration
fn for_initialize(i: Input) -> Result<ForInitialize> {
    use ForInitialize::*;
    alt((
        map(var_declaration, VarDeclaration),
        map(separated_list(tag(Token::Comma), assignment), Assignments),
    ))(i)
}

//forIncrement
//    : assignment ( ',' assignment )*
fn for_increment(i: Input) -> Result<Vec<Assignment>> {
    separated_list(tag(Token::Comma), assignment)(i)
}

// assignment
//    : lvalue op=assignmentOperator expression
//    | incrDecr lvalue
//    | lvalue incrDecr
fn assignment(i: Input) -> Result<Assignment> {
    use Assignment::*;
    let infix = map(
        tuple((lvalue, assignment_operator, expression)),
        |(lvalue, operator, value)| Infix(operator, lvalue, value),
    );
    let prefix = map(pair(incr_decr, lvalue), |(op, value)| Prefix(op, value));
    let postfix = map(pair(lvalue, incr_decr), |(value, op)| Postfix(op, value));
    alt((infix, prefix, postfix))(i)
}

// incrDecr
//    : '++'
//    | '--'
fn incr_decr(i: Input) -> Result<IncOp> {
    alt((
        map(tag(Token::Inc), |_| IncOp::Add),
        map(tag(Token::Dec), |_| IncOp::Sub),
    ))(i)
}

// assignmentOperator
//    : '='
//    | '*='
//    | '/='
//    | '%='
//    | '+='
//    | '-='
//    | '>>='
//    | '<<='
//    | '&='
//    | '^='
//    | '|='
fn assignment_operator(i: Input) -> Result<AssignmentOp> {
    use AssignmentOp::*;
    alt((
        map(tag(Token::Assign), |_| Simple),
        map(tag(Token::MulAssign), |_| Mul),
        map(tag(Token::DivAssign), |_| Div),
        map(tag(Token::ModuleAssign), |_| Module),
        map(tag(Token::AddAssign), |_| Add),
        map(tag(Token::SubAssign), |_| Sub),
        map(tag(Token::SRightAssign), |_| SRight),
        map(tag(Token::SLeftAssign), |_| SLeft),
        map(tag(Token::BitAndAssign), |_| BitAnd),
        map(tag(Token::BitXorAssign), |_| BitXor),
        map(tag(Token::BitOrAssign), |_| BitOr),
    ))(i)
}

// procedureCall
//    : factor
#[inline]
fn procedure_call(i: Input) -> Result<Statement> {
    map_from(factor)(i)
}

// statementSequenceEntry
//    : statementSequence?
//    | codeBlock

// statement
//    : assignment ';'
//    | procedureCall ';'
//    | varDeclaration ';'
//    | ifStatement
//    | whileStatement
//    | doStatement ';'
//    | forStatement
//    | switchStatement
//    | breakStatement ';'
//    | continueStatement ';'
//    | throwStatement ';'
//    | tryStatement
//    | returnStatement
fn statement(i: Input) -> Result<Statement> {
    alt((
        map_from(semicolon_terminated(assignment)),
        semicolon_terminated(procedure_call),
        map_from(semicolon_terminated(var_declaration)),
        map_from(semicolon_terminated(const_declaration)),
        map_from(if_statement),
        map_from(while_statement),
        map_from(semicolon_terminated(do_statement)),
        // switch_statement
        semicolon_terminated(break_statement),
        semicolon_terminated(continue_statement),
        map_from(semicolon_terminated(throw_statement)),
        // try_statement
        return_statement,
    ))(i)
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
        map(if_statement, |stmt| vec![Statement::If(stmt)]),
        code_block,
    ));
    map(
        tuple((
            preceded(tag(Token::If), group),
            code_block,
            opt(preceded(tag(Token::Else), else_case)),
        )),
        |(cond, true_branch, false_branch)| IfStatement {
            test: cond,
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

fn switch_stmt(i: Input) -> Result<Statement> {
    map(
        preceded(
            tag(Token::Switch),
            pair(
                between_parens(expression),
                between_brackets(many1(case_block)),
            ),
        ),
        |(cond, case_blocks)| Statement::Switch(SwitchStatement { cond, case_blocks }),
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
    let body = alt((map(tag(Token::Semicolon), |_| None), map(code_block, Some)));
    map(
        tuple((preceded(tag(Token::Function), symbol), param_decl, body)),
        |(name, params, body)| FunctionDef {
            id: name,
            params,
            body,
        },
    )(i)
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

fn annotations(i: Input) -> Result<Vec<SymbolRef>> {
    many0(between_parens(symbol_ref))(i)
}

fn declaration<'a, T: 'a>(
    memberdef: impl Fn(Input<'a>) -> Result<T>,
) -> impl Fn(Input<'a>) -> Result<Declaration<'a, T>> {
    alt((
        map(usingdef, Declaration::UsingDef),
        map(
            tuple((annotations, many0(declaration_flags), memberdef)),
            |(annotations, flags, definition)| Declaration::MemberDef {
                annotations,
                flags,
                definition,
            },
        ),
    ))
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

// program : moduleDeclaration * EOF;
fn program(i: Input) -> Result<Program> {
    map(many0(module_declaration), |members| Program { members })(i)
}

// moduleDeclaration
//    : annotations? flags=declarationFlags* (classdef | moduledef | functiondef | enumdef | variabledef )
//    | usingdef
fn module_declaration(i: Input) -> Result<ModuleDeclaration> {
    use ModuleMember::*;
    let module_member = alt((
        map_from(classdef),
        map_from(module_def),
        map_from(functiondef),
        map_from(variabledef),
        map_from(constantdef),
        map_from(enumdef),
    ));
    declaration(module_member)(i)
}

// classDeclaration
//    : annotations? flags=declarationFlags* (classdef | functiondef | variabledef | enumdef )
//    | usingdef
fn class_declaration(i: Input) -> Result<ClassDeclaration> {
    let class_member = alt((
        map_from(classdef),
        map_from(functiondef),
        map_from(variabledef),
        map_from(constantdef),
        map_from(enumdef),
    ));
    declaration(class_member)(i)
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

// moduledef
//    : ModuleToken id=symbol '{' moduleDeclaration* '}'
fn module_def(i: Input) -> Result<ModuleDef> {
    map(
        preceded(
            tag(Token::Module),
            pair(symbol, between_brackets(many0(module_declaration))),
        ),
        |(id, members)| ModuleDef { id, members },
    )(i)
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

// throwStatement: ThrowToken ( lvalue | creation );
fn throw_statement(i: Input) -> Result<ThrowStatement> {
    let value = alt((
        map(lvalue, ThrowStatement::LValue),
        map(creation, ThrowStatement::Creation),
    ));
    preceded(tag(Token::Throw), value)(i)
}

fn try_stmt(i: Input) -> Result<Statement> {
    let try_block = preceded(tag(Token::Try), code_block);
    let catch_block = many0(catch_stmt);
    let finally_block = opt(preceded(tag(Token::Finally), code_block));

    map(
        tuple((try_block, catch_block, finally_block)),
        |(body, catch_body, finally_body)| {
            Statement::Try(TryStatement {
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
            assert_parsing!(statement, $source, $expected.into())
        };
    }

    #[test]
    fn parse_using() {
        let text = "using a.b;";
        let tree = UsingDef {
            using_module: ModuleId(vec![Id("a"), Id("b")]),
            name: None,
        };
        assert_parsing!(usingdef, text, tree)
    }

    #[test]
    fn parse_using_as() {
        let text = "using a.b as d;";
        let tree = UsingDef {
            using_module: ModuleId(vec![Id("a"), Id("b")]),
            name: Some(Id("d")),
        };
        assert_parsing!(usingdef, text, tree)
    }

    #[test]
    fn parse_return_stmt() {
        let text = "return 12;";
        let tree = Statement::Return(Some(12.into()));
        assert_parsing!(text, tree)
    }

    #[test]
    fn parse_throw_stmt() {
        let text = "throw exception";
        let tree = ThrowStatement::LValue(LValue {
            scope: Scope::Local,
            extension: LValueExtension::Select(Id("exception"), None),
        });
        assert_parsing!(throw_statement, text, tree)
    }

    #[test]
    fn parse_uninitialized_var() {
        let text = "var alpha;";
        let tree = VariableDef(vec![VariableDefPair {
            name: Id("alpha"),
            value: None,
        }]);
        assert_parsing!(variabledef, text, tree);
    }

    #[test]
    fn parse_uninitialized_const() {
        // Monkey C allows this weirdness
        let text = "const AGE;";
        let tree = ConstantDef(vec![VariableDefPair {
            name: Id("AGE"),
            value: None,
        }]);
        assert_parsing!(text, tree);
    }

    #[test]
    fn parse_variabledef_pair() {
        let text = "alpha = 12";
        let tree = VariableDefPair {
            name: Id("alpha"),
            value: Some(12.into()),
        };
        assert_parsing!(variabledef_pair, text, tree)
    }

    #[test]
    fn parse_var_declaration() {
        let text = "var alpha = 12, beta";
        let tree = VariableDef(vec![
            VariableDefPair {
                name: Id("alpha"),
                value: Some(12.into()),
            },
            VariableDefPair {
                name: Id("beta"),
                value: None,
            },
        ]);
        assert_parsing!(var_declaration, text, tree);
    }

    #[test]
    fn parse_initialized_var() {
        let text = "var alpha = 12, beta;";
        let tree = VariableDef(vec![
            VariableDefPair {
                name: Id("alpha"),
                value: Some(12.into()),
            },
            VariableDefPair {
                name: Id("beta"),
                value: None,
            },
        ]);
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
            test: Expression::BinOp(BinOp::Greater, Box::new((Id("x").into(), 10.into()))),
            true_branch: vec![Statement::Return(Some(5.into()))],
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
            test: Expression::BinOp(BinOp::Greater, Box::new((Id("x").into(), 10.into()))),
            true_branch: vec![Statement::Return(Some(5.into()))],
            false_branch: vec![Statement::Return(Some(6.into()))],
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
            test: Expression::BinOp(BinOp::Greater, Box::new((Id("x").into(), 10.into()))),
            true_branch: vec![Statement::Return(Some(5.into()))],
            false_branch: vec![IfStatement {
                test: Expression::BinOp(BinOp::Equal, Box::new((Id("y").into(), 12.into()))),
                true_branch: vec![Statement::Return(Some(6.into()))],
                false_branch: vec![],
            }
            .into()],
        };
        assert_parsing!(if_statement, text, tree);
    }

    #[test]
    fn parse_while_statement() {
        let text = r#"
            while (x > 10) {
                break;
            }
        "#;
        let tree = WhileStatement {
            cond: Expression::BinOp(BinOp::Greater, Box::new((Id("x").into(), 10.into()))),
            body: vec![Statement::Break],
        };
        assert_parsing!(while_statement, text, tree);
    }

    #[test]
    fn parse_do_statement() {
        let text = r#"
            do {
                break;
            } while (x > 10);
        "#;
        let tree = DoStatement {
            cond: Expression::BinOp(BinOp::Greater, Box::new((Id("x").into(), 10.into()))),
            body: vec![Statement::Break],
        };
        assert_parsing!(do_statement, text, tree);
    }

    #[test]
    fn parse_for_initialize_declaration() {
        let text = "var alpha = 12, beta";
        let tree = ForInitialize::VarDeclaration(VariableDef(vec![
            VariableDefPair {
                name: Id("alpha"),
                value: Some(12.into()),
            },
            VariableDefPair {
                name: Id("beta"),
                value: None,
            },
        ]));
        assert_parsing!(for_initialize, text, tree);
    }

    #[test]
    fn parse_for_initialize_assignments() {
        let text = "alpha = 12, ++beta";
        let tree = ForInitialize::Assignments(vec![
            Assignment::Infix(
                AssignmentOp::Simple,
                LValue {
                    scope: Scope::Local,
                    extension: LValueExtension::Select(Id("alpha"), None),
                },
                12.into(),
            ),
            Assignment::Prefix(
                IncOp::Add,
                LValue {
                    scope: Scope::Local,
                    extension: LValueExtension::Select(Id("beta"), None),
                },
            ),
        ]);
        assert_parsing!(for_initialize, text, tree);
    }

    #[test]
    fn parse_for_statement() {
        let text = r#"
            for(var i = 1; i < 10; i++) {
                break;
            }
        "#;
        let tree = ForStatement {
            initialize: Some(ForInitialize::VarDeclaration(VariableDef(vec![
                VariableDefPair {
                    name: Id("i"),
                    value: Some(1.into()),
                },
            ]))),
            test: Some(Expression::BinOp(
                BinOp::Less,
                Box::new((Id("i").into(), 10.into())),
            )),
            increment: vec![Assignment::Postfix(
                IncOp::Add,
                LValue {
                    scope: Scope::Local,
                    extension: LValueExtension::Select(Id("i"), None),
                },
            )],
            body: vec![Statement::Break],
        };
        assert_parsing!(for_statement, text, tree);
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
