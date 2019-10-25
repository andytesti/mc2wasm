use std::collections::HashMap;

macro_rules! impl_from {
    ($parent:ident <- $($child:ident),*) => {
        $(impl<'a> From<$child<'a>> for $parent<'a> {
            fn from(c: $child<'a>) -> Self {
                $parent::$child(c)
            }
        })*
    };

    ($parent:ident <- $($child:ident -> $alias:ident),*) => {
        $(impl<'a> From<$child<'a>> for $parent<'a> {
            fn from(c: $child<'a>) -> Self {
                $parent::$alias(c)
            }
        })*
    };

    ($parent:ident <- box $($child:ident),*) => {
        $(impl<'a> From<$child<'a>> for $parent<'a> {
            fn from(c: $child<'a>) -> Self {
                $parent::$child(Box::new(c))
            }
        })*
    };
}

#[derive(Debug, PartialEq, Clone)]
pub struct Id<'a>(pub &'a str);

#[derive(Debug, PartialEq, Clone)]
pub struct ModuleId<'a>(pub Vec<Id<'a>>);

#[derive(Debug, PartialEq, Clone)]
pub enum Visibility {
    Private,
    Public,
    Protected,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableDefPair<'a> {
    pub name: Id<'a>,
    pub value: Option<Expression<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClassDef<'a> {
    pub id: Id<'a>,
    pub extends_value: Option<ModuleId<'a>>,
    pub members: Vec<ClassDeclaration<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumDeclaration<'a> {
    pub id: Id<'a>,
    pub initializer: Option<Expression<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumDef<'a>(pub Vec<EnumDeclaration<'a>>);

#[derive(Debug, PartialEq, Clone)]
pub enum AssignmentOp {
    Simple,
    Add,
    Sub,
    Div,
    Mul,
    SLeft,
    SRight,
    BitAnd,
    BitOr,
    BitXor,
    Module,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IncOp {
    Add,
    Sub,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Assignment<'a> {
    Infix(AssignmentOp, Expression<'a>, Expression<'a>),
    Prefix(IncOp, Expression<'a>),
    Postfix(IncOp, Expression<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum VariableKind {
    Var,
    Const,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableDef<'a>(pub Vec<VariableDefPair<'a>>);

#[derive(Debug, PartialEq, Clone)]
pub struct ConstantDef<'a>(pub Vec<VariableDefPair<'a>>);

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDef<'a> {
    pub id: Id<'a>,
    pub params: Vec<Id<'a>>,
    pub body: Option<Sequence<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Call<'a>(pub Id<'a>, pub Vec<Expression<'a>>);

#[derive(Debug, PartialEq, Clone)]
pub struct Invoke<'a>(pub Expression<'a>, pub Id<'a>, pub Vec<Expression<'a>>);

#[derive(Debug, PartialEq, Clone)]
pub struct Ternary<'a> {
    pub condition: Expression<'a>,
    pub then_branch: Expression<'a>,
    pub else_branch: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ModuleMember<'a> {
    VariableDef(VariableDef<'a>),
    ConstantDef(ConstantDef<'a>),
    EnumDef(EnumDef<'a>),
    FunctionDef(FunctionDef<'a>),
    ClassDef(ClassDef<'a>),
    ModuleDef(ModuleDef<'a>),
}

impl_from!(ModuleMember <-
    ConstantDef,
    VariableDef,
    FunctionDef,
    EnumDef,
    ClassDef,
    ModuleDef
);

#[derive(Debug, PartialEq, Clone)]
pub struct ModuleDef<'a> {
    pub id: Id<'a>,
    pub members: Vec<ModuleDeclaration<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UsingDef<'a> {
    pub using_module: ModuleId<'a>,
    pub name: Option<Id<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStatement<'a> {
    pub test: Expression<'a>,
    pub true_branch: Sequence<'a>,
    pub false_branch: Sequence<'a>,
}

type Assignments<'a> = Vec<Assignment<'a>>;

#[derive(Debug, PartialEq, Clone)]
pub enum ForInitialize<'a> {
    Assignments(Assignments<'a>),
    VarDeclaration(VariableDef<'a>),
}

impl_from!(ForInitialize <- Assignments);
impl_from!(ForInitialize <- VariableDef -> VarDeclaration);

#[derive(Debug, PartialEq, Clone)]
pub struct ForStatement<'a> {
    pub initialize: Option<ForInitialize<'a>>,
    pub test: Option<Expression<'a>>,
    pub increment: Vec<Assignment<'a>>,
    pub body: Sequence<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileStatement<'a> {
    pub test: Expression<'a>,
    pub body: Sequence<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DoStatement<'a> {
    pub test: Expression<'a>,
    pub body: Sequence<'a>,
}

pub type Sequence<'a> = Vec<Statement<'a>>;

#[derive(Debug, PartialEq, Clone)]
pub enum Literal<'a> {
    Null,
    Me,
    Global,
    Boolean(bool),
    Character(char),
    Integer(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    String(&'a str),
    Symbol(Id<'a>),
    SymbolRef(SymbolRef<'a>),
}

impl<'a> From<bool> for Literal<'a> {
    fn from(b: bool) -> Self {
        Literal::Boolean(b)
    }
}

impl<'a> From<i64> for Literal<'a> {
    fn from(i: i64) -> Self {
        Literal::Long(i)
    }
}

impl<'a> From<&'a str> for Literal<'a> {
    fn from(s: &'a str) -> Self {
        Literal::String(s)
    }
}

impl<'a> From<Id<'a>> for Literal<'a> {
    fn from(id: Id<'a>) -> Self {
        Literal::Symbol(id)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolRefValue<'a> {
    Symbol(Id<'a>),
    String(&'a str),
    Bool(bool),
    Null,
    Integer(i64),
    Character(char),
    Array(Vec<Id<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct SymbolRef<'a> {
    pub id: Id<'a>,
    pub value: Option<SymbolRefValue<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CaseLabel<'a> {
    Expression(Expression<'a>),
    InstanceOf(Expression<'a>),
    Default,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CaseStatement<'a> {
    pub label: CaseLabel<'a>,
    pub body: Sequence<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InfixOp {
    Module,
    Div,
    Mul,
    Add,
    Sub,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    Distinct,
    And,
    BitAnd,
    Or,
    BitOr,
    InstanceOf,
    Has,
    BitXor,
    SLeft,
    SRight,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SwitchStatement<'a> {
    pub test: Expression<'a>,
    pub case_statements: Vec<CaseStatement<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CatchTypeBlock<'a> {
    pub test: (Id<'a>, Expression<'a>),
    pub body: Sequence<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CatchBlock<'a> {
    pub test: Id<'a>,
    pub body: Sequence<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TryStatement<'a> {
    pub try_block: Sequence<'a>,
    pub catch_type_blocks: Vec<CatchTypeBlock<'a>>,
    pub catch_block: Option<CatchBlock<'a>>,
    pub finally_block: Option<Sequence<'a>>,
}

//     : assignment ';'
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
#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    Assignment(Assignment<'a>),
    ProcedureCall(Expression<'a>),
    VarDeclaration(VariableDef<'a>),
    ConstDeclaration(ConstantDef<'a>),
    If(IfStatement<'a>),
    While(WhileStatement<'a>),
    Do(DoStatement<'a>),
    For(ForStatement<'a>),
    Switch(SwitchStatement<'a>),
    Break,
    Continue,
    Throw(Expression<'a>),
    Try(TryStatement<'a>),
    Return(Option<Expression<'a>>),
    Sequence(Sequence<'a>),
}

impl_from!(Statement <-
    Assignment,
    Sequence
);

impl_from!(Statement <-
    Expression -> ProcedureCall,
    VariableDef -> VarDeclaration,
    ConstantDef -> ConstDeclaration,
    IfStatement -> If,
    WhileStatement -> While,
    DoStatement -> Do,
    ForStatement -> For,
    TryStatement -> Try,
    SwitchStatement -> Switch
);

#[derive(Debug, PartialEq, Clone)]
pub enum PrefixOp {
    Not,
    BitNot,
    Plus,
    Minus,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Index<'a>(pub Expression<'a>, pub Expression<'a>);

#[derive(Debug, PartialEq, Clone)]
pub struct Select<'a>(pub Expression<'a>, pub Id<'a>);

pub type Invocation<'a> = Vec<Expression<'a>>;

#[derive(Debug, PartialEq, Clone)]
pub struct Infix<'a>(pub InfixOp, pub Expression<'a>, pub Expression<'a>);

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'a> {
    Me,
    PrefixOp(PrefixOp, Box<Expression<'a>>),
    Literal(Literal<'a>),
    Infix(Box<Infix<'a>>),
    Ternary(Box<Ternary<'a>>),
    Call(Call<'a>),
    Select(Box<Select<'a>>),
    Invoke(Box<Invoke<'a>>),
    Index(Box<Index<'a>>),
    Array(Vec<Expression<'a>>),
    ByteArray(Vec<Expression<'a>>),
    EmptyArray(Box<Expression<'a>>),
    EmptyByteArray(Box<Expression<'a>>),
    Dictionary(Vec<(Expression<'a>, Expression<'a>)>),
    Object {
        lvalue: Box<Expression<'a>>,
        arguments: Vec<Expression<'a>>,
    },
}

impl_from!(Expression <- Literal, Call);
impl_from!(Expression <- box
    Ternary,
    Infix,
    Index,
    Select,
    Invoke
);

impl<'a> From<Id<'a>> for Expression<'a> {
    fn from(id: Id<'a>) -> Self {
        Literal::from(id).into()
    }
}

impl<'a> From<&'a str> for Expression<'a> {
    fn from(s: &'a str) -> Self {
        Literal::from(s).into()
    }
}

impl<'a> From<i64> for Expression<'a> {
    fn from(i: i64) -> Self {
        Literal::from(i).into()
    }
}

impl<'a> From<bool> for Expression<'a> {
    fn from(b: bool) -> Self {
        Literal::from(b).into()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Scope {
    Local,
    Global,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ClassMember<'a> {
    ConstantDef(ConstantDef<'a>),
    VariableDef(VariableDef<'a>),
    FunctionDef(FunctionDef<'a>),
    EnumDef(EnumDef<'a>),
    ClassDef(ClassDef<'a>),
}

impl_from!(ClassMember <-
    ConstantDef,
    VariableDef,
    FunctionDef,
    EnumDef,
    ClassDef
);

#[derive(Debug, PartialEq, Clone)]
pub enum DeclarationFlag {
    Hidden,
    Static,
    Native,
    Public,
    Private,
    Protected,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration<'a, T: 'a> {
    MemberDef {
        annotations: Vec<SymbolRef<'a>>,
        flags: Vec<DeclarationFlag>,
        definition: T,
    },
    UsingDef(UsingDef<'a>),
}

pub type ClassDeclaration<'a> = Declaration<'a, ClassMember<'a>>;
pub type ModuleDeclaration<'a> = Declaration<'a, ModuleMember<'a>>;

#[derive(Debug, PartialEq, Clone)]
pub struct Program<'a> {
    pub members: Vec<ModuleDeclaration<'a>>,
}
