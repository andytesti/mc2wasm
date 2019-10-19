use std::collections::HashMap;

pub type Body<'a> = Vec<Stmt<'a>>;

pub type Id<'a> = &'a str;

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
    pub value: Option<Expr<'a>>,
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
    pub initializer: Option<Expr<'a>>,
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
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assignment<'a> {
    pub operator: AssignmentOp,
    pub assignee: Assignee<'a>,
    pub value: Expr<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum VariableKind {
    Var,
    Const,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableDef<'a> {
    pub kind: VariableKind,
    pub pairs: Vec<VariableDefPair<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDef<'a> {
    pub id: Id<'a>,
    pub params: Vec<Id<'a>>,
    pub body: CodeBlock<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpr<'a> {
    pub name: Id<'a>,
    pub arguments: Vec<Expr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InvokeExpr<'a> {
    pub receiver: Expr<'a>,
    pub method: Id<'a>,
    pub arguments: Vec<Expr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ternary<'a> {
    pub condition: Expr<'a>,
    pub then_branch: Expr<'a>,
    pub else_branch: Expr<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ModuleMember<'a> {
    VarDef(VariableDef<'a>),
    EnumDef(EnumDef<'a>),
    FunctionDef(FunctionDef<'a>),
    ClassDef(ClassDef<'a>),
    ModuleDef(ModuleDef<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ModuleDef<'a> {
    pub name: Id<'a>,
    pub members: Vec<DecoratedDef<'a, ModuleMember<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UsingDef<'a> {
    pub using_module: ModuleId<'a>,
    pub name: Option<Id<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct NewObject<'a> {
    pub path: ModuleId<'a>,
    pub args: Vec<Expr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStatement<'a> {
    pub cond: Expr<'a>,
    pub true_branch: CodeBlock<'a>,
    pub false_branch: CodeBlock<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForStmt<'a> {
    pub init: Option<Stmt<'a>>,
    pub cond: Option<Expr<'a>>,
    pub inc: Option<Stmt<'a>>,
    pub body: Stmt<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileStmt<'a> {
    pub cond: Expr<'a>,
    pub body: Stmt<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DoWhileStmt<'a> {
    pub cond: Expr<'a>,
    pub body: CodeBlock<'a>,
}

pub type CodeBlock<'a> = Vec<Stmt<'a>>;

#[derive(Debug, PartialEq, Clone)]
pub enum Literal<'a> {
    Null,
    Me,
    Global,
    Boolean(bool),
    Character(char),
    Integer(i64),
    String(&'a str),
    Symbol(Id<'a>),
    SymbolRef(Id<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum CaseLabel<'a> {
    Literal(Literal<'a>),
    InstanceOf(ModuleId<'a>),
    Default,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CaseBlock<'a> {
    pub labels: Vec<CaseLabel<'a>>,
    pub statements: CodeBlock<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
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
pub struct SwitchStmt<'a> {
    pub cond: Expr<'a>,
    pub case_blocks: Vec<CaseBlock<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CatchGuard<'a> {
    Variable(Id<'a>),
    InstanceOf(Id<'a>, ModuleId<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct CatchStmt<'a> {
    pub guard: CatchGuard<'a>,
    pub body: CodeBlock<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TryStmt<'a> {
    pub body: CodeBlock<'a>,
    pub catch_body: Vec<CatchStmt<'a>>,
    pub finally_body: Option<CodeBlock<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt<'a> {
    Break,
    Assignment(Assignment<'a>),
    Call(CallExpr<'a>),
    Invoke(InvokeExpr<'a>),
    Using(UsingDef<'a>),
    Function(FunctionDef<'a>),
    VarDef(VariableDef<'a>),
    Return(Expr<'a>),
    IfStmt(IfStatement<'a>),
    ForStmt(Box<ForStmt<'a>>),
    WhileStmt(Box<WhileStmt<'a>>),
    DoWhileStmt(DoWhileStmt<'a>),
    SwitchStmt(SwitchStmt<'a>),
    TryStmt(TryStmt<'a>),
    Throw(Expr<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrefixOp {
    Not,
    BitNot,
    Plus,
    Minus,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Index<'a> {
    pub receiver: Expr<'a>,
    pub offset: Expr<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Select<'a> {
    pub receiver: Expr<'a>,
    pub field: Id<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Assignee<'a> {
    Var(Id<'a>),
    Index(Index<'a>),
    Select(Select<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    PrefixOp(PrefixOp, Box<Expr<'a>>),
    Me,
    Literal(Literal<'a>),
    BinOp(BinOp, Box<(Expr<'a>, Expr<'a>)>),
    Assignment(Box<Assignment<'a>>),
    Ternary(Box<Ternary<'a>>),
    Select(Box<Select<'a>>),
    Call(CallExpr<'a>),
    Invoke(Box<InvokeExpr<'a>>),
    Index(Box<Index<'a>>),
    NewArray(Vec<Expr<'a>>),
    NewByteArray(Vec<Expr<'a>>),
    NewEmptyArray(Box<Expr<'a>>),
    NewEmptyByteArray(Box<Expr<'a>>),
    NewDictionary(Vec<(Expr<'a>, Expr<'a>)>),
    NewObject(NewObject<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Scope {
    Instance,
    Static,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DecoratedDef<'a, T: 'a> {
    pub annotations: Vec<Id<'a>>,
    pub scope: Scope,
    pub visibility: Visibility,
    pub definition: T,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ClassMember<'a> {
    VariableDef(VariableDef<'a>),
    FunctionDef(FunctionDef<'a>),
    EnumDef(EnumDef<'a>),
    ClassDef(ClassDef<'a>),
}

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
pub enum ClassDeclaration<'a> {
    ClassMember {
        annotations: Vec<Id<'a>>,
        flags: Vec<DeclarationFlag>,
        def: ClassMember<'a>,
    },
    UsingDef(UsingDef<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct RootModuleDef<'a> {
    pub members: Vec<DecoratedDef<'a, ModuleMember<'a>>>,
}
