use std::collections::HashMap;

pub type Body<'a> = Vec<Stmt<'a>>;

#[derive(Debug, PartialEq, Clone)]
pub struct Ident<'a>(pub &'a str);

#[derive(Debug, PartialEq, Clone)]
pub struct Path<'a>(pub Vec<Ident<'a>>);

#[derive(Debug, PartialEq, Clone)]
pub enum Visibility {
    Private,
    Public,
    Protected,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InitializableDef<'a> {
    pub name: Ident<'a>,
    pub init: Option<Expr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClassDef<'a> {
    pub name: Ident<'a>,
    pub extends: Option<Path<'a>>,
    pub members: Vec<DecoratedDef<'a, ClassMember<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumDef<'a>(pub Vec<InitializableDef<'a>>);

#[derive(Debug, PartialEq, Clone)]
pub struct Assignation<'a> {
    pub name: Ident<'a>,
    pub value: Expr<'a>,
}

pub type VarDef<'a> = InitializableDef<'a>;

#[derive(Debug, PartialEq, Clone)]
pub struct ConstDef<'a> {
    pub name: Ident<'a>,
    pub init: Expr<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDef<'a> {
    pub name: Ident<'a>,
    pub params: Vec<Ident<'a>>,
    pub body: StmtBlock<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpr<'a> {
    pub name: Ident<'a>,
    pub arguments: Vec<Expr<'a>>,
    pub then: Option<Box<CallExpr<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InvokeExpr<'a> {
    pub receiver: Expr<'a>,
    pub call: CallExpr<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ternary<'a> {
    pub condition: Expr<'a>,
    pub true_branch: Expr<'a>,
    pub false_branch: Expr<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ModuleMember<'a> {
    VarDef(VarDef<'a>),
    ConstDef(ConstDef<'a>),
    EnumDef(EnumDef<'a>),
    FunctionDef(FunctionDef<'a>),
    ClassDef(ClassDef<'a>),
    ModuleDef(ModuleDef<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ModuleDef<'a> {
    pub name: Ident<'a>,
    pub members: Vec<DecoratedDef<'a, ModuleMember<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Using<'a> {
    pub path: Path<'a>,
    pub alias: Option<Ident<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct NewObject<'a> {
    pub path: Path<'a>,
    pub args: Vec<Expr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Group<'a>(pub Expr<'a>);

#[derive(Debug, PartialEq, Clone)]
pub struct IfStmt<'a> {
    pub cond: Expr<'a>,
    pub true_branch: Stmt<'a>,
    pub false_branch: Option<Stmt<'a>>,
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
    pub body: StmtBlock<'a>,
}

pub type StmtBlock<'a> = Vec<Stmt<'a>>;

#[derive(Debug, PartialEq, Clone)]
pub enum Literal<'a> {
    Boolean(bool),
    Character(char),
    Integer(i64),
    String(&'a str),
    Symbol(Ident<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum CaseLabel<'a> {
    Literal(Literal<'a>),
    InstanceOf(Path<'a>),
    Default,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CaseBlock<'a> {
    pub labels: Vec<CaseLabel<'a>>,
    pub statements: StmtBlock<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
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
    Or,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SwitchStmt<'a> {
    pub cond: Expr<'a>,
    pub case_blocks: Vec<CaseBlock<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CatchGuard<'a> {
    Variable(Ident<'a>),
    InstanceOf(Ident<'a>, Path<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct CatchStmt<'a> {
    pub guard: CatchGuard<'a>,
    pub body: StmtBlock<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TryStmt<'a> {
    pub body: StmtBlock<'a>,
    pub catch_body: Vec<CatchStmt<'a>>,
    pub finally_body: Option<StmtBlock<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt<'a> {
    Break,
    Assignation(Assignation<'a>),
    Call(CallExpr<'a>),
    Invoke(InvokeExpr<'a>),
    Using(Using<'a>),
    Function(FunctionDef<'a>),
    VarDef(VarDef<'a>),
    Return(Expr<'a>),
    IfStmt(Box<IfStmt<'a>>),
    ForStmt(Box<ForStmt<'a>>),
    WhileStmt(Box<WhileStmt<'a>>),
    DoWhileStmt(DoWhileStmt<'a>),
    SwitchStmt(SwitchStmt<'a>),
    TryStmt(TryStmt<'a>),
    Throw(Expr<'a>),
    StmtBlock(StmtBlock<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrefixOp {
    Not,
    Plus,
    Minus,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    PrefixOp(PrefixOp, Box<Expr<'a>>),
    Null,
    Me,
    Ident(Ident<'a>),
    Literal(Literal<'a>),
    BinOp(BinOp, Box<(Expr<'a>, Expr<'a>)>),
    Assignation(Box<Assignation<'a>>),
    Ternary(Box<Ternary<'a>>),
    Call(CallExpr<'a>),
    Invoke(Box<InvokeExpr<'a>>),
    Group(Box<Group<'a>>),
    NewArray(Vec<Expr<'a>>),
    NewEmptyArray(Box<Expr<'a>>),
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
    pub annotations: Vec<Ident<'a>>,
    pub scope: Scope,
    pub visibility: Visibility,
    pub definition: T,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ClassMember<'a> {
    VarDef(VarDef<'a>),
    ConstDef(ConstDef<'a>),
    FunctionDef(FunctionDef<'a>),
    EnumDef(EnumDef<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct RootModuleDef<'a> {
    pub members: Vec<DecoratedDef<'a, ModuleMember<'a>>>,
}
