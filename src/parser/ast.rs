use std::collections::HashMap;

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
    pub body: Option<CodeBlock<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Call<'a> {
    pub name: Id<'a>,
    pub arguments: Vec<Expression<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Invoke<'a> {
    pub receiver: Expression<'a>,
    pub method: Id<'a>,
    pub arguments: Vec<Expression<'a>>,
}

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

impl<'a> From<ConstantDef<'a>> for ModuleMember<'a> {
    fn from(cf: ConstantDef<'a>) -> Self {
        ModuleMember::ConstantDef(cf)
    }
}

impl<'a> From<VariableDef<'a>> for ModuleMember<'a> {
    fn from(cf: VariableDef<'a>) -> Self {
        ModuleMember::VariableDef(cf)
    }
}

impl<'a> From<FunctionDef<'a>> for ModuleMember<'a> {
    fn from(fd: FunctionDef<'a>) -> Self {
        ModuleMember::FunctionDef(fd)
    }
}

impl<'a> From<EnumDef<'a>> for ModuleMember<'a> {
    fn from(fd: EnumDef<'a>) -> Self {
        ModuleMember::EnumDef(fd)
    }
}

impl<'a> From<ClassDef<'a>> for ModuleMember<'a> {
    fn from(cd: ClassDef<'a>) -> Self {
        ModuleMember::ClassDef(cd)
    }
}

impl<'a> From<ModuleDef<'a>> for ModuleMember<'a> {
    fn from(cd: ModuleDef<'a>) -> Self {
        ModuleMember::ModuleDef(cd)
    }
}

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
    pub true_branch: CodeBlock<'a>,
    pub false_branch: CodeBlock<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForInitialize<'a> {
    Assignments(Vec<Assignment<'a>>),
    VarDeclaration(VariableDef<'a>),
}

impl<'a> From<Vec<Assignment<'a>>> for ForInitialize<'a> {
    fn from(a: Vec<Assignment<'a>>) -> Self {
        ForInitialize::Assignments(a)
    }
}

impl<'a> From<VariableDef<'a>> for ForInitialize<'a> {
    fn from(vd: VariableDef<'a>) -> Self {
        ForInitialize::VarDeclaration(vd)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForStatement<'a> {
    pub initialize: Option<ForInitialize<'a>>,
    pub test: Option<Expression<'a>>,
    pub increment: Vec<Assignment<'a>>,
    pub body: CodeBlock<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileStatement<'a> {
    pub test: Expression<'a>,
    pub body: CodeBlock<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DoStatement<'a> {
    pub test: Expression<'a>,
    pub body: CodeBlock<'a>,
}

pub type CodeBlock<'a> = Vec<Statement<'a>>;

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
pub struct SwitchStatement<'a> {
    pub test: Expression<'a>,
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
pub struct TryStatement<'a> {
    pub body: CodeBlock<'a>,
    pub catch_body: Vec<CatchStmt<'a>>,
    pub finally_body: Option<CodeBlock<'a>>,
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
    CodeBlock(CodeBlock<'a>),
}

impl<'a> From<Assignment<'a>> for Statement<'a> {
    fn from(a: Assignment<'a>) -> Self {
        Statement::Assignment(a)
    }
}

impl<'a> From<Expression<'a>> for Statement<'a> {
    fn from(e: Expression<'a>) -> Self {
        Statement::ProcedureCall(e)
    }
}

impl<'a> From<VariableDef<'a>> for Statement<'a> {
    fn from(vd: VariableDef<'a>) -> Self {
        Statement::VarDeclaration(vd)
    }
}

impl<'a> From<ConstantDef<'a>> for Statement<'a> {
    fn from(cd: ConstantDef<'a>) -> Self {
        Statement::ConstDeclaration(cd)
    }
}

impl<'a> From<IfStatement<'a>> for Statement<'a> {
    fn from(is: IfStatement<'a>) -> Self {
        Statement::If(is)
    }
}

impl<'a> From<WhileStatement<'a>> for Statement<'a> {
    fn from(ws: WhileStatement<'a>) -> Self {
        Statement::While(ws)
    }
}

impl<'a> From<DoStatement<'a>> for Statement<'a> {
    fn from(ds: DoStatement<'a>) -> Self {
        Statement::Do(ds)
    }
}

impl<'a> From<ForStatement<'a>> for Statement<'a> {
    fn from(fs: ForStatement<'a>) -> Self {
        Statement::For(fs)
    }
}

impl<'a> From<CodeBlock<'a>> for Statement<'a> {
    fn from(cb: CodeBlock<'a>) -> Self {
        Statement::CodeBlock(cb)
    }
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
    pub receiver: Expression<'a>,
    pub offset: Expression<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Select<'a> {
    pub receiver: Expression<'a>,
    pub field: Id<'a>,
}

pub type Invocation<'a> = Vec<Expression<'a>>;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'a> {
    Me,
    PrefixOp(PrefixOp, Box<Expression<'a>>),
    Literal(Literal<'a>),
    BinOp(BinOp, Box<(Expression<'a>, Expression<'a>)>),
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

impl<'a> From<Literal<'a>> for Expression<'a> {
    fn from(l: Literal<'a>) -> Self {
        Expression::Literal(l)
    }
}

impl<'a> From<Ternary<'a>> for Expression<'a> {
    fn from(t: Ternary<'a>) -> Self {
        Expression::Ternary(Box::new(t))
    }
}

impl<'a> From<Call<'a>> for Expression<'a> {
    fn from(c: Call<'a>) -> Self {
        Expression::Call(c)
    }
}

impl<'a> From<Index<'a>> for Expression<'a> {
    fn from(i: Index<'a>) -> Self {
        Expression::Index(Box::new(i))
    }
}

impl<'a> From<Select<'a>> for Expression<'a> {
    fn from(s: Select<'a>) -> Self {
        Expression::Select(Box::new(s))
    }
}

impl<'a> From<Invoke<'a>> for Expression<'a> {
    fn from(i: Invoke<'a>) -> Self {
        Expression::Invoke(Box::new(i))
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

impl<'a> From<ConstantDef<'a>> for ClassMember<'a> {
    fn from(cf: ConstantDef<'a>) -> Self {
        ClassMember::ConstantDef(cf)
    }
}

impl<'a> From<VariableDef<'a>> for ClassMember<'a> {
    fn from(cf: VariableDef<'a>) -> Self {
        ClassMember::VariableDef(cf)
    }
}

impl<'a> From<FunctionDef<'a>> for ClassMember<'a> {
    fn from(fd: FunctionDef<'a>) -> Self {
        ClassMember::FunctionDef(fd)
    }
}

impl<'a> From<EnumDef<'a>> for ClassMember<'a> {
    fn from(fd: EnumDef<'a>) -> Self {
        ClassMember::EnumDef(fd)
    }
}

impl<'a> From<ClassDef<'a>> for ClassMember<'a> {
    fn from(cd: ClassDef<'a>) -> Self {
        ClassMember::ClassDef(cd)
    }
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
