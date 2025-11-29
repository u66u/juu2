#[derive(Debug, Clone)]
pub enum Type {
    Named { name: String, generics: Vec<Type> },
    Pointer { inner: Box<Type>, mutable: bool },
    TypeVar(String),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),

    Variable {
        name: String,
        generics: Vec<Type>,
    },

    Binary {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        op: UnOp,
        expr: Box<Expr>,
    },
    StructInit {
        name: String,
        generics: Vec<Type>,
        fields: Vec<(String, Expr)>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    MemberAccess {
        object: Box<Expr>,
        field: String,
    },
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Assign,
    AddAssign,
    SubAssign,
    Eq,
    Neq,
    Lt,
    Gt,
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        name: String,
        mutable: bool,
        annotation: Option<Type>,
        value: Option<Expr>,
    },
    Return(Expr),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Item {
    Fn(Function),
    Struct {
        name: String,
        generics: Vec<String>,
        fields: Vec<(String, Type)>,
    },
    Impl {
        generics: Vec<String>,
        trait_name: Option<Type>,
        target_type: Type,
        methods: Vec<Function>,
    },
    Typeclass {
        name: String,
        generics: Vec<String>,
        methods: Vec<Function>,
    },
    Stmt(Stmt),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub generics: Vec<String>,
    pub params: Vec<(String, Type)>,
    pub ret_type: Option<Type>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}
