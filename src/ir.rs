// src/ir.rs

#[derive(Debug, Clone)]
pub struct Program {
    // C definitions: "typedef struct Point_Int { ... } Point_Int;"
    pub structs: Vec<StructDef>,
    // C function prototypes and bodies
    pub functions: Vec<FunctionDef>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<(String, CType)>,
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<(String, CType)>,
    pub ret_type: CType,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CType {
    Void,
    Int,
    Bool,
    U8,       
    CharStar, 
    GenRef,   // Runtime handle { index, gen }
    // use this for casting: ((Point*)juu_access(r))->x
    Struct(String),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    VarDecl {
        name: String,
        ty: CType,
        value: Option<Expr>,
    },
    Assign {
        target: Expr,
        value: Expr,
    },
    Return(Option<Expr>),
    Expr(Expr),
    If {
        cond: Expr,
        then_block: Vec<Stmt>,
        else_block: Vec<Stmt>,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(String),
    Variable(String),

    // ((Struct*)juu_access(obj))->field
    MemberAccess {
        object: Box<Expr>,
        field: String,
        struct_name: String, 
    },

    Alloc {
        struct_name: String,
    },

    Binary {
        op: String,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Call {
        func_name: String,
        args: Vec<Expr>,
    },
    Raw(String),
}
