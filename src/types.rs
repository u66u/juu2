use std::collections::HashMap;
use std::sync::{Arc, Mutex};

pub type TypeVarID = u32;

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Bool,
    String,
    Void,

    // A named type constructor (e.g. List<T>, Animal)
    Constructor {
        name: String,
        types: Vec<Type>, // Renamed 'args' to 'types' for clarity?
    },

    Function {
        params: Vec<Type>,
        ret: Box<Type>,
    },

    Pointer {
        inner: Box<Type>,
        mutable: bool,
    },

    Var(Arc<Mutex<TypeVar>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVar {
    // ?T
    Unknown { id: TypeVarID, level: usize },

    // T
    Known(Type),

    // Generic T inside "fn<T>"
    Generic { id: TypeVarID },
}

#[derive(Debug, Clone)]
pub struct ProgramContext {
    pub types: HashMap<String, TypeInfo>,
    pub functions: HashMap<String, Scheme>,
}

// Holds 'let' variables and generic type parameters (?T)
#[derive(Debug, Clone)]
pub struct Env<'a> {
    // We keep a reference to Globals so we don't have to pass two args everywhere
    pub context: &'a ProgramContext,
    // Local variables: "x" -> Int
    pub vars: HashMap<String, Type>,
    // Parent scope
    pub outer: Option<&'a Env<'a>>,
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub name: String,
    pub fields: HashMap<String, Type>,
}

// "Scheme" is a type with generics: forall T. T -> T (Hindley-Milner checks)
#[derive(Debug, Clone)]
pub struct Scheme {
    pub generics: Vec<u32>, // IDs of generic vars
    pub ty: Type,
}

impl<'a> Env<'a> {
    // Create the root scope (Function Body)
    pub fn new(context: &'a ProgramContext) -> Self {
        Env {
            context,
            vars: HashMap::new(),
            outer: None,
        }
    }

    // Create a child scope (If block)
    pub fn enter_scope(&'a self) -> Self {
        Env {
            context: self.context,
            vars: HashMap::new(),
            outer: Some(self),
        }
    }

    // Does NOT look at globals. Does NOT instantiate.
    pub fn get_var(&self, name: &str) -> Option<Type> {
        if let Some(t) = self.vars.get(name) {
            return Some(t.clone());
        }
        if let Some(outer) = self.outer {
            return outer.get_var(name);
        }
        None
    }

    pub fn get_type_info(&self, name: &str) -> Option<&TypeInfo> {
        self.context.types.get(name)
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Int, Type::Int) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::String, Type::String) => true,
            (Type::Void, Type::Void) => true,

            (
                Type::Constructor {
                    name: n1,
                    types: t1,
                },
                Type::Constructor {
                    name: n2,
                    types: t2,
                },
            ) => n1 == n2 && t1 == t2,

            (
                Type::Function {
                    params: p1,
                    ret: r1,
                },
                Type::Function {
                    params: p2,
                    ret: r2,
                },
            ) => p1 == p2 && r1 == r2,

            (
                Type::Pointer {
                    inner: i1,
                    mutable: m1,
                },
                Type::Pointer {
                    inner: i2,
                    mutable: m2,
                },
            ) => m1 == m2 && i1 == i2,

            // Equality for Variables: Are they the exact same pointer?
            (Type::Var(v1), Type::Var(v2)) => Arc::ptr_eq(v1, v2),

            _ => false,
        }
    }
}
