use crate::ast;
use crate::ir;
use crate::types::*;
use std::collections::{HashMap, HashSet};

pub struct MonoContext {
    pub type_map: HashMap<String, String>,

    pub fn_queue: Vec<(String, Vec<Type>)>,
    pub generated_fns: HashSet<String>,

    pub output: ir::Program,
}

impl MonoContext {
    pub fn new() -> Self {
        MonoContext {
            type_map: HashMap::new(),
            fn_queue: Vec::new(),
            generated_fns: HashSet::new(),
            output: ir::Program {
                structs: vec![],
                functions: vec![],
            },
        }
    }

    pub fn mangle_type(&mut self, t: &Type) -> String {
        match t {
            Type::Int => "int".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "char*".to_string(),
            Type::Void => "void".to_string(),
            Type::Constructor { name, types } => {
                if types.is_empty() {
                    return name.clone();
                }

                let mut mangled = name.clone();
                for arg in types {
                    mangled.push('_');
                    mangled.push_str(&self.mangle_type(arg));
                }
                mangled
            }
            Type::Pointer { inner, mutable } => "GenRef".to_string(),
            _ => "void*".to_string(),
        }
    }
}

pub fn monomorphize(program: &ast::Program, ctx: &ProgramContext) -> ir::Program {
    let mut mono = MonoContext::new();

    mono.fn_queue.push(("main".to_string(), vec![]));

    let mut i = 0;
    while i < mono.fn_queue.len() {
        let (fn_name, concrete_generics) = mono.fn_queue[i].clone();
        i += 1;

        let mangled_name = if fn_name == "main" {
            "main".to_string()
        } else {
            format!("{}_{}", fn_name, i)
        };

        if mono.generated_fns.contains(&mangled_name) {
            continue;
        }
        mono.generated_fns.insert(mangled_name.clone());
    }

    mono.output
}
