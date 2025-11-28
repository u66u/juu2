use crate::ast;
use crate::infer::*;
use crate::types::*;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

// A helper to map AST types to Semantic Types
// e.g. "List<T>" -> Type::Constructor("List", [Type::Var(?1)])
pub fn resolve_type(ast_type: &ast::Type, env: &Env) -> Result<Type, String> {
    match ast_type {
        ast::Type::Named { name, generics } => {
            match name.as_str() {
                "Int" => Ok(Type::Int),
                "Bool" => Ok(Type::Bool),
                "String" => Ok(Type::String),
                "Void" => Ok(Type::Void),
                _ => {
                    // It's a Struct, Enum, or Type Variable
                    // Check if it exists in Env (like a generic T)
                    if let Some(t) = env.get_var(name) {
                        return Ok(t);
                    }

                    // Otherwise, treat it as a Constructor (Struct/Enum)
                    let mut resolved_args = Vec::new();
                    for arg in generics {
                        resolved_args.push(resolve_type(arg, env)?);
                    }

                    Ok(Type::Constructor {
                        name: name.clone(),
                        types: resolved_args,
                    })
                }
            }
        }
        ast::Type::Pointer { inner, mutable } => {
            let resolved_inner = resolve_type(inner, env)?;
            Ok(Type::Pointer {
                inner: Box::new(resolved_inner),
                mutable: *mutable,
            })
        }
        ast::Type::TypeVar(_) => Err("Direct TypeVar in AST not supported yet".to_string()),
    }
}

pub fn infer_expr(expr: &ast::Expr, env: &mut Env, level: usize) -> Result<Type, String> {
    match expr {
        ast::Expr::Literal(lit) => match lit {
            ast::Literal::Int(_) => Ok(Type::Int),
            ast::Literal::Bool(_) => Ok(Type::Bool),
            ast::Literal::String(_) => Ok(Type::String),
        },

        ast::Expr::Variable { name, generics: _ } => {
            // 1. Check Local Scope Chain (e.g., let x = 5)
            // Locals are Monomorphic (already types), so no instantiation needed
            if let Some(t) = env.get_var(name) {
                return Ok(t);
            }

            // 2. Check Global Scope (e.g., fn map<T>...)
            // Globals are Schemes (Generic) that have to be instantiated
            if let Some(scheme) = env.context.functions.get(name) {
                println!("DEBUG: Instantiating {} at level {}", name, level);
                println!("   Scheme: {:?}", scheme.ty);

                // turning "forall T. T -> T" into "?1 -> ?1"
                return Ok(instantiate(scheme, level));
            }

            Err(format!("Unknown variable or function: {}", name))
        }

        ast::Expr::Binary { op, lhs, rhs } => {
            let t_lhs = infer_expr(lhs, env, level)?;
            let t_rhs = infer_expr(rhs, env, level)?;

            match op {
                ast::BinOp::Add | ast::BinOp::Sub | ast::BinOp::Mul | ast::BinOp::Div => {
                    unify(t_lhs, Type::Int)?;
                    unify(t_rhs, Type::Int)?;
                    Ok(Type::Int)
                }
                ast::BinOp::Eq | ast::BinOp::Neq => {
                    unify(t_lhs, t_rhs)?;
                    Ok(Type::Bool)
                }
                ast::BinOp::Lt | ast::BinOp::Gt => {
                    unify(t_lhs, Type::Int)?;
                    unify(t_rhs, Type::Int)?;
                    Ok(Type::Bool)
                }
                ast::BinOp::Assign => {
                    unify(t_lhs, t_rhs)?;
                    Ok(Type::Void)
                }
                _ => Err("Operator not implemented yet".to_string()),
            }
        }

        ast::Expr::Call { callee, args } => {
            // infer Callee, args, create` return type, construct function
            let t_callee = infer_expr(callee, env, level)?;

            let mut t_args = Vec::new();
            for arg in args {
                t_args.push(infer_expr(arg, env, level)?);
            }

            let t_ret = new_var(level);

            let expected_fn = Type::Function {
                params: t_args,
                ret: Box::new(t_ret.clone()),
            };

            unify(t_callee, expected_fn)?;

            Ok(t_ret)
        }

        ast::Expr::StructInit {
            name,
            generics,
            fields,
        } => {
            // Look up struct definition
            let type_info = env
                .get_type_info(name)
                .ok_or(format!("Unknown struct: {}", name))?
                .clone();

            // Resolve User Provided Generics (e.g. Box<Int>)
            if generics.len() != type_info.generics.len() {
                return Err(format!(
                    "Struct {} expects {} generic args, got {}",
                    name,
                    type_info.generics.len(),
                    generics.len()
                ));
            }

            let mut sub_map = HashMap::new();
            let mut resolved_generics = Vec::new();

            for (i, ast_type) in generics.iter().enumerate() {
                let concrete_type = resolve_type(ast_type, env)?;
                let gen_id = type_info.generics[i];
                sub_map.insert(gen_id, concrete_type.clone());
                resolved_generics.push(concrete_type);
            }

            for (f_name, f_expr) in fields {
                let t_expr = infer_expr(f_expr, env, level)?;

                let raw_field_type = type_info
                    .fields
                    .get(f_name)
                    .ok_or(format!("Struct {} has no field {}", name, f_name))?;

                // substitute generic types with inferred types
                let expected_type = substitute(raw_field_type.clone(), &sub_map);

                unify(t_expr, expected_type)?;
            }

            Ok(Type::Constructor {
                name: name.clone(),
                types: resolved_generics,
            })
        }

        ast::Expr::MemberAccess { object, field } => {
            let t_obj = infer_expr(object, env, level)?;
            let t_pruned = prune(t_obj.clone());

            match t_pruned {
                Type::Constructor {
                    name,
                    types: concrete_args,
                } => {
                    let type_info = env
                        .get_type_info(&name)
                        .ok_or(format!("Unknown struct type: {}", name))?;

                    if let Some(raw_field_type) = type_info.fields.get(field) {
                        // Create mapping from Definition IDs -> Concrete Types
                        let mut sub_map = HashMap::new();
                        for (i, concrete) in concrete_args.iter().enumerate() {
                            if i < type_info.generics.len() {
                                sub_map.insert(type_info.generics[i], concrete.clone());
                            }
                        }

                        // Substitute T -> Int
                        Ok(substitute(raw_field_type.clone(), &sub_map))
                    } else {
                        Err(format!("Field {} not found on {}", field, name))
                    }
                }
                _ => Err(format!(
                    "Cannot access field {} on non-struct type {:?}",
                    field, t_pruned
                )),
            }
        }

        _ => Err("Expression not supported yet".to_string()),
    }
}

pub fn collect_types(program: &ast::Program, ctx: &mut ProgramContext) -> Result<(), String> {
    // Sub-Pass 1: Register Names (Needed for self-referrential structs for eexample)
    for item in &program.items {
        if let ast::Item::Struct { name, .. } = item {
            ctx.types.insert(
                name.clone(),
                TypeInfo {
                    name: name.clone(),
                    fields: HashMap::new(),
                    generics: Vec::new(),
                },
            );
        }
    }

    // Sub-Pass 2: Resolve Fields
    for item in &program.items {
        if let ast::Item::Struct {
            name,
            generics,
            fields,
        } = item
        {
            let mut gen_map = HashMap::new();
            let mut gen_ids = Vec::new();

            for gen_name in generics {
                let id = fresh_id();
                gen_map.insert(gen_name.clone(), id);
                gen_ids.push(id);
            }

            let mut field_map = HashMap::new();
            for (f_name, f_type) in fields {
                let resolved = resolve_type_with_generics(f_type, ctx, &gen_map)?;
                field_map.insert(f_name.clone(), resolved);
            }

            if let Some(info) = ctx.types.get_mut(name) {
                info.fields = field_map;
                info.generics = gen_ids;
            }
        }
    }
    Ok(())
}

pub fn collect_functions(program: &ast::Program, ctx: &mut ProgramContext) -> Result<(), String> {
    for item in &program.items {
        match item {
            ast::Item::Fn(func) => {
                // Create a Scheme for the function
                // The scheme needs to know about the Generic Params (<T, U>)
                // We map user strings "T" -> TypeVar IDs
                let mut gen_map: HashMap<String, u32> = HashMap::new();
                let mut gen_ids = Vec::new();

                for gen_name in &func.generics {
                    let id = fresh_id();
                    gen_map.insert(gen_name.clone(), id);
                    gen_ids.push(id);
                }

                let resolve_sig = |ast_t: &ast::Type| -> Result<Type, String> {
                    resolve_type_with_generics(ast_t, ctx, &gen_map)
                };

                let mut param_types = Vec::new();
                for (_, p_type) in &func.params {
                    param_types.push(resolve_sig(p_type)?);
                }

                let ret_type = if let Some(rt) = &func.ret_type {
                    resolve_sig(rt)?
                } else {
                    Type::Void
                };

                let fn_type = Type::Function {
                    params: param_types,
                    ret: Box::new(ret_type),
                };

                let scheme = Scheme {
                    generics: gen_ids,
                    ty: fn_type,
                };

                ctx.functions.insert(func.name.clone(), scheme);
            }
            // TODO: Handle Impl/Typeclass
            _ => {}
        }
    }
    Ok(())
}

fn resolve_type_with_generics(
    ast_t: &ast::Type,
    ctx: &ProgramContext,
    gen_map: &HashMap<String, u32>,
) -> Result<Type, String> {
    match ast_t {
        ast::Type::Named { name, generics } => {
            // Is it a generic param "T"?
            if let Some(id) = gen_map.get(name) {
                println!(
                    "matched ast_t: {:?} and entered the Some(id) = gen_map.get(name) branch",
                    ast_t
                );
                // Return a rigid Generic variable
                return Ok(Type::Var(Arc::new(Mutex::new(TypeVar::Generic {
                    id: *id,
                }))));
            }

            // Standard resolution
            let mut args = Vec::new();
            for arg in generics {
                args.push(resolve_type_with_generics(arg, ctx, gen_map)?);
            }

            // Is it a known struct?
            if ctx.types.contains_key(name) {
                return Ok(Type::Constructor {
                    name: name.clone(),
                    types: args,
                });
            }

            match name.as_str() {
                "Int" => Ok(Type::Int),
                "Bool" => Ok(Type::Bool),
                "String" => Ok(Type::String),
                "Void" => Ok(Type::Void),
                _ => Err(format!("Unknown type: {}", name)),
            }
        }
        ast::Type::Pointer { inner, mutable } => {
            let i = resolve_type_with_generics(inner, ctx, gen_map)?;
            Ok(Type::Pointer {
                inner: Box::new(i),
                mutable: *mutable,
            })
        }
        _ => Err("Unsupported type in signature".into()),
    }
}

fn check_block(
    stmts: &[ast::Stmt],
    env: &mut Env,
    level: usize,
    expected_ret: &Type,
) -> Result<Type, String> {
    let mut last_type = Type::Void;

    for stmt in stmts {
        match stmt {
            ast::Stmt::Let {
                name,
                annotation,
                value,
                ..
            } => {
                // 1. Infer the value's type
                let mut value_type = Type::Void; // Default if no value? (maybe disallowed)

                if let Some(expr) = value {
                    value_type = infer_expr(expr, env, level)?;
                }

                // 2. If there's an annotation (let x: Int = ...) - unify
                if let Some(ann) = annotation {
                    let ann_type = resolve_type(ann, env)?;
                    unify(value_type.clone(), ann_type)?;
                    value_type = prune(value_type); // Prune to be safe
                }

                env.vars.insert(name.clone(), value_type);
                last_type = Type::Void; // Let statements don't return a value
            }
            ast::Stmt::Return(expr) => {
                let t = infer_expr(expr, env, level)?;
                unify(t, expected_ret.clone())
                    .map_err(|e| format!("Return type mismatch: {}", e))?;
                last_type = Type::Void;
            }
            ast::Stmt::Expr(expr) => {
                last_type = infer_expr(expr, env, level)?;
            }
        }
    }

    Ok(last_type)
}

pub fn check_program(program: &ast::Program) -> Result<ProgramContext, String> {
    let mut ctx = ProgramContext {
        types: HashMap::new(),
        functions: HashMap::new(),
    };

    collect_types(program, &mut ctx)?;
    collect_functions(program, &mut ctx)?;

    println!("DEBUG: Registered Functions: {:?}", ctx.functions.keys());
    for (name, scheme) in &ctx.functions {
        println!(
            "DEBUG: Scheme for {}: generics={:?}, type={:?}",
            name, scheme.generics, scheme.ty
        );
    }

    for item in &program.items {
        if let ast::Item::Fn(func) = item {
            let scheme = ctx
                .functions
                .get(&func.name)
                .ok_or("Function scheme missing")?
                .clone();

            let mut env = Env::new(&ctx);

            if let Type::Function { params, ret } = &scheme.ty {
                for (i, (p_name, _)) in func.params.iter().enumerate() {
                    env.vars.insert(p_name.clone(), params[i].clone());
                }

                let body_res = check_block(&func.body, &mut env, 1, ret)?;

                if **ret != Type::Void {
                    unify(body_res, *ret.clone()).map_err(|e| {
                        format!("Function {} implicit return mismatch: {}", func.name, e)
                    })?;
                }
            } else {
                return Err("Scheme was not a function?".into());
            }
        }
    }

    Ok(ctx)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_program;

    fn check(code: &str) -> Result<ProgramContext, String> {
        let program = parse_program(code).map_err(|e| format!("{}", e))?;
        check_program(&program)
    }

    #[test]
    fn test_valid_math() {
        let code = "fn main() { let x = 5 + 5 * 2 }";
        assert!(check(code).is_ok());
    }

    #[test]
    fn test_type_mismatch() {
        let code = "fn main() { let x = 5 + true }";
        let result = check(code);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Type Mismatch"));
    }

    #[test]
    fn test_unknown_variable() {
        let code = "fn main() { let x = y }";
        let result = check(code);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Unknown variable"));
    }

    #[test]
    fn test_struct_access() {
        let code = r#"
            struct Point { x: Int, y: Int } -- ADDED COMMA HERE
            fn main() { 
                let p = Point { x: 1, y: 2 }
                let z = p.x + p.y
            }
        "#;
        assert!(check(code).is_ok());
    }

    #[test]
    fn test_struct_field_mismatch() {
        let code = r#"
            struct Point { x: Int }
            fn main() { 
                let p = Point { x: true } -- Error: expected Int
            }
        "#;
        let result = check(code);
        assert!(result.is_err());
        println!("{}", result.unwrap_err());
    }

    #[test]
    fn test_function_args_mismatch() {
        let code = r#"
            fn add(a: Int, b: Int) -> Int { a + b }
            fn main() { 
                add(5, true) -- Error: 2nd arg is bool
            }
        "#;
        assert!(check(code).is_err());
    }

    #[test]
    fn test_return_type_check() {
        let code = r#"
            fn get_num() -> Int { 
                true -- Implicit return is Bool, expected Int
            }
        "#;
        assert!(check(code).is_err());
    }
    #[test]
    fn test_generic_function_inference() {
        let code = r#"
            fn id<T>(x: T) -> T { x }
            
            fn main() {
                let a = id(5)       -- T becomes Int
                let b = id(true)    -- T becomes Bool
                let c = a + 5
            }
        "#;
        assert!(check(code).is_ok());
    }

    #[test]
    fn test_generic_function_error() {
        let code = r#"
            fn pair<T>(a: T, b: T) -> T { a }
            
            fn main() {
                let p = pair(5, true) -- Error: Int != Bool
            }
        "#;
        let res = check(code);
        assert!(res.is_err());
        assert!(res.unwrap_err().contains("Type Mismatch"));
    }

    #[test]
    fn test_generic_struct_usage() {
        let code = r#"
            struct Box<T> { inner: T }
            
            fn main() {
                let b1 = Box<Int> { inner: 10 }
                let b2 = Box<Bool> { inner: false }
                
                let x = b1.inner + 5
            }
        "#;
        assert!(check(code).is_ok());
    }

    #[test]
    fn test_variable_shadowing() {
        let code = r#"
            fn main() {
                let x = 5
                let x = true -- Shadowing x
                let y = x    -- y should be Bool
            }
        "#;
        assert!(check(code).is_ok());
    }

    #[test]
    fn test_nested_scope_shadowing() {
        let code = r#"
            fn main() {
                let x = 5
                {
                    let x = true
                    let z = x -- Bool
                }
                let y = x + 10 -- Should refer to outer Int (5)
            }
        "#;
    }
}
