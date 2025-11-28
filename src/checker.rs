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
                    // Check Generic parameters (T) first
                    if let Some(t) = env.get_var(name) {
                        return Ok(t);
                    }

                    // Otherwise, it's a Struct/Typeclass
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
        ast::Type::TypeVar(_) => Err("Direct TypeVar in AST not supported".to_string()),
    }
}

pub fn infer_expr(expr: &ast::Expr, env: &mut Env, level: usize) -> Result<Type, String> {
    match expr {
        ast::Expr::Literal(lit) => match lit {
            ast::Literal::Int(_) => Ok(Type::Int),
            ast::Literal::Bool(_) => Ok(Type::Bool),
            ast::Literal::String(_) => Ok(Type::String),
        },

        ast::Expr::Block(stmts) => {
            let mut scoped_env = env.enter_scope();
            check_block(stmts, &mut scoped_env, level, &Type::Void)
        }

        ast::Expr::Variable { name, generics: _ } => {
            // 1. Locals
            if let Some(t) = env.get_var(name) {
                return Ok(t);
            }
            // 2. Globals (Functions)
            if let Some(scheme) = env.context.functions.get(name) {
                return Ok(instantiate(scheme, level));
            }
            Err(format!("Unknown variable or function: {}", name))
        }

        ast::Expr::Unary { op, expr } => {
            let t_expr = infer_expr(expr, env, level)?;
            match op {
                ast::UnOp::Neg => {
                    unify(t_expr, Type::Int)?;
                    Ok(Type::Int)
                }
                ast::UnOp::Not => {
                    unify(t_expr, Type::Bool)?;
                    Ok(Type::Bool)
                }
            }
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
                ast::BinOp::Assign | ast::BinOp::AddAssign => {
                    unify(t_lhs, t_rhs)?;
                    Ok(Type::Void)
                }
                _ => Err("Operator not implemented yet".to_string()),
            }
        }

        ast::Expr::Call { callee, args } => {
            // Handle Method Calls (Implicit Self)
            let (t_callee, real_args) = if let ast::Expr::MemberAccess { object, field } = &**callee
            {
                let t_obj = infer_expr(object, env, level)?;
                let t_pruned = prune(t_obj.clone());

                // Resolve Method Type
                let method_type = match t_pruned {
                    Type::Constructor {
                        name: struct_name,
                        types: concrete_args,
                    } => {
                        let method_name = format!("{}::{}", struct_name, field);

                        if let Some(scheme) = env.context.functions.get(&method_name) {
                            let mut fn_type = instantiate(scheme, level);

                            // Substitute Struct Generics (Box<Int> -> T=Int)
                            if let Some(info) = env.get_type_info(&struct_name) {
                                let mut sub_map = HashMap::new();
                                for (i, concrete) in concrete_args.iter().enumerate() {
                                    if i < info.generics.len() {
                                        sub_map.insert(info.generics[i], concrete.clone());
                                    }
                                }
                                fn_type = substitute(fn_type, &sub_map);
                            }
                            Ok(fn_type)
                        } else {
                            // Fallback: maybe it's a field closure?
                            infer_expr(callee, env, level)
                        }
                    }
                    _ => infer_expr(callee, env, level),
                }?;

                // Simple heuristic: if function expects (Args + 1), inject self.
                let inject_self = match &method_type {
                    Type::Function { params, .. } => params.len() == args.len() + 1,
                    _ => false,
                };

                let mut arg_types = Vec::new();
                if inject_self {
                    arg_types.push(t_obj);
                }
                for arg in args {
                    arg_types.push(infer_expr(arg, env, level)?);
                }
                (method_type, arg_types)
            } else {
                // else: standard Call
                let t = infer_expr(callee, env, level)?;
                let mut arg_types = Vec::new();
                for arg in args {
                    arg_types.push(infer_expr(arg, env, level)?);
                }
                (t, arg_types)
            };

            let t_ret = new_var(level);
            let expected_fn = Type::Function {
                params: real_args,
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
            let type_info = env
                .get_type_info(name)
                .ok_or(format!("Unknown struct: {}", name))?
                .clone();

            if generics.len() != type_info.generics.len() {
                return Err(format!("Generic count mismatch for {}", name));
            }

            let mut sub_map = HashMap::new();
            let mut resolved_generics = Vec::new();

            for (i, ast_type) in generics.iter().enumerate() {
                let concrete = resolve_type(ast_type, env)?;
                sub_map.insert(type_info.generics[i], concrete.clone());
                resolved_generics.push(concrete);
            }

            for (f_name, f_expr) in fields {
                let t_expr = infer_expr(f_expr, env, level)?;
                let raw_field_type = type_info
                    .fields
                    .get(f_name)
                    .ok_or(format!("Struct {} has no field {}", name, f_name))?;

                let expected = substitute(raw_field_type.clone(), &sub_map);
                unify(t_expr, expected)?;
            }

            Ok(Type::Constructor {
                name: name.clone(),
                types: resolved_generics,
            })
        }

        ast::Expr::MemberAccess { object, field } => {
            let t_obj = infer_expr(object, env, level)?;
            let t_pruned = prune(t_obj);

            match t_pruned {
                Type::Constructor {
                    name,
                    types: concrete_args,
                } => {
                    let info = env
                        .get_type_info(&name)
                        .ok_or(format!("Unknown struct: {}", name))?;

                    if let Some(raw_t) = info.fields.get(field) {
                        let mut sub_map = HashMap::new();
                        for (i, concrete) in concrete_args.iter().enumerate() {
                            if i < info.generics.len() {
                                sub_map.insert(info.generics[i], concrete.clone());
                            }
                        }
                        Ok(substitute(raw_t.clone(), &sub_map))
                    } else {
                        // If field missing, assume it's a method access (function pointer)
                        // Handled by Call logic usually, but here return method type
                        let method_name = format!("{}::{}", name, field);
                        if let Some(scheme) = env.context.functions.get(&method_name) {
                            Ok(instantiate(scheme, level))
                        } else {
                            Err(format!("Field/Method {} not found on {}", field, name))
                        }
                    }
                }
                _ => Err(format!("Cannot access field {} on non-struct", field)),
            }
        }
    }
}

pub fn collect_types(program: &ast::Program, ctx: &mut ProgramContext) -> Result<(), String> {
    // 1. Register Names
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

    // 2. Resolve Fields
    for item in &program.items {
        if let ast::Item::Struct {
            name,
            generics,
            fields,
        } = item
        {
            let mut env = Env::new(ctx);
            let mut gen_ids = Vec::new();

            for gen_name in generics {
                let id = fresh_id();
                gen_ids.push(id);
                env.vars.insert(
                    gen_name.clone(),
                    Type::Var(Arc::new(Mutex::new(TypeVar::Generic { id }))),
                );
            }

            let mut field_map = HashMap::new();
            for (f_name, f_type) in fields {
                field_map.insert(f_name.clone(), resolve_type(f_type, &env)?);
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
    // Collect Traits
    let traits = collect_traits(program)?;
    ctx.traits.extend(traits);

    // Collect Function Schemes (Standalone + Impls)
    let mut schemes = Vec::new();

    schemes.extend(collect_standalone_fns(program, ctx)?);
    schemes.extend(collect_impls(program, ctx)?);

    // Commit
    for (name, scheme) in schemes {
        if ctx.functions.contains_key(&name) {
            return Err(format!("Duplicate function: {}", name));
        }
        ctx.functions.insert(name, scheme);
    }
    Ok(())
}

fn collect_standalone_fns(
    program: &ast::Program,
    ctx: &ProgramContext,
) -> Result<Vec<(String, Scheme)>, String> {
    let mut results = Vec::new();
    for item in &program.items {
        if let ast::Item::Fn(func) = item {
            let mut env = Env::new(ctx);
            let (scheme, _) = build_scheme(func, &mut env, vec![])?;
            results.push((func.name.clone(), scheme));
        }
    }
    Ok(results)
}

fn collect_traits(program: &ast::Program) -> Result<HashMap<String, TraitInfo>, String> {
    let mut results = HashMap::new();
    for item in &program.items {
        if let ast::Item::Typeclass {
            name,
            generics,
            methods,
        } = item
        {
            let dummy_ctx = ProgramContext {
                types: HashMap::new(),
                functions: HashMap::new(),
                traits: HashMap::new(),
            };
            let mut env = Env::new(&dummy_ctx);

            let mut gen_ids = Vec::new();
            for gen_name in generics {
                let id = fresh_id();
                gen_ids.push(id);
                env.vars.insert(
                    gen_name.clone(),
                    Type::Var(Arc::new(Mutex::new(TypeVar::Generic { id }))),
                );
            }

            let self_id = fresh_id();
            env.vars.insert(
                "Self".into(),
                Type::Var(Arc::new(Mutex::new(TypeVar::Generic { id: self_id }))),
            );

            let mut method_schemes = HashMap::new();
            for m in methods {
                let (scheme, _) = build_scheme(m, &mut env, gen_ids.clone())?;
                method_schemes.insert(m.name.clone(), scheme);
            }

            results.insert(
                name.clone(),
                TraitInfo {
                    name: name.clone(),
                    generics: gen_ids,
                    methods: method_schemes,
                },
            );
        }
    }
    Ok(results)
}

fn collect_impls(
    program: &ast::Program,
    ctx: &ProgramContext,
) -> Result<Vec<(String, Scheme)>, String> {
    let mut results = Vec::new();
    for item in &program.items {
        if let ast::Item::Impl {
            generics,
            trait_name,
            target_type,
            methods,
        } = item
        {
            let mut env = Env::new(ctx);
            let mut impl_gen_ids = Vec::new();

            // Register <T>
            for gen_name in generics {
                let id = fresh_id();
                impl_gen_ids.push(id);
                env.vars.insert(
                    gen_name.clone(),
                    Type::Var(Arc::new(Mutex::new(TypeVar::Generic { id }))),
                );
            }

            // Resolve Target & Self
            let resolved_target = resolve_type(target_type, &env)?;
            env.vars.insert("Self".into(), resolved_target.clone());

            // Validate Trait
            let mut required_methods = None;
            if let Some(tr_ast) = trait_name {
                let resolved_tr = resolve_type(tr_ast, &env)?;
                if let Type::Constructor { name, .. } = &resolved_tr {
                    if let Some(info) = ctx.traits.get(name) {
                        required_methods = Some((
                            name.clone(),
                            info.methods.keys().cloned().collect::<Vec<_>>(),
                        ));
                    } else {
                        return Err(format!("Unknown trait: {}", name));
                    }
                }
            }

            let prefix = match &resolved_target {
                Type::Constructor { name, .. } => name.clone(),
                _ => "Unknown".to_string(),
            };

            let mut method_names = Vec::new();
            for method in methods {
                let (scheme, _) = build_scheme(method, &mut env, impl_gen_ids.clone())?;
                results.push((format!("{}::{}", prefix, method.name), scheme));
                method_names.push(method.name.clone());
            }

            if let Some((tr_name, reqs)) = required_methods {
                for req in reqs {
                    if !method_names.contains(&req) {
                        return Err(format!("Impl for {} missing: {}", tr_name, req));
                    }
                }
            }
        }
    }
    Ok(results)
}

fn build_scheme(
    func: &ast::Function,
    env: &mut Env,
    mut existing_gen_ids: Vec<u32>,
) -> Result<(Scheme, Vec<u32>), String> {
    let mut method_gen_ids = Vec::new();
    for gen_name in &func.generics {
        let id = fresh_id();
        method_gen_ids.push(id);
        env.vars.insert(
            gen_name.clone(),
            Type::Var(Arc::new(Mutex::new(TypeVar::Generic { id }))),
        );
    }

    let mut param_types = Vec::new();
    for (_, p_type) in &func.params {
        param_types.push(resolve_type(p_type, &env)?);
    }

    let ret_type = if let Some(rt) = &func.ret_type {
        resolve_type(rt, &env)?
    } else {
        Type::Void
    };

    existing_gen_ids.extend(method_gen_ids);

    Ok((
        Scheme {
            generics: existing_gen_ids.clone(),
            ty: Type::Function {
                params: param_types,
                ret: Box::new(ret_type),
            },
        },
        existing_gen_ids,
    ))
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
                let mut value_type = None;
                if let Some(expr) = value {
                    value_type = Some(infer_expr(expr, env, level)?);
                }

                if let Some(ann) = annotation {
                    let ann_type = resolve_type(ann, env)?;
                    if let Some(v_type) = value_type.clone() {
                        unify(v_type, ann_type.clone())?;
                    }
                    value_type = Some(ann_type);
                }

                let final_type = match value_type {
                    Some(t) => prune(t),
                    None => return Err(format!("Variable '{}' needs type or value", name)),
                };

                env.vars.insert(name.clone(), final_type);
                last_type = Type::Void;
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
        traits: HashMap::new(),
    };

    collect_types(program, &mut ctx)?;
    collect_functions(program, &mut ctx)?;

    // Pass 3: Function Bodies
    for item in &program.items {
        if let ast::Item::Fn(func) = item {
            let scheme = ctx
                .functions
                .get(&func.name)
                .ok_or("Scheme missing")?
                .clone();

            let mut env = Env::new(&ctx);

            // Register Generics (map "T" -> GenericID)
            for (i, gen_name) in func.generics.iter().enumerate() {
                let id = scheme.generics[i];
                env.vars.insert(
                    gen_name.clone(),
                    Type::Var(Arc::new(Mutex::new(TypeVar::Generic { id }))),
                );
            }

            // Register Params
            if let Type::Function { params, ret } = &scheme.ty {
                for (i, (p_name, _)) in func.params.iter().enumerate() {
                    env.vars.insert(p_name.clone(), params[i].clone());
                }

                // Check Body in a child scope
                let mut body_env = env.enter_scope();
                let body_res = check_block(&func.body, &mut body_env, 1, ret)?;

                if **ret != Type::Void {
                    unify(body_res, *ret.clone()).map_err(|e| {
                        format!("Function '{}' implicit return mismatch: {}", func.name, e)
                    })?;
                }
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

        assert!(check(code).is_ok());
    }

    #[test]
    fn test_typeclass_basic() {
        let code = r#"
            typeclass Barks {
                fn bark() -> String {}
            }
            struct Dog { }
            impl Barks for Dog {
                fn bark() -> String { "Woof" }
            }
            fn main() {
                let d = Dog {}
                d.bark()
            }
        "#;
        assert!(check(code).is_ok());
    }
}
