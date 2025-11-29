use crate::ast;
use crate::ir::{self, CType};
use crate::types::*;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};

pub struct MonoContext<'a> {
    pub ast_functions: HashMap<String, &'a ast::Function>,
    pub ast_structs: HashMap<String, &'a ast::Item>,

    pub fn_queue: Vec<(String, Vec<Type>)>,
    pub struct_queue: Vec<(String, Vec<Type>)>,

    pub generated_fns: HashSet<String>,
    pub generated_structs: HashSet<String>,

    pub ir_structs: Vec<ir::StructDef>,
    pub ir_functions: Vec<ir::FunctionDef>,

    pub local_vars: HashMap<String, Type>,

    pub ctx: &'a ProgramContext,
}

impl<'a> MonoContext<'a> {
    pub fn new(prog: &'a ast::Program, ctx: &'a ProgramContext) -> Self {
        let mut ast_functions = HashMap::new();
        let mut ast_structs = HashMap::new();

        for item in &prog.items {
            match item {
                ast::Item::Fn(func) => {
                    ast_functions.insert(func.name.clone(), func);
                }
                ast::Item::Struct { name, .. } => {
                    ast_structs.insert(name.clone(), item);
                }
                ast::Item::Impl {
                    target_type,
                    methods,
                    ..
                } => {
                    if let ast::Type::Named {
                        name: struct_name, ..
                    } = target_type
                    {
                        for method in methods {
                            let key = format!("{}::{}", struct_name, method.name);
                            ast_functions.insert(key, method);
                        }
                    }
                }
                _ => {}
            }
        }

        MonoContext {
            ast_functions,
            ast_structs,
            fn_queue: Vec::new(),
            struct_queue: Vec::new(),
            generated_fns: HashSet::new(),
            generated_structs: HashSet::new(),
            ir_structs: Vec::new(),
            ir_functions: Vec::new(),
            local_vars: HashMap::new(),
            ctx,
        }
    }

    pub fn mangle(&self, name: &str, generics: &[Type]) -> String {
        let safe_name = name.replace("::", "__");
        if generics.is_empty() {
            return safe_name;
        }
        let mut s = safe_name;
        for t in generics {
            s.push('_');
            s.push_str(&self.type_to_string(t));
        }
        s
    }

    fn type_to_string(&self, t: &Type) -> String {
        match t {
            Type::Int => "Int".into(),
            Type::Bool => "Bool".into(),
            Type::String => "Str".into(),
            Type::Void => "Void".into(),
            Type::Constructor { name, types } => self.mangle(name, types),
            Type::Pointer { inner, .. } => format!("Ptr_{}", self.type_to_string(inner)),
            Type::Function { .. } => "Fn".into(),
            _ => "Unknown".into(),
        }
    }

    pub fn lower_type(&mut self, t: &Type) -> CType {
        match t {
            Type::Int => CType::Int,
            Type::Bool => CType::Bool,
            Type::String => CType::CharStar,
            Type::Void => CType::Void,

            Type::Constructor { name, types } => {
                let mangled = self.mangle(name, types);
                if !self.generated_structs.contains(&mangled) {
                    self.struct_queue.push((name.clone(), types.clone()));
                }
                CType::GenRef
            }

            Type::Pointer { .. } => CType::GenRef,
            Type::Function { .. } => CType::GenRef,

            Type::Var(_) => panic!("Monomorphization failed: Found raw type variable {:?}", t),
        }
    }
}

pub fn run(program: &ast::Program, ctx: &ProgramContext) -> ir::Program {
    let mut mc = MonoContext::new(program, ctx);

    mc.fn_queue.push(("main".to_string(), vec![]));

    while !mc.fn_queue.is_empty() || !mc.struct_queue.is_empty() {
        while let Some((name, concrete_gens)) = mc.struct_queue.pop() {
            let mangled = mc.mangle(&name, &concrete_gens);
            if mc.generated_structs.contains(&mangled) {
                continue;
            }
            mc.generated_structs.insert(mangled.clone());

            lower_struct(&mut mc, name, concrete_gens, mangled);
        }

        if let Some((name, concrete_gens)) = mc.fn_queue.pop() {
            let mangled = mc.mangle(&name, &concrete_gens);
            if mc.generated_fns.contains(&mangled) {
                continue;
            }
            mc.generated_fns.insert(mangled.clone());

            lower_function(&mut mc, name, concrete_gens, mangled);
        }
    }

    ir::Program {
        structs: mc.ir_structs,
        functions: mc.ir_functions,
    }
}

fn lower_struct(mc: &mut MonoContext, original_name: String, concrete_gens: Vec<Type>, mangled_name: String) {
    if !mc.ast_structs.contains_key(&original_name) {
        return;
    }
    let item = *mc.ast_structs.get(&original_name).unwrap();
    
    if let ast::Item::Struct { name: _, generics, fields } = item {
        let mut ir_fields = Vec::new();
        let mut ctor_params = Vec::new();
        let mut ctor_body = Vec::new();

        // 1. Setup Constructor: VarDecl r = Alloc(...)
        ctor_body.push(ir::Stmt::VarDecl {
            name: "r".into(),
            ty: CType::GenRef,
            value: Some(ir::Expr::Alloc { struct_name: mangled_name.clone() })
        });

        for (f_name, f_ast_type) in fields {
            // Resolve Type
            let sem_type = ast_to_semantic(f_ast_type, &generics, &concrete_gens, mc.ctx, None);
            let c_type = mc.lower_type(&sem_type);
            
            // Add to Struct Def
            ir_fields.push((f_name.clone(), c_type.clone()));

            // Add to Constructor Params
            ctor_params.push((f_name.clone(), c_type));

            // Add Assignment to Constructor Body: r.field = arg
            ctor_body.push(ir::Stmt::Assign {
                target: ir::Expr::MemberAccess {
                    object: Box::new(ir::Expr::Variable("r".into())),
                    field: f_name.clone(),
                    struct_name: mangled_name.clone()
                },
                value: ir::Expr::Variable(f_name.clone())
            });
        }

        // Return r
        ctor_body.push(ir::Stmt::Return(Some(ir::Expr::Variable("r".into()))));

        // Add Struct Definition to IR
        mc.ir_structs.push(ir::StructDef {
            name: mangled_name.clone(),
            fields: ir_fields
        });

        // Add Constructor Function to IR
        let ctor_name = format!("new_{}", mangled_name);
        mc.ir_functions.push(ir::FunctionDef {
            name: ctor_name,
            params: ctor_params,
            ret_type: CType::GenRef,
            body: ctor_body,
        });
    }
}

fn lower_function(
    mc: &mut MonoContext,
    original_name: String,
    concrete_gens: Vec<Type>,
    mangled_name: String,
) {
    let func = *mc
        .ast_functions
        .get(&original_name)
        .unwrap_or_else(|| panic!("Function '{}' not found", original_name));

    mc.local_vars.clear();

    // 1. Determine Self Type & Align Generics
    let mut local_gen_names = func.generics.clone();
    let mut local_gen_types = concrete_gens.clone();
    let mut self_type = None;

    if let Some((struct_name, _)) = original_name.split_once("::") {
        if let Some(ast::Item::Struct {
            generics: s_gens, ..
        }) = mc.ast_structs.get(struct_name)
        {
            let mut combined_names = s_gens.clone();
            combined_names.extend(local_gen_names);
            local_gen_names = combined_names;

            let num_s_gens = s_gens.len();
            if num_s_gens <= local_gen_types.len() {
                let s_concrete = local_gen_types[0..num_s_gens].to_vec();
                self_type = Some(Type::Constructor {
                    name: struct_name.to_string(),
                    types: s_concrete,
                });
            }
        } else {
            match struct_name {
                "Int" => self_type = Some(Type::Int),
                "Bool" => self_type = Some(Type::Bool),
                "String" => self_type = Some(Type::String),
                _ => {}
            }
        }
    }

    // 2. Lower Params
    let mut ir_params = Vec::new();
    for (p_name, p_ast_type) in &func.params {
        let sem_type = ast_to_semantic(
            p_ast_type,
            &local_gen_names,
            &local_gen_types,
            mc.ctx,
            self_type.as_ref(),
        );
        mc.local_vars.insert(p_name.clone(), sem_type.clone());

        let c_type = mc.lower_type(&sem_type);
        ir_params.push((p_name.clone(), c_type));
    }

    // 3. Lower Return type
    let ret_sem = if let Some(rt) = &func.ret_type {
        ast_to_semantic(
            rt,
            &local_gen_names,
            &local_gen_types,
            mc.ctx,
            self_type.as_ref(),
        )
    } else {
        Type::Void
    };
    let ir_ret = mc.lower_type(&ret_sem);

    // 4. Lower Body
    let mut ir_body = Vec::new();
    let body_len = func.body.len();

    for (i, stmt) in func.body.iter().enumerate() {
        let is_last = i == body_len - 1;

        let mut ir_stmt = lower_stmt(
            mc,
            stmt,
            &local_gen_names,
            &local_gen_types,
            self_type.as_ref(),
        );

        // IMPLICIT RETURN MAGIC
        if is_last && ir_ret != CType::Void {
            if let ir::Stmt::Expr(e) = ir_stmt {
                ir_stmt = ir::Stmt::Return(Some(e));
            }
        }

        ir_body.push(ir_stmt);
    }

    mc.ir_functions.push(ir::FunctionDef {
        name: mangled_name,
        params: ir_params,
        ret_type: ir_ret,
        body: ir_body,
    });
}

fn lower_stmt(
    mc: &mut MonoContext,
    stmt: &ast::Stmt,
    gen_names: &[String],
    gen_types: &[Type],
    self_type: Option<&Type>,
) -> ir::Stmt {
    match stmt {
        ast::Stmt::Let {
            name,
            annotation,
            value,
            ..
        } => {
            let ir_val = value
                .as_ref()
                .map(|v| lower_expr(mc, v, gen_names, gen_types, self_type));

            let sem_type = if let Some(ann) = annotation {
                ast_to_semantic(ann, gen_names, gen_types, mc.ctx, self_type)
            } else if let Some(v) = value {
                recover_type(mc, v, gen_names, gen_types, self_type)
            } else {
                Type::Void
            };

            mc.local_vars.insert(name.clone(), sem_type.clone());
            let c_type = mc.lower_type(&sem_type);

            ir::Stmt::VarDecl {
                name: name.clone(),
                ty: c_type,
                value: ir_val,
            }
        }
        ast::Stmt::Expr(e) => ir::Stmt::Expr(lower_expr(mc, e, gen_names, gen_types, self_type)),
        ast::Stmt::Return(e) => {
            ir::Stmt::Return(Some(lower_expr(mc, e, gen_names, gen_types, self_type)))
        }
    }
}

fn lower_expr(
    mc: &mut MonoContext,
    expr: &ast::Expr,
    gen_names: &[String],
    gen_types: &[Type],
    self_type: Option<&Type>,
) -> ir::Expr {
    match expr {
        ast::Expr::Literal(l) => match l {
            ast::Literal::Int(i) => ir::Expr::Literal(i.to_string()),
            ast::Literal::Bool(b) => ir::Expr::Literal(if *b { "1".into() } else { "0".into() }),
            ast::Literal::String(s) => ir::Expr::Literal(format!("\"{}\"", s)),
        },
        ast::Expr::Variable { name, .. } => ir::Expr::Variable(name.clone()),
        ast::Expr::Binary { op, lhs, rhs } => {
            let op_str = match op {
                ast::BinOp::Add => "+",
                ast::BinOp::Mul => "*",
                ast::BinOp::Eq => "==",
                ast::BinOp::Assign => "=",
                ast::BinOp::AddAssign => "+=",
                _ => "?",
            };
            ir::Expr::Binary {
                op: op_str.to_string(),
                lhs: Box::new(lower_expr(mc, lhs, gen_names, gen_types, self_type)),
                rhs: Box::new(lower_expr(mc, rhs, gen_names, gen_types, self_type)),
            }
        }
        ast::Expr::Call { callee, args } => {
            let mut ir_args = Vec::new();
            if let ast::Expr::MemberAccess { object, field } = &**callee {
                let t_obj = recover_type(mc, object, gen_names, gen_types, self_type);

                if let Type::Constructor {
                    name: struct_name,
                    types: concrete_args,
                } = t_obj
                {
                    let method_name = format!("{}::{}", struct_name, field);

                    if let Some(scheme) = mc.ctx.functions.get(&method_name) {
                        let mangled = mc.mangle(&method_name, &concrete_args);
                        if !mc.generated_fns.contains(&mangled) {
                            mc.fn_queue
                                .push((method_name.clone(), concrete_args.clone()));
                        }

                        // Implicit Self Check
                        let needs_self = if let Type::Function { params, .. } = &scheme.ty {
                            params.len() == args.len() + 1
                        } else {
                            false
                        };

                        if needs_self {
                            ir_args.push(lower_expr(mc, object, gen_names, gen_types, self_type));
                        }
                        for arg in args {
                            ir_args.push(lower_expr(mc, arg, gen_names, gen_types, self_type));
                        }
                        return ir::Expr::Call {
                            func_name: mangled,
                            args: ir_args,
                        };
                    }
                }
            }

            for arg in args {
                ir_args.push(lower_expr(mc, arg, gen_names, gen_types, self_type));
            }

            if let ast::Expr::Variable { name, generics } = &**callee {
                let concrete_call_gens = solve_generics_from_args(
                    mc, name, args, generics, gen_names, gen_types, self_type,
                );
                let mangled = mc.mangle(name, &concrete_call_gens);

                if !mc.generated_fns.contains(&mangled) {
                    mc.fn_queue.push((name.clone(), concrete_call_gens));
                }
                return ir::Expr::Call {
                    func_name: mangled,
                    args: ir_args,
                };
            }

            ir::Expr::Call {
                func_name: "UNKNOWN_INDIRECT".into(),
                args: ir_args,
            }
        }
ast::Expr::StructInit { name, generics, fields } => {
            let mut concrete_struct_gens = Vec::new();
            for g in generics {
                concrete_struct_gens.push(ast_to_semantic(g, gen_names, gen_types, mc.ctx, self_type));
            }
            
            let mangled_struct_name = mc.mangle(name, &concrete_struct_gens);
            
            // Ensure struct (and its constructor) are generated
            if !mc.generated_structs.contains(&mangled_struct_name) {
                mc.struct_queue.push((name.clone(), concrete_struct_gens));
            }

            // We need to call new_Struct(arg1, arg2...)
            // BUT the user might have written fields in any order: Point { y: 2, x: 1 }
            // We must reorder them to match the Struct Definition order.
            
            let item = mc.ast_structs.get(name).expect("Struct definition missing");
            let def_fields = match item {
                ast::Item::Struct { fields, .. } => fields,
                _ => panic!("Not a struct"),
            };

            let mut arg_map = HashMap::new();
            for (f_name, f_expr) in fields {
                arg_map.insert(f_name.clone(), lower_expr(mc, f_expr, gen_names, gen_types, self_type));
            }

            // Build ordered args list
            let mut args_list = Vec::new();
            for (f_name, _) in def_fields {
                if let Some(expr) = arg_map.remove(f_name) {
                    args_list.push(expr);
                } else {
                    // This should be caught by the checker!
                    panic!("Missing field '{}' in struct init for {}", f_name, name);
                }
            }

            ir::Expr::Call { 
                func_name: format!("new_{}", mangled_struct_name), 
                args: args_list 
            }
        },
        ast::Expr::MemberAccess { object, field } => {
            let ir_obj = lower_expr(mc, object, gen_names, gen_types, self_type);
            let t_obj = recover_type(mc, object, gen_names, gen_types, self_type);

            let struct_name = if let Type::Constructor { name, types } = t_obj {
                mc.mangle(&name, &types)
            } else {
                "UNKNOWN".into()
            };

            ir::Expr::MemberAccess {
                object: Box::new(ir_obj),
                field: field.clone(),
                struct_name,
            }
        }
        _ => ir::Expr::Literal("0".into()),
    }
}

fn recover_type(
    mc: &MonoContext,
    expr: &ast::Expr,
    gen_names: &[String],
    gen_types: &[Type],
    self_type: Option<&Type>,
) -> Type {
    match expr {
        ast::Expr::Literal(l) => match l {
            ast::Literal::Int(_) => Type::Int,
            ast::Literal::Bool(_) => Type::Bool,
            ast::Literal::String(_) => Type::String,
        },
        ast::Expr::Variable { name, .. } => {
            if let Some(t) = mc.local_vars.get(name) {
                return t.clone();
            }
            Type::Void
        }
        ast::Expr::Binary { op, .. } => match op {
            ast::BinOp::Eq | ast::BinOp::Neq | ast::BinOp::Lt | ast::BinOp::Gt => Type::Bool,
            _ => Type::Int,
        },
        // Handle Call return type substitution
        ast::Expr::Call { callee, args } => {
            if let ast::Expr::Variable { name, generics } = &**callee {
                let concrete_call_gens = solve_generics_from_args(
                    mc, name, args, generics, gen_names, gen_types, self_type,
                );
                if let Some(scheme) = mc.ctx.functions.get(name) {
                    if let Type::Function { ret, .. } = &scheme.ty {
                        // Substitute generics
                        let mut sub_map = HashMap::new();
                        for (i, concrete) in concrete_call_gens.iter().enumerate() {
                            if i < scheme.generics.len() {
                                sub_map.insert(scheme.generics[i], concrete.clone());
                            }
                        }
                        return substitute((**ret).clone(), &sub_map);
                    }
                }
            }
            Type::Void
        }
        ast::Expr::StructInit { name, generics, .. } => {
            let mut args = Vec::new();
            for g in generics {
                args.push(ast_to_semantic(g, gen_names, gen_types, mc.ctx, self_type));
            }
            Type::Constructor {
                name: name.clone(),
                types: args,
            }
        }
        ast::Expr::MemberAccess { object, field } => {
            let t_obj = recover_type(mc, object, gen_names, gen_types, self_type);
            if let Type::Constructor { name, types } = t_obj {
                if let Some(info) = mc.ctx.types.get(&name) {
                    if let Some(raw_t) = info.fields.get(field) {
                        let mut sub_map = HashMap::new();
                        for (i, concrete) in types.iter().enumerate() {
                            if i < info.generics.len() {
                                sub_map.insert(info.generics[i], concrete.clone());
                            }
                        }
                        return substitute(raw_t.clone(), &sub_map);
                    }
                }
            }
            Type::Void
        }
        _ => Type::Void,
    }
}

fn solve_generics_from_args(
    mc: &MonoContext,
    fn_name: &str,
    args: &[ast::Expr],
    explicit_generics: &[ast::Type],
    local_gen_names: &[String],
    local_gen_types: &[Type],
    self_type: Option<&Type>,
) -> Vec<Type> {
    if !explicit_generics.is_empty() {
        let mut res = Vec::new();
        for g in explicit_generics {
            res.push(ast_to_semantic(
                g,
                local_gen_names,
                local_gen_types,
                mc.ctx,
                self_type,
            ));
        }
        return res;
    }

    let mut inferred = Vec::new();
    let mut map: HashMap<u32, Type> = HashMap::new();

    if let Some(scheme) = mc.ctx.functions.get(fn_name) {
        if let Type::Function { params, .. } = &scheme.ty {
            for (i, param_type) in params.iter().enumerate() {
                if i >= args.len() {
                    break;
                }
                let arg_type =
                    recover_type(mc, &args[i], local_gen_names, local_gen_types, self_type);
                if let Type::Var(v) = param_type {
                    let inner = v.lock().unwrap();
                    if let TypeVar::Generic { id } = *inner {
                        map.insert(id, arg_type);
                    }
                }
            }
        }
        for id in &scheme.generics {
            if let Some(t) = map.get(id) {
                inferred.push(t.clone());
            } else {
                inferred.push(Type::Int);
            }
        }
    }
    inferred
}

fn substitute(t: Type, map: &HashMap<u32, Type>) -> Type {
    match t {
        Type::Var(v) => {
            let inner = v.lock().unwrap();
            match &*inner {
                TypeVar::Generic { id } => {
                    if let Some(replacement) = map.get(id) {
                        return replacement.clone();
                    }
                }
                TypeVar::Known(k) => return substitute(k.clone(), map),
                _ => {}
            }
            drop(inner);
            Type::Var(v)
        }
        Type::Constructor { name, types } => Type::Constructor {
            name,
            types: types.into_iter().map(|arg| substitute(arg, map)).collect(),
        },
        Type::Function { params, ret } => Type::Function {
            params: params.into_iter().map(|p| substitute(p, map)).collect(),
            ret: Box::new(substitute(*ret, map)),
        },
        Type::Pointer { inner, mutable } => Type::Pointer {
            inner: Box::new(substitute(*inner, map)),
            mutable,
        },
        _ => t,
    }
}

fn ast_to_semantic(
    ast_type: &ast::Type,
    gen_names: &[String],
    gen_types: &[Type],
    ctx: &ProgramContext,
    self_type: Option<&Type>,
) -> Type {
    match ast_type {
        ast::Type::Named { name, generics } => {
            if name == "Self" {
                if let Some(st) = self_type {
                    return st.clone();
                }
            }

            // Check Generics
            if let Some(idx) = gen_names.iter().position(|n| n == name) {
                if idx < gen_types.len() {
                    return gen_types[idx].clone();
                }
            }

            match name.as_str() {
                "Int" => return Type::Int,
                "Bool" => return Type::Bool,
                "String" => return Type::String,
                "Void" => return Type::Void,
                _ => {}
            }

            let mut args = Vec::new();
            for g in generics {
                args.push(ast_to_semantic(g, gen_names, gen_types, ctx, self_type));
            }
            Type::Constructor {
                name: name.clone(),
                types: args,
            }
        }
        ast::Type::Pointer { inner, mutable } => Type::Pointer {
            inner: Box::new(ast_to_semantic(inner, gen_names, gen_types, ctx, self_type)),
            mutable: *mutable,
        },
        _ => Type::Void,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checker::check_program;
    use crate::parser::parse_program;

    
    fn compile_to_ir(code: &str) -> ir::Program {
        let ast = parse_program(code).expect("Parse failed");
        let ctx = check_program(&ast).expect("Type check failed");
        run(&ast, &ctx)
    }

    #[test]

    fn test_mono_generic_function_naming() {
        let code = r#"
            fn id<T>(x: T) -> T { x }
            fn main() { 
                let a = id(5) 
                let b = id(true)
            }
        "#;
        let ir = compile_to_ir(code);
        let names: Vec<String> = ir.functions.iter().map(|f| f.name.clone()).collect();
        assert!(names.iter().any(|n| n.contains("id_Int")));
        assert!(names.iter().any(|n| n.contains("id_Bool")));
    }

    #[test]
    fn test_mono_nested_generics() {
        
        let code = r#"
            struct Box<T> { inner: T }
            fn main() {
                let b = Box<Box<Int>> { 
                    inner: Box<Int> { inner: 5 } 
                }
            }
        "#;
        let ir = compile_to_ir(code);

        
        let structs: Vec<String> = ir.structs.iter().map(|s| s.name.clone()).collect();
        assert!(structs.contains(&"Box_Int".to_string()));
        assert!(structs.contains(&"Box_Box_Int".to_string()));
    }

    #[test]
    fn test_mono_member_access_resolution() {
        
        let code = r#"
            struct Point { x: Int, y: Int }
            fn main() {
                let p = Point { x: 1, y: 2 }
                let val = p.x
            }
        "#;
        let ir = compile_to_ir(code);

        let main = ir.functions.iter().find(|f| f.name == "main").unwrap();

        
        let mut found = false;
        for stmt in &main.body {
            if let ir::Stmt::VarDecl {
                value: Some(ir::Expr::MemberAccess { struct_name, .. }),
                ..
            } = stmt
            {
                assert_eq!(struct_name, "Point");
                found = true;
            }
        }
        assert!(
            found,
            "Did not generate MemberAccess with correct struct name"
        );
    }

    #[test]
    fn test_mono_method_call_mangling() {
        let code = r#"
            struct Dog { }
            impl Dog {
                fn bark() -> Int { 1 }
            }
            fn main() {
                let d = Dog {}
                d.bark()
            }
        "#;
        let ir = compile_to_ir(code);
        let main = ir.functions.iter().find(|f| f.name == "main").unwrap();
        let mut found_call = false;
        for stmt in &main.body {
            if let ir::Stmt::Expr(ir::Expr::Call { func_name, .. }) = stmt {
                assert!(func_name.contains("Dog::bark") || func_name.contains("Dog__bark"));
                found_call = true;
            }
        }
        assert!(found_call);
    }

    #[test]
    fn test_mono_primitive_types() {
        let code = "fn main() { let x = 5 }";
        let ir = compile_to_ir(code);

        let main = ir.functions.iter().find(|f| f.name == "main").unwrap();
        match &main.body[0] {
            ir::Stmt::VarDecl { ty, .. } => {
                
                assert_eq!(*ty, CType::Int);
            }
            _ => panic!("Expected VarDecl"),
        }
    }
}
