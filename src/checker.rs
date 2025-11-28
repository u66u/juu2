use crate::ast;
use crate::infer::*;
use crate::types::*;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};



pub fn resolve_type(ast_type: &ast::Type, env: &Env) -> Result<Type, String> {
    match ast_type {
        ast::Type::Named { name, generics } => {
            match name.as_str() {
                "Int" => Ok(Type::Int),
                "Bool" => Ok(Type::Bool),
                "String" => Ok(Type::String),
                "Void" => Ok(Type::Void),
                _ => {
                    
                    
                    if let Some(t) = env.get_var(name) {
                        return Ok(t);
                    }

                    
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
            
            
            if let Some(t) = env.get_var(name) {
                return Ok(t);
            }

            
            
            if let Some(scheme) = env.context.functions.get(name) {
                
                
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

        ast::Expr::StructInit { name, fields, .. } => {
            
            let type_info = env
                .get_type_info(name)
                .ok_or(format!("Unknown struct: {}", name))?
                .clone();

            
            for (f_name, f_expr) in fields {
                let t_expr = infer_expr(f_expr, env, level)?;
                let t_field = type_info
                    .fields
                    .get(f_name)
                    .ok_or(format!("Struct {} has no field {}", name, f_name))?;

                unify(t_expr, t_field.clone())?;
            }

            
            
            
            Ok(Type::Constructor {
                name: name.clone(),
                types: vec![],
            })
        }

        ast::Expr::MemberAccess { object, field } => {
            
            let t_obj = infer_expr(object, env, level)?;

            
            
            let t_pruned = prune(t_obj.clone());

            match t_pruned {
                Type::Constructor { name, .. } => {
                    let type_info = env
                        .get_type_info(&name)
                        .ok_or(format!("Unknown struct type: {}", name))?;

                    if let Some(t_field) = type_info.fields.get(field) {
                        Ok(t_field.clone())
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
    for item in &program.items {
        if let ast::Item::Struct {
            name,
            generics: _,
            fields,
        } = item
        {
            
            
            
            

            
            
            

            let mut field_map = HashMap::new();
            
            let dummy_env = Env::new(ctx);

            for (f_name, f_type) in fields {
                let resolved = resolve_type(f_type, &dummy_env)?;
                field_map.insert(f_name.clone(), resolved);
            }

            ctx.types.insert(
                name.clone(),
                TypeInfo {
                    name: name.clone(),
                    fields: field_map,
                },
            );
        }
    }
    Ok(())
}

pub fn collect_functions(program: &ast::Program, ctx: &mut ProgramContext) -> Result<(), String> {
    for item in &program.items {
        match item {
            ast::Item::Fn(func) => {
                
                

                
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
            
            if let Some(id) = gen_map.get(name) {
                
                return Ok(Type::Var(Arc::new(Mutex::new(TypeVar::Generic {
                    id: *id,
                }))));
            }

            
            let mut args = Vec::new();
            for arg in generics {
                args.push(resolve_type_with_generics(arg, ctx, gen_map)?);
            }

            
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

pub fn check_program(program: &ast::Program) -> Result<ProgramContext, String> {
    let mut ctx = ProgramContext {
        types: HashMap::new(),
        functions: HashMap::new(),
    };

    
    collect_types(program, &mut ctx)?;

    
    collect_functions(program, &mut ctx)?;

    
    for item in &program.items {
        if let ast::Item::Fn(func) = item {
            
            let mut env = Env::new(&ctx);

            
            
            
            

            
            
        }
    }

    Ok(ctx)
}
