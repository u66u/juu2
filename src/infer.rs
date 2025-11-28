use crate::types::*;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};


static mut TYPE_VAR_COUNTER: u32 = 0;

pub fn fresh_id() -> u32 {
    unsafe {
        TYPE_VAR_COUNTER += 1;
        TYPE_VAR_COUNTER
    }
}


pub fn new_var(level: usize) -> Type {
    Type::Var(Arc::new(Mutex::new(TypeVar::Unknown {
        id: fresh_id(),
        level,
    })))
}




pub fn prune(t: Type) -> Type {
    match t {
        Type::Var(ref v) => {
            let mut inner = v.lock().unwrap();
            match *inner {
                TypeVar::Known(ref actual) => {
                    
                    let deep = prune(actual.clone());
                    
                    *inner = TypeVar::Known(deep.clone());
                    deep
                }
                _ => t.clone(),
            }
        }
        _ => t,
    }
}




pub fn instantiate(scheme: &Scheme, current_level: usize) -> Type {
    
    let mut map = HashMap::new();

    for gen_id in &scheme.generics {
        map.insert(*gen_id, new_var(current_level));
    }

    
    fn replace(t: Type, map: &HashMap<u32, Type>) -> Type {
        match t {
            Type::Var(v) => {
                let inner = v.lock().unwrap();
                match &*inner {
                    TypeVar::Generic { id } => {
                        if let Some(replacement) = map.get(id) {
                            return replacement.clone();
                        }
                    }
                    TypeVar::Known(k) => return replace(k.clone(), map),
                    _ => {}
                }
                
                drop(inner);
                Type::Var(v)
            }
            Type::Constructor { name, types } => Type::Constructor {
                name,
                types: types.into_iter().map(|arg| replace(arg, map)).collect(),
            },
            Type::Function { params, ret } => Type::Function {
                params: params.into_iter().map(|p| replace(p, map)).collect(),
                ret: Box::new(replace(*ret, map)),
            },
            Type::Pointer { inner, mutable } => Type::Pointer {
                inner: Box::new(replace(*inner, map)),
                mutable,
            },
            _ => t,
        }
    }

    replace(scheme.ty.clone(), &map)
}




fn occurs_in_type(id: u32, t: &Type) -> bool {
    let t = prune(t.clone());
    match t {
        Type::Var(v) => {
            let inner = v.lock().unwrap();
            match *inner {
                TypeVar::Unknown { id: other_id, .. } => id == other_id,
                TypeVar::Known(ref k) => occurs_in_type(id, k),
                _ => false,
            }
        }
        Type::Constructor { types, .. } => types.iter().any(|arg| occurs_in_type(id, arg)),
        Type::Function { params, ret } => {
            params.iter().any(|p| occurs_in_type(id, p)) || occurs_in_type(id, &ret)
        }
        Type::Pointer { inner, .. } => occurs_in_type(id, &inner),
        _ => false,
    }
}



fn bind(v_arc: Arc<Mutex<TypeVar>>, id: u32, t: Type) -> Result<(), String> {
    
    if let Type::Var(other) = &t {
        if Arc::ptr_eq(&v_arc, other) {
            return Ok(());
        }
    }

    
    if occurs_in_type(id, &t) {
        return Err(format!("Recursive type detected: ?{} inside {:?}", id, t));
    }

    
    let mut inner = v_arc.lock().unwrap();
    *inner = TypeVar::Known(t);
    Ok(())
}




pub fn unify(t1: Type, t2: Type) -> Result<(), String> {
    
    let t1 = prune(t1);
    let t2 = prune(t2);

    match (t1.clone(), t2.clone()) {
        
        (Type::Var(v), t) | (t, Type::Var(v)) => {
            let id = {
                let inner = v.lock().unwrap();
                match *inner {
                    TypeVar::Unknown { id, .. } => id,
                    TypeVar::Known(_) => unreachable!("Prune should have handled Known"),
                    TypeVar::Generic { .. } => {
                        return Err("Cannot unify rigid Generic T".to_string());
                    }
                }
            };
            bind(v, id, t)
        }

        
        (Type::Int, Type::Int) => Ok(()),
        (Type::Bool, Type::Bool) => Ok(()),
        (Type::String, Type::String) => Ok(()),
        (Type::Void, Type::Void) => Ok(()),

        
        (
            Type::Constructor {
                name: n1,
                types: args1,
            },
            Type::Constructor {
                name: n2,
                types: args2,
            },
        ) => {
            if n1 != n2 {
                return Err(format!("Type mismatch: {} != {}", n1, n2));
            }
            if args1.len() != args2.len() {
                return Err(format!(
                    "Type arg count mismatch for {}: {} vs {}",
                    n1,
                    args1.len(),
                    args2.len()
                ));
            }
            for (a, b) in args1.into_iter().zip(args2.into_iter()) {
                unify(a, b)?;
            }
            Ok(())
        }

        
        (
            Type::Function {
                params: p1,
                ret: r1,
            },
            Type::Function {
                params: p2,
                ret: r2,
            },
        ) => {
            if p1.len() != p2.len() {
                return Err(format!(
                    "Function param count mismatch: {} vs {}",
                    p1.len(),
                    p2.len()
                ));
            }
            for (a, b) in p1.into_iter().zip(p2.into_iter()) {
                unify(a, b)?;
            }
            unify(*r1, *r2)
        }

        
        (
            Type::Pointer {
                inner: i1,
                mutable: m1,
            },
            Type::Pointer {
                inner: i2,
                mutable: m2,
            },
        ) => {
            if m1 != m2 {
                return Err(format!(
                    "Pointer mutability mismatch: mut={} vs mut={}",
                    m1, m2
                ));
            }
            unify(*i1, *i2)
        }

        
        _ => Err(format!("Type Mismatch: {:?} != {:?}", t1, t2)),
    }
}



pub fn generalize(t: Type, current_level: usize) -> Scheme {
    let t = prune(t);
    let mut generics = Vec::new();
    let mut map = HashMap::new();

    
    fn scan(
        t: Type,
        current_level: usize,
        generics: &mut Vec<u32>,
        map: &mut HashMap<u32, Type>,
    ) -> Type {
        match t {
            Type::Var(v) => {
                let mut inner = v.lock().unwrap();
                match *inner {
                    TypeVar::Unknown { id, level } if level > current_level => {
                        
                        if !generics.contains(&id) {
                            generics.push(id);
                        }
                        
                        *inner = TypeVar::Generic { id };
                        
                        drop(inner);
                        Type::Var(v.clone())
                    }
                    TypeVar::Known(ref k) => {
                        let k = k.clone();
                        drop(inner); 
                        scan(k, current_level, generics, map)
                    }
                    _ => {
                        drop(inner);
                        Type::Var(v)
                    }
                }
            }
            Type::Constructor { name, types } => Type::Constructor {
                name,
                types: types
                    .into_iter()
                    .map(|arg| scan(arg, current_level, generics, map))
                    .collect(),
            },
            Type::Function { params, ret } => Type::Function {
                params: params
                    .into_iter()
                    .map(|p| scan(p, current_level, generics, map))
                    .collect(),
                ret: Box::new(scan(*ret, current_level, generics, map)),
            },
            Type::Pointer { inner, mutable } => Type::Pointer {
                inner: Box::new(scan(*inner, current_level, generics, map)),
                mutable,
            },
            _ => t,
        }
    }

    let new_type = scan(t, current_level, &mut generics, &mut map);
    Scheme {
        generics,
        ty: new_type,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_unify() {
        let t1 = Type::Int;
        let t2 = Type::Int;
        assert!(unify(t1, t2).is_ok());

        let t3 = Type::Bool;
        assert!(unify(Type::Int, t3).is_err());
    }

    #[test]
    fn test_var_binding() {
        
        let v1 = new_var(0);
        let t_int = Type::Int;

        assert!(unify(v1.clone(), t_int).is_ok());

        
        let pruned = prune(v1);
        assert_eq!(pruned, Type::Int);
    }

    #[test]
    fn test_list_generics() {
        
        let var_a = new_var(0);
        let list_var = Type::Constructor {
            name: "List".into(),
            types: vec![var_a.clone()],
        };
        let list_int = Type::Constructor {
            name: "List".into(),
            types: vec![Type::Int],
        };

        assert!(unify(list_var, list_int).is_ok());

        
        assert_eq!(prune(var_a), Type::Int);
    }

    #[test]
    fn test_function_unify() {
        
        
        let var_a = new_var(0);
        let var_b = new_var(0);

        let fn1 = Type::Function {
            params: vec![var_a.clone()],
            ret: Box::new(Type::Bool),
        };
        let fn2 = Type::Function {
            params: vec![Type::Int],
            ret: Box::new(var_b.clone()),
        };

        assert!(unify(fn1, fn2).is_ok());

        assert_eq!(prune(var_a), Type::Int);
        assert_eq!(prune(var_b), Type::Bool);
    }

    #[test]
    fn test_recursive_crash() {
        
        let var_a = new_var(0);
        let list_a = Type::Constructor {
            name: "List".into(),
            types: vec![var_a.clone()],
        };

        let result = unify(var_a, list_a);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Recursive"));
    }
}
