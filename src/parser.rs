use lazy_static::lazy_static;
use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc::*, Op, PrattParser};
use pest_derive::Parser;


use crate::ast::*;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct LangParser;



lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use Rule::*;
        PrattParser::new()
            .op(Op::infix(assign, Right)
                | Op::infix(add_assign, Right)
                | Op::infix(sub_assign, Right)
                | Op::infix(mul_assign, Right)
                | Op::infix(div_assign, Right))
            .op(Op::infix(eq, Left)
                | Op::infix(neq, Left)
                | Op::infix(lt, Left)
                | Op::infix(gt, Left)
                | Op::infix(le, Left)
                | Op::infix(ge, Left))
            .op(Op::infix(add, Left) | Op::infix(sub, Left))
            .op(Op::infix(mul, Left) | Op::infix(div, Left))
            .op(Op::prefix(neg) | Op::prefix(not))
    };
}



pub fn parse_program(source: &str) -> Result<Program, pest::error::Error<Rule>> {
    let mut pairs = LangParser::parse(Rule::program, source)?;
    let root = pairs.next().unwrap(); 

    let mut items = Vec::new();
    for inner in root.into_inner() {
        if inner.as_rule() == Rule::item {
            
            let specific_item = inner.into_inner().next().unwrap();
            items.push(parse_item(specific_item));
        }
    }

    Ok(Program { items })
}

fn parse_item(pair: Pair<Rule>) -> Item {
    match pair.as_rule() {
        
        Rule::fn_def => Item::Fn(parse_fn_struct(pair)),

        Rule::struct_def => parse_struct(pair),
        Rule::stmt => {
            let stmt = parse_stmt(pair.into_inner().next().unwrap());
            Item::Stmt(stmt)
        }
        Rule::impl_def => parse_impl(pair),
        Rule::typeclass_def => parse_typeclass(pair),
        _ => panic!("Unexpected item: {:?}", pair.as_rule()),
    }
}

fn parse_fn_struct(pair: Pair<Rule>) -> Function {
    let mut inner = pair.into_inner();

    let name = inner.next().unwrap().as_str().to_string();

    let mut generics = Vec::new();
    let mut next_token = inner.next().unwrap();
    if next_token.as_rule() == Rule::generic_params {
        generics = parse_generic_params(next_token);
        next_token = inner.next().unwrap();
    }

    let mut params = Vec::new();
    if next_token.as_rule() == Rule::param_list {
        for param_pair in next_token.into_inner() {
            let mut p_inner = param_pair.into_inner();
            let p_name = p_inner.next().unwrap().as_str().to_string();
            let p_type = parse_type(p_inner.next().unwrap());
            params.push((p_name, p_type));
        }
        next_token = inner.next().unwrap();
    }

    let mut ret_type = None;
    if next_token.as_rule() == Rule::type_expr {
        ret_type = Some(parse_type(next_token));
        next_token = inner.next().unwrap();
    }

    let body = parse_block(next_token);

    
    Function {
        name,
        generics,
        params,
        ret_type,
        body,
    }
}

fn parse_impl(pair: Pair<Rule>) -> Item {
    let inner = pair.into_inner();

    let mut generics = Vec::new();
    let mut methods = Vec::new();

    
    
    
    

    
    let mut header_types: Vec<Type> = Vec::new();

    for part in inner {
        match part.as_rule() {
            Rule::generic_params => generics = parse_generic_params(part),
            Rule::ident => {
                
                
                
                let name = part.as_str().to_string();
                header_types.push(Type::Named {
                    name,
                    generics: vec![],
                }); 
            }
            Rule::generic_args => {
                
                if let Some(last_type) = header_types.last_mut() {
                    if let Type::Named { generics: g, .. } = last_type {
                        *g = parse_generic_args(part);
                    }
                }
            }
            Rule::fn_def => {
                
                methods.push(parse_fn_struct(part));
            }
            _ => {}
        }
    }

    
    let (trait_name, target_type) = if header_types.len() == 1 {
        (None, header_types.pop().unwrap())
    } else if header_types.len() == 2 {
        let target = header_types.pop().unwrap();
        let trait_t = header_types.pop().unwrap();
        (Some(trait_t), target)
    } else {
        panic!(
            "Invalid impl definition: found {} types in header (expected 1 or 2)",
            header_types.len()
        );
    };

    Item::Impl {
        generics,
        trait_name,
        target_type,
        methods,
    }
}

fn parse_typeclass(pair: Pair<Rule>) -> Item {
    let inner = pair.into_inner();

    let mut name = String::new();
    let mut generics = Vec::new();
    let mut methods = Vec::new();

    for part in inner {
        match part.as_rule() {
            Rule::ident => name = part.as_str().to_string(),
            Rule::generic_params => generics = parse_generic_params(part),
            Rule::fn_def => methods.push(parse_fn_struct(part)),
            _ => {}
        }
    }

    Item::Typeclass {
        name,
        generics,
        methods,
    }
}



fn parse_fn(pair: Pair<Rule>) -> Item {
    let mut inner = pair.into_inner();

    let name = inner.next().unwrap().as_str().to_string();

    let mut generics = Vec::new();
    let mut next_token = inner.next().unwrap();
    if next_token.as_rule() == Rule::generic_params {
        generics = parse_generic_params(next_token);
        next_token = inner.next().unwrap();
    }

    let mut params = Vec::new();
    if next_token.as_rule() == Rule::param_list {
        for param_pair in next_token.into_inner() {
            let mut p_inner = param_pair.into_inner();
            let p_name = p_inner.next().unwrap().as_str().to_string();
            let p_type = parse_type(p_inner.next().unwrap());
            params.push((p_name, p_type));
        }
        next_token = inner.next().unwrap();
    }

    let mut ret_type = None;
    if next_token.as_rule() == Rule::type_expr {
        ret_type = Some(parse_type(next_token));
        next_token = inner.next().unwrap();
    }

    let body = parse_block(next_token);

    
    Item::Fn(Function {
        name,
        generics,
        params,
        ret_type,
        body,
    })
}

fn parse_struct(pair: Pair<Rule>) -> Item {
    
    let inner_all = pair.into_inner();

    
    let mut fields: Vec<(String, Type)> = Vec::new();
    let mut generics: Vec<String> = Vec::new();
    let mut name = String::new();

    for part in inner_all {
        match part.as_rule() {
            Rule::ident => name = part.as_str().to_string(),
            Rule::generic_params => generics = parse_generic_params(part),
            Rule::struct_field => {
                let mut f_inner = part.into_inner();
                let f_name = f_inner.next().unwrap().as_str().to_string();
                let f_type = parse_type(f_inner.next().unwrap());
                fields.push((f_name, f_type));
            }
            _ => {}
        }
    }

    Item::Struct {
        name,
        generics,
        fields,
    }
}

fn parse_generic_params(pair: Pair<Rule>) -> Vec<String> {
    pair.into_inner().map(|p| p.as_str().to_string()).collect()
}



fn parse_block(pair: Pair<Rule>) -> Vec<Stmt> {
    pair.into_inner().map(parse_stmt).collect()
}

fn parse_stmt(pair: Pair<Rule>) -> Stmt {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::let_stmt => {
            let mut parts = inner.into_inner();
            let mut name_part = parts.next().unwrap();
            let mut mutable = false;

            
            if name_part.as_str() == "mut" {
                mutable = true;
                name_part = parts.next().unwrap();
            }

            let name = name_part.as_str().to_string();

            
            let mut annotation = None;
            let mut value = None;

            
            for part in parts {
                match part.as_rule() {
                    Rule::type_expr => {
                        annotation = Some(parse_type(part));
                    }
                    Rule::expr => {
                        
                        value = Some(parse_expr(part.into_inner()));
                    }
                    _ => {}
                }
            }

            Stmt::Let {
                name,
                mutable,
                annotation,
                value,
            }
        }
        
        Rule::return_stmt => {
            let expr = parse_expr(inner.into_inner().next().unwrap().into_inner());
            Stmt::Return(expr)
        }
        Rule::expr_stmt => {
            let expr = parse_expr(inner.into_inner().next().unwrap().into_inner());
            Stmt::Expr(expr)
        }
        _ => panic!("Unknown statement: {:?}", inner.as_rule()),
    }
}



fn parse_expr(pairs: Pairs<Rule>) -> Expr {
    PRATT_PARSER
        .map_primary(|primary| parse_atom(primary))
        .map_infix(|lhs, op, rhs| {
            let bin_op = match op.as_rule() {
                Rule::add => BinOp::Add,
                Rule::sub => BinOp::Sub,
                Rule::mul => BinOp::Mul,
                Rule::div => BinOp::Div,
                Rule::assign => BinOp::Assign,
                Rule::add_assign => BinOp::AddAssign,
                Rule::sub_assign => BinOp::SubAssign, 
                Rule::eq => BinOp::Eq,
                Rule::neq => BinOp::Neq,
                Rule::lt => BinOp::Lt,
                Rule::gt => BinOp::Gt,
                _ => panic!("Unknown infix op: {:?}", op.as_rule()),
            };
            Expr::Binary {
                op: bin_op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        })
        .map_prefix(|op, rhs| {
            let un_op = match op.as_rule() {
                Rule::neg => UnOp::Neg,
                Rule::not => UnOp::Not,
                _ => panic!("Unknown prefix op"),
            };
            Expr::Unary {
                op: un_op,
                expr: Box::new(rhs),
            }
        })
        .parse(pairs)
}



fn parse_atom(pair: Pair<Rule>) -> Expr {
    match pair.as_rule() {
        Rule::literal => parse_literal(pair.into_inner().next().unwrap()),
        Rule::struct_init => parse_struct_init(pair),
        Rule::term => parse_term(pair),
        Rule::expr => parse_expr(pair.into_inner()), 
        _ => panic!("Unexpected atom: {:?}", pair.as_rule()),
    }
}

fn parse_literal(pair: Pair<Rule>) -> Expr {
    match pair.as_rule() {
        Rule::int_lit => Expr::Literal(Literal::Int(pair.as_str().parse().unwrap())),
        Rule::string_lit => {
            
            let s = pair.as_str();
            Expr::Literal(Literal::String(s[1..s.len() - 1].to_string()))
        }
        Rule::bool_lit => Expr::Literal(Literal::Bool(pair.as_str() == "true")),
        _ => panic!("Unknown literal: {:?}", pair.as_rule()),
    }
}

fn parse_struct_init(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();

    
    let mut next = inner.next().unwrap();
    let mut generics = Vec::new();
    if next.as_rule() == Rule::generic_args {
        generics = parse_generic_args(next);
        if let Some(n) = inner.next() {
            next = n;
        } else {
            return Expr::StructInit {
                name,
                generics,
                fields: vec![],
            };
        }
    }

    
    let mut fields = Vec::new();
    
    
    if next.as_rule() == Rule::field_init {
        fields.push(parse_field_init(next));
        for f in inner {
            fields.push(parse_field_init(f));
        }
    }

    Expr::StructInit {
        name,
        generics,
        fields,
    }
}

fn parse_field_init(pair: Pair<Rule>) -> (String, Expr) {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let expr = parse_expr(inner.next().unwrap().into_inner());
    (name, expr)
}


fn parse_term(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();

    let name_token = inner.next().unwrap();
    let name = name_token.as_str().to_string();

    
    let mut generics: Vec<Type> = Vec::new();

    
    
    let mut expr = Expr::Variable {
        name: name.clone(),
        generics: generics.clone(),
    };

    for part in inner {
        match part.as_rule() {
            Rule::generic_args => {
                generics = parse_generic_args(part);
                
                if let Expr::Variable { name, .. } = expr {
                    expr = Expr::Variable {
                        name,
                        generics: generics.clone(),
                    };
                }
            }
            Rule::method_call => {
                let mut args = Vec::new();
                let mut inner_call = part.into_inner();
                if let Some(arg_list) = inner_call.next() {
                    args = arg_list
                        .into_inner()
                        .map(|e| parse_expr(e.into_inner()))
                        .collect();
                }
                expr = Expr::Call {
                    callee: Box::new(expr),
                    args,
                };
            }
            Rule::member_access => {
                let mut inner_access = part.into_inner();
                let field = inner_access.next().unwrap().as_str().to_string();
                expr = Expr::MemberAccess {
                    object: Box::new(expr),
                    field,
                };
            }
            _ => {
                eprintln!("Unexpected rule in term: {:?}", part.as_rule());
            }
        }
    }

    expr
}



fn parse_type(pair: Pair<Rule>) -> Type {
    let inner = pair.into_inner();
    let mut mutable = false;
    let mut pointer = false;

    
    let mut name = String::new();
    let mut generics = Vec::new();

    for part in inner {
        match part.as_rule() {
            Rule::mut_token => mutable = true,
            Rule::ptr_token => pointer = true,
            Rule::ident => name = part.as_str().to_string(),
            Rule::generic_args => generics = parse_generic_args(part),
            _ => {}
        }
    }

    let base_type = Type::Named { name, generics };

    if pointer {
        Type::Pointer {
            inner: Box::new(base_type),
            mutable,
        }
    } else {
        base_type
    }
}

fn parse_generic_args(pair: Pair<Rule>) -> Vec<Type> {
    pair.into_inner().map(parse_type).collect()
}

#[cfg(test)]
mod tests {
    use crate::ast::{BinOp, Expr, Function, Item, Stmt, Type};
    use crate::parser::parse_program;

    

    
    fn extract_first_function(code: &str) -> Function {
        let program = parse_program(code).expect("Failed to parse");
        match &program.items[0] {
            Item::Fn(func) => func.clone(),
            _ => panic!("Expected Item::Fn, got {:?}", program.items[0]),
        }
    }

    fn extract_first_stmt_of_main(code: &str) -> Stmt {
        let func = extract_first_function(code);
        func.body[0].clone()
    }

    

    #[test]
    fn test_impl_parsing_distinction() {
        
        let code_trait = "impl Barks for Animal { fn bark() {} }";
        let program = parse_program(code_trait).unwrap();

        if let Item::Impl {
            trait_name,
            target_type,
            ..
        } = &program.items[0]
        {
            
            assert!(trait_name.is_some());
            if let Type::Named { name, .. } = trait_name.as_ref().unwrap() {
                assert_eq!(name, "Barks");
            }
            
            if let Type::Named { name, .. } = target_type {
                assert_eq!(name, "Animal");
            }
        } else {
            panic!("Expected Impl");
        }

        
        let code_regular = "impl Animal { fn new() {} }";
        let program2 = parse_program(code_regular).unwrap();

        if let Item::Impl {
            trait_name,
            target_type,
            ..
        } = &program2.items[0]
        {
            
            assert!(trait_name.is_none());
            
            if let Type::Named { name, .. } = target_type {
                assert_eq!(name, "Animal");
            }
        } else {
            panic!("Expected Impl");
        }
    }

    #[test]
    fn test_generics_everywhere() {
        let code = r#"
            fn map<T, U>(list: List<T>, func: Fn<T, U>) -> List<U> {
                let x: Result<List<T>, Error> = fetch()
            }
        "#;
        let func = extract_first_function(code);

        
        assert_eq!(func.generics, vec!["T", "U"]);

        
        let (p_name, p_type) = &func.params[0];
        assert_eq!(p_name, "list");
        
        if let Type::Named { name, generics } = p_type {
            assert_eq!(name, "List");
            if let Type::Named { name: inner, .. } = &generics[0] {
                assert_eq!(inner, "T");
            } else {
                panic!("Expected T");
            }
        }

        
        if let Some(Type::Named { name, generics }) = &func.ret_type {
            assert_eq!(name, "List");
            if let Type::Named { name: inner, .. } = &generics[0] {
                assert_eq!(inner, "U");
            }
        }
    }

    #[test]
    fn test_complex_math_precedence() {
        
        
        let code = "fn main() { 10 + 5 * 2 == 20 }";
        let stmt = extract_first_stmt_of_main(code);

        if let Stmt::Expr(Expr::Binary { op, lhs, rhs }) = stmt {
            
            assert!(matches!(op, BinOp::Eq));

            
            if let Expr::Binary {
                op: op_add,
                lhs: l_add,
                rhs: r_add,
            } = *lhs
            {
                assert!(matches!(op_add, BinOp::Add));
                
                
                if let Expr::Binary { op: op_mul, .. } = *r_add {
                    assert!(matches!(op_mul, BinOp::Mul));
                } else {
                    panic!("Expected Multiply");
                }
            } else {
                panic!("Expected Add");
            }
        } else {
            panic!("Expected Binary Expr");
        }
    }

    #[test]
    fn test_chained_calls_with_args() {
        
        let code = "fn main() { obj.method(1).prop.final(2, 3) }";
        let stmt = extract_first_stmt_of_main(code);

        
        if let Stmt::Expr(Expr::Call { callee, args }) = stmt {
            assert_eq!(args.len(), 2); 

            
            if let Expr::MemberAccess { object, field } = *callee {
                assert_eq!(field, "final");

                
                if let Expr::MemberAccess {
                    object: inner_obj,
                    field: f2,
                } = *object
                {
                    assert_eq!(f2, "prop");

                    
                    if let Expr::Call {
                        callee: method_callee,
                        args: m_args,
                    } = *inner_obj
                    {
                        assert_eq!(m_args.len(), 1);

                        
                        if let Expr::MemberAccess {
                            object: root,
                            field: f3,
                        } = *method_callee
                        {
                            assert_eq!(f3, "method");

                            
                            if let Expr::Variable { name: v, .. } = *root {
                                assert_eq!(v, "obj");
                            } else {
                                panic!("Root not variable");
                            }
                        }
                    }
                }
            }
        } else {
            panic!("Parsing failed");
        }
    }

    #[test]
    fn test_parsing_comments() {
        let code = r#"
            -- This is a comment
            fn main() { -- Inline comment
                let x = 5
                -- Another comment
            }
        "#;
        assert!(parse_program(code).is_ok());
    }

    #[test]
    fn test_mutable_pointer_syntax() {
        let code = "fn swap(a: mut *Int, b: mut *Int) {}";
        let func = extract_first_function(code);

        let (_, type_a) = &func.params[0];
        match type_a {
            Type::Pointer { mutable, inner } => {
                assert_eq!(*mutable, true);
                match &**inner {
                    Type::Named { name, .. } => assert_eq!(name, "Int"),
                    _ => panic!("Expected Int"),
                }
            }
            _ => panic!("Expected Pointer"),
        }
    }
}
