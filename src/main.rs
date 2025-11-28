mod ast;
mod ast_to_ir;
mod checker;
mod infer;
mod ir;
mod parser;
mod types;

use std::fs;

fn main() {
    let source_code = r#"
        struct Vector {
            x: Int,
            y: Int
        }

        typeclass Animal {
            fn bark(obj: mut *Self) -> String {
}
        }

        fn dot_product(a: Vector, b: Vector) -> Int {
            -- Implicit return of the math expression
            a.x * b.x + a.y * b.y
        }

        fn main() {
            let xd = 5
            let v1 = Vector { x: 10, y: 20 }
            
            -- Type inference should figure out v2 is a Vector
            let v2 = Vector { x: 3, y: 4 }
            
            let result = dot_product(v1, v2)
            
            -- This variable 'check' is inferred as Int
            let check = result + 100
        }
    "#;

    println!("--- Source Code ---");
    println!("{}", source_code);
    println!("-------------------");

    println!("1. Parsing...");
    match parser::parse_program(source_code) {
        Ok(program) => {
            println!("   ✅ Parse Successful!");
            println!("Ast: {:?}", &program);

            println!("2. Checking Types...");
            match checker::check_program(&program) {
                Ok(ctx) => {
                    println!("   ✅ Type Check Successful!");
                    println!("\n--- Inferred Globals ---");
                    for (name, scheme) in &ctx.functions {
                        println!("Function {}: {:?}", name, scheme.ty);
                    }
                    println!("\n--- Registered Structs ---");
                    for (name, info) in &ctx.types {
                        println!("Struct {}: {:?}", name, info.fields);
                    }
                }
                Err(e) => {
                    eprintln!("   ❌ Type Error: {}", e);
                }
            }
        }
        Err(e) => {
            eprintln!("   ❌ Parse Error: {}", e);
        }
    }
}
