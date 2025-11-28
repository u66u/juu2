mod ast;
mod checker;
mod monomorphize;
mod infer;
mod ir;
mod parser;
mod types;

use std::fs;

fn main() {
    let source_code = r#"
        struct Point {
            x: Int,
            y: Int
        }
        struct Vector {
            x: Point,
            y: Point
        }

        struct Gen<T> {
            internal: T
        }

        typeclass Animal {
            fn bark(obj: mut *Self) -> String {
                "bark"
            }
        }

        impl<T> Animal for Gen<T> {
            fn bark(obj: Self) {
                "bark"
            }
        }

        fn dot_product(a: Vector, b: Vector) -> Int {
            a.x.x * b.x.x + a.y.y * b.y.y
        }

        fn main() {
            let xd = 5
            let g = Gen<Int> {}
            g.bark()
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
