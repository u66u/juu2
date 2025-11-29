mod ast;
mod checker;
mod codegen;
mod infer;
mod ir;
mod monomorphize;
mod parser;
mod types;

use crate::codegen::c;
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
            fn bark(obj: Self) -> String {
                "bark"
            }
        }

        typeclass Adder {
            fn add(obj: Self) -> Int {}
        }

        impl Adder for Point {
            fn add(obj: Self) -> Int {
                obj.x + obj.y
            }
        }

        impl<T> Animal for Gen<T> {
            fn bark(obj: Self) -> String {
                "bark"
            }
        }

        fn dot_product(a: Vector, b: Vector) -> Int {
            a.x.x * b.x.x + a.y.y * b.y.y
        }

        fn main() {
            let xd = 5
            let g = Gen<Int> { internal: 5}
            g.bark()
            let a = Point {x: 5, y: 5}
            a.add()
        }
    "#;

    println!("--- Source Code ---");
    println!("{}", source_code);
    println!("-------------------");

    println!("Parsing...");
    match parser::parse_program(source_code) {
        Ok(program) => {
            println!("Ast: {:?}", &program);

            println!("2. Checking Types...");
            match checker::check_program(&program) {
                Ok(ctx) => {
                    println!(" Type Check Successful!");
                    println!("\n--- Inferred Globals ---");
                    for (name, scheme) in &ctx.functions {
                        println!("Function {}: {:?}", name, scheme.ty);
                    }
                    println!("\n--- Registered Structs ---");
                    for (name, info) in &ctx.types {
                        println!("Struct {}: {:?}", name, info.fields);
                    }
                    let ir = monomorphize::run(&program, &ctx);
                    let c_code = c::generate(&ir);

                    println!("\n--- Generated C Code ---");
                    println!("{}", c_code);

                    if !fs::exists("./out").unwrap() {
                        fs::create_dir("./out").unwrap();
                    }
                    fs::write("out/output.c", c_code).expect("Unable to write file");
                }
                Err(e) => {
                    eprintln!("   Type Error: {}", e);
                }
            }
        }
        Err(e) => {
            eprintln!("   Parse Error: {}", e);
        }
    }
}
