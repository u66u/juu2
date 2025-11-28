mod ast;
mod checker;
mod infer;
mod parser;
mod types;

fn main() {
    let source_code = r#"
        struct Animal<T> {
            age: Int
            name: String
        }

        fn main() {
            let mut x = 10 * (5 + 2)
            let dog = Animal<Int> { age: x, name: "Buddy" }
            dog.bark()

            let ss = dog
            
            x += 5
        }
    "#;

    println!("Parsing...");
    match parser::parse_program(source_code) {
        Ok(program) => {
            println!("{:#?}", program);
        }
        Err(e) => {
            eprintln!("Error: {}", e);
        }
    }
}
