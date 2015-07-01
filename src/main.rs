extern crate readline;

mod errors;
mod number;
mod atom;
mod parser;

fn repl() {
    println!("Rust Lisp!");
    let mut env = Env::new();

    loop {
        match readline::readline(">") {
            Some(s) => {
                if let Ok(p) = parser::tokenize(&s) {
                    match eval(&p, &mut env) {
                        Ok(r) => println!("{}", r),
                        Err(e) => println!("Error in evaluation: {:?}", e)
                    }
                } else {
                    println!("Error in parsing");
                }
            },
            None => {
                println!("Exiting...");
                break;
            }
        }
    }
}

pub fn main() {
    repl();
}
