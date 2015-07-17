extern crate readline;

mod errors;
mod number;
mod atom;
mod parser;
mod env;
mod eval;
mod funcs;

use parser::{tokenize};
use eval::{eval, expand};
use env::*;

fn repl() {
    println!("Rust Lisp!");
    let mut env = Env::new();

    loop {
        match readline::readline(">") {
            Some(s) => {
                let result = tokenize(&s)
                    .and_then(|node| expand(&node, &mut env, 0))
                    .and_then(|node| eval(&node, &mut env));

                match result {
                    Ok(r) => println!("{:?}", r),
                    Err(e) => println!("Error in evaluation: {:?}", e)
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
