extern crate readline;
#[macro_use]
extern crate lazy_static;

mod errors;
mod number;
mod symbol;
mod atom;
mod parser;
mod env;
mod eval;
mod funcs;
mod base_lib;

use parser::{tokenize};
use eval::{eval, expand};
use env::*;
use errors::Error;

fn repl() {
    println!("Rust Lisp!");
    let mut env = Env::new();

    base_lib::init(&mut env).unwrap();

    let mut lines: String = String::new();

    loop {
        let prompt = if lines.is_empty() { ">" } else  { "" };

        match readline::readline(prompt) {
            Some(s) => {
                if s == "quit" {
                    return;
                }
                lines.push_str(&s);

                let result = tokenize(&lines)
                    .and_then(|node| expand(&node, &mut env, 0))
                    .and_then(|node| eval(&node, &mut env));

                match result {
                    Ok(r) => {
                        println!("{}", r);
                        lines.clear();
                    },
                    Err(Error::EoF) => {
                    },
                    Err(e) => {
                        println!("Error in evaluation: {:?}", e);
                        lines.clear();
                    }
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
