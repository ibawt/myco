#![feature(test)]
extern crate test;

#[macro_use]
extern crate log;
extern crate env_logger;
extern crate linenoise;

mod errors;
mod number;
mod symbol;
mod atom;
mod opcodes;
mod parser;
mod env;
mod eval;
mod funcs;
mod compiler;
mod vm;
mod base_lib;

use errors::Error;
use std::env::args;
use std::fs::File;
use std::io::prelude::*;
use vm::*;

fn repl() {
    env_logger::init().unwrap();

    println!("Rust Lisp!");

    let mut vm = VirtualMachine::default();

    for i in args().skip(1) {
        let mut file = File::open(i).unwrap();
        let mut s = String::new();
        file.read_to_string(&mut s).unwrap();

        let a = vm.eval_string(&s).unwrap();
        println!("{}", a);
    }

    let mut lines: String = String::new();

    loop {
        let prompt = if lines.is_empty() {
            ">"
        } else {
            ""
        };
        match linenoise::input(prompt) {
            Some(s) => {
                match &s[..] {
                    "quit" => return,
                    ",print-stack" => {
                        vm.print_stack();
                    }
                    _ => {
                        lines.push_str(&s);

                        let result = vm.eval_string(&lines);

                        match result {
                            Ok(r) => {
                                println!("{}", r);
                                linenoise::history_add(&lines);
                                lines.clear();
                            }
                            Err(Error::EoF) => {}
                            Err(e) => {
                                println!("Error in evaluation: {:?}", e);
                                lines.clear();
                            }
                        }
                    }
                }
            }
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
