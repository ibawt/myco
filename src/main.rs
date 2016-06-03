#![feature(test)]
extern crate test;

extern crate readline;
#[macro_use]
extern crate lazy_static;
extern crate smallvec;

mod errors;
mod number;
mod symbol;
mod atom;
mod parser;
mod env;
mod eval;
mod funcs;
mod vm;
mod base_lib;

use parser::{tokenize};
use env::*;
use errors::Error;
use std::env::args;
use std::fs::File;
use std::io::prelude::*;
use vm::*;
use atom::*;

fn run_node(vm: &mut VirtualMachine, node: Atom, env: &mut Env) -> AtomResult {
    let mut out = vec![];
    let mut e = env.clone();
    vm::compile(node, &mut out, &mut e).unwrap();
    let mut frame = vm::empty_frame();
    frame.program.env = e;
    frame.program.body = out;
    vm.reset();
    vm.frames.push(frame);
    vm.run()
}

fn repl() {
    println!("Rust Lisp!");
    let mut env = Env::new(None);

    let mut vm = VirtualMachine::default();

    base_lib::init(&mut env).unwrap();

    for i in args().skip(1) {
        let mut file = File::open(i).unwrap();
        let mut s = String::new();
        file.read_to_string(&mut s).unwrap();

        let a = tokenize(&s)
            .and_then(|node| {
                run_node(&mut vm, node, &mut env)
                // eval(node, &mut env)
            }).unwrap();
        println!("{}", a);
    }

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
                    .and_then(|node| run_node(&mut vm, node, &mut env));

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
