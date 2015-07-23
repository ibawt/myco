//////////////////////////////////////////////////////////////////////////////
// Copyright 2015 Ian Quick <ian.quick@gmail.com>                           //
//                                                                          //
// Licensed under the Apache License, Version 2.0 (the "License");          //
// you may not use this file except in compliance with the License.         //
// You may obtain a copy of the License at                                  //
//                                                                          //
//   http://www.apache.org/licenses/LICENSE-2.0                             //
//                                                                          //
// Unless required by applicable law or agreed to in writing, software      //
// distributed under the License is distributed on an "AS IS" BASIS,        //
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. //
// See the License for the specific language governing permissions and      //
// limitations under the License.                                           //
//////////////////////////////////////////////////////////////////////////////
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
                    .and_then(|node| {
                        println!("<-- tokenize: {}", node);
                        expand(&node, &mut env, 0)
                    })
                    .and_then(|node| {
                        println!("<-- expand: {}", node);
                        eval(&node, &mut env)
                    });

                match result {
                    Ok(r) => println!("== {} ==", r),
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
