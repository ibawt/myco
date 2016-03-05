use atom::*;
use env::Env;
use errors::*;
use symbol::*;
use funcs::*;
use self::Instruction::*;

#[derive (Debug, Clone)]
struct Frame {
    program: CompiledFunction,
    pc: usize
}

impl Frame {
    fn new(f: CompiledFunction) -> Frame {
        Frame {
            program: f,
            pc: 0
        }
    }

    fn current_instruction(&self) -> Option<&Instruction> {
        self.program.body.get(self.pc)
    }
}

#[derive (Debug)]
pub struct VirtualMachine {
    stack: Vec<Atom>,
    frames: Vec<Frame>,
    fp: usize,
    sp: usize,
}


#[derive (Debug, Clone, PartialEq)]
pub enum Instruction {
    CONST(Atom),
    LOAD(InternedStr),
    DEFINE(InternedStr),
    POP,
    // FUNCTION,
    JUMP,
    RETURN,
    CALL(Function, usize),
    DCALL(usize)
}

fn compile_node(node: Atom, out: &mut Vec<Instruction>, env: &Env) -> Result<(), Error> {
    match node {
        Atom::Symbol(sym) => {
            out.push(Instruction::LOAD(sym))
        },
        Atom::List(ref list) => {
            for n in list.iter() {
                try!(compile(n.clone(), out, env));
            }
        },
        // handle constants
        _ => {
            out.push(Instruction::CONST(node.clone()));
        }
    }
    Ok(())
}

pub fn compile(node: Atom, out: &mut Vec<Instruction>, env: &Env) -> Result<(), Error> {
    println!("compiling: {}", node);
    match node {
        Atom::List(ref list) => {
            if list.is_empty() {
                out.push(Instruction::CONST(node.clone()));
                return Ok(())
            }
            ()
        },
        _ => return compile_node(node, out, env)
    }

    let list = try!(node.as_list());

    if let Atom::List(_) = list[0] {
        for i in list.iter().skip(1) {
            try!(compile(i.clone(), out, env));
        }
        try!(compile(list[0].clone(), out, env));
        out.push(DCALL(list.len() - 1));
        return Ok(())
    }

    match list[0] {
        Atom::Form(f) => {
            match f {
                Form::Set => {
                },
                Form::Def => {
                    let sym = try!(list[1].as_symbol());
                    try!(compile(list[2].clone(), out, env));
                    out.push(DEFINE(*sym));
                    return Ok(())
                },
                Form::Do => {
                    if list.len() > 2 {
                        for i in 1..list.len()-1 {
                            try!(compile(list[i].clone(), out, env));
                            out.push(POP);
                        }
                    }
                    return compile(list[list.len()-1].clone(), out, env)
                },
                Form::Fn => {
                    let mut body = Vec::new();
                    let mut env = Env::new(Some(env.clone()));
                    try!(compile(list[2].clone(), &mut body, &mut env));
                    body.push(RETURN);
                    let func = CompiledFunction {
                        body: body,
                        params: try!(list[1].as_list()).clone(),
                        env: env
                    };
                    out.push(CONST(Atom::Function(Function::Compiled(func))));
                    return Ok(())
                }
                _ => {
                    return Err(Error::NotImplemented)
                }
            }
        },
        Atom::Symbol(_) => {
            for n in list.iter().skip(1) {
                try!(compile(n.clone(), out, env));
            }
            try!(compile_node(list[0].clone(), out, env));
            out.push( DCALL(list.len() -1 ));
            return Ok(())
        }
        Atom::Function(ref func) => {
            for n in list.iter().skip(1) {
                try!(compile(n.clone(), out, env));
            }
            match *func {
                Function::Native(_) => {
                    out.push(Instruction::CALL(func.clone(), list.len() -1));
                    return Ok(())
                },
                Function::Compiled(_) => {
                    out.push(CALL(func.clone(), list.len() -1));
                    return Ok(())
                },
                _ => {
                    return Err(Error::NotImplemented)
                }
            }
        },
        _ => {
            println!("it's a {}",list[0]);
            println!("output so far is: {:?}", out);
            return Err(Error::NotAFunction)
        }
    }

    Err(Error::InvalidArguments)
}

fn eval_native_borrow(v: &mut VirtualMachine, n: Native, len: usize) -> AtomResult {
    let &mut VirtualMachine { ref mut stack, ref mut frames, fp, .. } = v;
    eval_native(n, &stack[len..], &mut frames[fp].program.env)
}

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            stack: vec![],
            frames: vec![],
            sp: 0,
            fp: 0
        }
    }

    fn next_instruction(&self) -> Option<Instruction> {
        self.frames[self.fp].current_instruction().map(|n| n.clone())
    }

    fn current_frame(&mut self) -> &mut Frame {
        &mut self.frames[self.fp]
    }

    fn current_env(&mut self) -> &mut Env {
        &mut self.current_frame().program.env
    }

    pub fn run(&mut self) -> AtomResult {
        while let Some(instruction) = self.next_instruction() {
            match instruction {
                POP => {
                    self.pop();
                }
                RETURN => {
                    if self.fp > 0 {
                        self.fp -= 1;
                        self.frames.pop();
                    } else {
                        return Ok(self.pop())
                    }
                },
                LOAD(sym) => {
                    let value = self.current_frame().program.env.get(sym.as_ref()).unwrap_or(Atom::Nil);
                    // println!("loading: {} => {} from env: {}",  sym, value, &self.current_frame().program.env);
                    self.push(value);
                },
                CONST(ref atom) => {
                    self.push(atom.clone())
                },
                DEFINE(sym) => {
                    let v = self.pop();
                    let a = self.current_env().define(sym, v).unwrap();
                    self.push(a);
                },
                DCALL(arity) => {
                    let func = self.pop();

                    match func {
                        Atom::Function(Function::Compiled(mut f)) => {
                            f.env.bind_mut(&f.params, &self.stack[self.sp-arity..]);
                            // println!("binding to env: {}", &f.env);
                            for _ in 0..arity {
                                self.pop();
                            }
                            self.frames.push(Frame::new(f.clone()));
                            self.fp += 1;
                            continue; // don't advance PC of the new frame
                        },
                        _ => {
                            println!("not a function: {:?}", func);
                            return Err(Error::NotAFunction)
                        }
                    }
                },
                CALL(func, arity) => {
                    match func {
                        Function::Native(n) => {
                            let len = self.stack.len();
                            let r = try!(eval_native_borrow(self, n, len - arity));
                            for _ in 1..arity {
                                self.pop();
                            }
                            self.push(r);
                        },
                        _ => {
                            return Err(Error::NotImplemented)
                        }
                    }
                },
                _ => ()
            }
            self.current_frame().pc += 1;
        }
        Ok(self.pop())
    }

    fn push(&mut self, a: Atom)  {
        self.stack.push(a);
        self.sp += 1
    }

    fn pop(&mut self) -> Atom {
        self.sp -= 1;
        self.stack.pop().unwrap()
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use parser::*;
    use atom::*;
    use env::*;
    use super::Frame;

    fn empty_frame() -> Frame {
        Frame::new(CompiledFunction{
            body: Vec::new(),
            params: empty_list(),
            env: Env::new(None)
        })
    }

    fn run_expr(s: &str) -> Atom {
        let p = tokenize(s).unwrap();
        let mut out = Vec::new();
        let env = Env::new(None);
        compile(p, &mut out, &env).unwrap();
        println!("output: {:?}", out);
        let mut vm = VirtualMachine::new();
        let mut f = empty_frame();
        f.program.env = env;
        f.program.body = out;
        vm.frames.push(f);
        vm.run().unwrap()
    }

    #[test]
    fn test() {
        assert_eq!(Atom::from(6), run_expr("(+ (+ 2 2) 2)"));
        assert_eq!(Atom::symbol("a"), run_expr("(def a 1)"));
        assert_eq!(Atom::from(3), run_expr("(do (def foo (fn (x) (+ x 2))) (foo 1))"));
    }

    #[test]
    fn test_invoke_fn_inline() {
        assert_eq!(Atom::from(1), run_expr("((fn (x) 1) 0)"));
    }

    #[test]
    fn test_closure() {
        assert_eq!(Atom::from(2), run_expr("(do (def foo (fn (x) (fn () (* 2 x)))) (def foo2 (foo 1)) (foo2))"))
    }
}
