use atom::*;
use number::*;
use env::Env;
use errors::*;
use symbol::*;
use parser::*;
use funcs::*;
use self::Instruction::*;

#[derive (Debug)]
struct Function2 {
    program: Vec<Instruction>,
    env: Env,
    arity: u8
}

#[derive (Debug)]
struct Frame {
    program: Function2,
    env: Env
}

#[derive (Debug)]
pub struct VirtualMachine {
    stack: Vec<Atom>,
    program: Vec<Instruction>,
    frames: Vec<Frame>,
    env: Env,
    pc: usize,
    fp: usize,
    sp: usize,
}


#[derive (Debug, Clone, PartialEq)]
pub enum Instruction {
    CONST(Atom),
    LOAD(InternedStr),
    STORE,
    DEFINE,
    FUNCTION,
    JUMP,
    CALL(Function, usize),
}

fn compile_node(node: Atom, out: &mut Vec<Instruction>) -> Result<Atom, Error> {
    match node {
        Atom::Symbol(sym) => {
            out.push(Instruction::LOAD(sym))
        },
        Atom::List(ref list) => {
            for n in list.iter() {
                try!(compile(n.clone(), out));
            }
        },
        // handle constants
        _ => {
            out.push(Instruction::CONST(node.clone()));
        }
    }
    Ok(Atom::Nil)
}

pub fn compile(node: Atom, out: &mut Vec<Instruction>) -> Result<Atom, Error> {
    match node {
        Atom::List(ref list) => {
            if list.is_empty() {
                out.push(Instruction::CONST(node.clone()));
                return Ok(Atom::Nil)
            }
            ()
        },
        _ => return compile_node(node, out)
    }

    let list = try!(node.as_list());

    match list[0] {
        Atom::Form(f) => {
            match f {
                Form::Set => {
                },
                Form::Def => {
                },
                Form::Do => {
                    for i in list.iter().skip(1) {
                        try!(compile(i.clone(), out));
                    }
                    return Ok(Atom::Nil)
                }
                _ => {
                    return Err(Error::NotImplemented)
                }
            }
        },
        Atom::Function(ref func) => {
            for n in list.iter().skip(1) {
                try!(compile(n.clone(), out));
            }
            match *func {
                Function::Native(_) => {
                    out.push(Instruction::CALL(func.clone(), list.len() -1));
                    return Ok(Atom::Nil)
                },
                _ => {
                    return Err(Error::NotImplemented)
                }
            }
        },
        _ => {
            return Err(Error::NotAFunction)
        }
    }

    Err(Error::InvalidArguments)
}

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            stack: vec![],
            program: vec![],
            frames: vec![],
            env: Env::new(None),
            pc: 0,
            sp: 0,
            fp: 0
        }
    }

    fn current_instruction(&self) -> Instruction {
        // self.frames[self.fp].program.program[self.pc].clone()
        self.program[self.pc].clone()
    }

    pub fn run(&mut self) -> AtomResult {
        loop {
            use self::Instruction::*;
            match self.current_instruction() {
                CONST(ref atom) => {
                    self.push(atom.clone())
                },
                CALL(func, arity) => {
                    match func {
                        Function::Native(n) => {
                            let len = self.stack.len();
                            let r = try!(eval_native(n, &self.stack[len-arity..], &mut self.env));
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
            self.pc += 1;
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
    #[test]
    fn test() {
        let p = tokenize("(+ (+ 2 2) 2)").unwrap();

        let mut out = vec![];

        compile(p, &mut out).unwrap();

        let mut vm = VirtualMachine::new();

        vm.program = out;

        let result = vm.run().unwrap();

        assert_eq!(Atom::from(6), result);
    }
}
