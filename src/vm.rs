use atom::*;
use number::*;
use env::Env;

#[derive (Debug)]
struct Function2 {
    program: Vec<Instruction>,
    env: Env,
    arity: u8
}

#[derive (Debug)]
struct Frame {
    program: Function2,
    sp: usize,
    env: Env,
    caller: usize
}

#[derive (Debug)]
pub struct VirtualMachine {
    program: Vec<Instruction>,
    stack: Vec<StackElement>,
    frames: Vec<Frame>,
    env: Env,
    pc: usize,
    fp: usize,
    sp: usize,
}

#[derive (Debug)]
enum StackElement {
    Atom(Atom),
}

fn compile(node: &[Atom]) -> Vec<Instruction> {
    let mut inst = vec![];

    inst.push(Instruction::LOADI(1));
    inst.push(Instruction::LOADI(2));
    inst.push(Instruction::ADD);
    inst.push(Instruction::LOADI(3));
    inst.push(Instruction::ADD);

    inst
}

#[derive (Debug,Clone, Copy, PartialEq)]
enum Instruction {
    // control
    NOP,
    DUP,
    POP,
    CALL,
    TCALL,
    JUMP(usize),
    JUMP_REL(i64),
    RET,

    //lispyness
    CONS,
    LIST,
    APPLY,

    // equivalence
    EQ,
    EQV,
    EQUAL,
    NOT,

    // predicates
    NULLP,
    BOOLEANP,
    SYMBOLP,
    NUMBERP,
    LISTP,
    KEYWORDP,
    FUNCTIONP,
    MACROP,

    // math
    ADD, SUB, MUL, DIV, IDIV, NUMEQ, LT, COMPARE,

    LOADI(i64),
    LOAD_ENV,
    PRINT,
}

use self::Instruction::*;

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            program: vec![],
            stack: vec![],
            frames: vec![],
            env: Env::new(None),
            pc: 0,
            sp: 0,
            fp: 0
        }
    }

    pub fn run(&mut self) -> AtomResult {
        loop {
            if self.pc >= self.program.len() {
                break;
            }
            match self.program[self.pc] {
                ADD => {
                    let a = self.pop().as_number().unwrap();
                    let b = self.pop().as_number().unwrap();
                    self.push( Atom::Number(a + b));
                    self.pc += 1;
                },
                LOADI(i) => {
                    self.push( Atom::Number(Number::Integer(i)));
                    self.pc += 1;
                },
                PRINT => {
                    println!("{}", self.pop());
                    self.pc += 1;
                },
                JUMP_REL(offset) => {
                    self.pc = ((self.pc as i64) + offset) as usize;
                    assert!( self.pc < self.program.len())
                },
                JUMP(addr) => {
                    self.pc = addr;
                    assert!( self.pc < self.program.len())
                },
                POP => {
                    self.pop();
                },
                LOAD_ENV => {
                    let atom = self.pop();
                    let key = try!(atom.as_symbol());
                    let v = self.env.get(key);
                    self.push(v.unwrap_or(Atom::Nil));
                    self.pc += 1;
                },
                CALL => {
                    let func = self.pop();
                    match try!(func.as_function()) {
                        &Function::Proc(ref p) => {
                            let e = Env::new(Some(p.closures.clone()))
                                .bind(&p.params, &self.stack[self.sp-p.len()..p.len()]);
                            for _ in 1..p.len() {
                                self.pop();
                                self.sp -= 1;
                            }
                        }
                        _ => ()
                    }
                }
                _ => ()
            }
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
