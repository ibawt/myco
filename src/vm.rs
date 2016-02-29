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
    env: Env
}

#[derive (Debug)]
pub struct VirtualMachine {
    stack: Vec<Atom>,
    frames: Vec<Frame>,
    env: Env,
    pc: usize,
    fp: usize,
    sp: usize,
}

fn compile(node: &[Atom]) -> Vec<Instruction> {
    let mut inst = vec![];

    // inst.push(Instruction::LOADI(1));
    // inst.push(Instruction::LOADI(2));
    // inst.push(Instruction::ADD);
    // inst.push(Instruction::LOADI(3));
    // inst.push(Instruction::ADD);

    inst
}

#[derive (Debug,Clone, Copy, PartialEq)]
enum Instruction {
    CONST,
    LOAD,
    STORE,
    DEFINE,
    FUNCTION,
    JUMP,
    CALL,
}

use self::Instruction::*;

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
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
