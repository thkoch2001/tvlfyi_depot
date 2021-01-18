use super::chunk;
use super::errors::*;
use super::opcode::OpCode;
use super::value::Value;

pub struct VM {
    chunk: chunk::Chunk,

    // TODO(tazjin): Accessing array elements constantly is not ideal,
    // lets see if something clever can be done with iterators.
    ip: usize,

    stack: Vec<Value>,
}

impl VM {
    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("fatal error: stack empty!")
    }
}

macro_rules! binary_op {
    ( $vm:ident, $op:tt ) => {{
        let a = $vm.pop();
        let b = $vm.pop();
        $vm.push(a $op b);
    }}
}

impl VM {
    fn run(&mut self) -> LoxResult<Value> {
        loop {
            let op = &self.chunk.code[self.ip];

            #[cfg(feature = "disassemble")]
            chunk::disassemble_instruction(&self.chunk, self.ip);

            self.ip += 1;

            match op {
                OpCode::OpReturn => return Ok(self.pop()),

                OpCode::OpConstant(idx) => {
                    let c = *self.chunk.constant(*idx);
                    self.push(c);
                }

                OpCode::OpNegate => {
                    let v = self.pop();
                    self.push(-v)
                }

                OpCode::OpAdd => binary_op!(self, +),
                OpCode::OpSubtract => binary_op!(self, -),
                OpCode::OpMultiply => binary_op!(self, *),
                OpCode::OpDivide => binary_op!(self, /),
            }
        }
    }
}

pub fn interpret(chunk: chunk::Chunk) -> LoxResult<Value> {
    let mut vm = VM {
        chunk,
        ip: 0,
        stack: vec![],
    };

    vm.run()
}
