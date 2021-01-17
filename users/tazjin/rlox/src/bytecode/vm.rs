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

impl VM {
    fn run(&mut self) -> LoxResult<()> {
        loop {
            let op = &self.chunk.code[self.ip];

            #[cfg(feature = "disassemble")]
            chunk::disassemble_instruction(&self.chunk, self.ip);

            self.ip += 1;

            match op {
                OpCode::OpReturn => {
                    println!("{:?}", self.pop());
                    return Ok(());
                }

                OpCode::OpConstant(idx) => {
                    let c = *self.chunk.constant(*idx);
                    self.push(c);
                }
            }
        }
    }
}

pub fn interpret(chunk: chunk::Chunk) -> LoxResult<()> {
    let mut vm = VM {
        chunk,
        ip: 0,
        stack: vec![],
    };

    vm.run()
}
