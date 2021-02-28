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

macro_rules! with_type {
    ( $self:ident, $val:ident, $type:pat, $body:expr ) => {
        match $val {
            $type => $body,
            _ => {
                return Err(Error {
                    line: $self.chunk.get_line($self.ip - 1),
                    kind: ErrorKind::TypeError(format!(
                        "Expected type {}, but found value: {:?}",
                        stringify!($type),
                        $val,
                    )),
                })
            }
        }
    };
}

macro_rules! binary_op {
    ( $vm:ident, $type:tt, $op:tt ) => {
        binary_op!($vm, $type, $type, $op)
    };

    ( $vm:ident, $in_type:tt, $out_type:tt, $op:tt ) => {{
        let b = $vm.pop();
        let a = $vm.pop();

        with_type!($vm, b, Value::$in_type(val_b), {
            with_type!($vm, a, Value::$in_type(val_a), {
                $vm.push(Value::$out_type(val_a $op val_b))
            })
        })
    }};
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
                    let c = self.chunk.constant(*idx).clone();
                    self.push(c);
                }

                OpCode::OpNil => self.push(Value::Nil),
                OpCode::OpTrue => self.push(Value::Bool(true)),
                OpCode::OpFalse => self.push(Value::Bool(false)),

                OpCode::OpNot => {
                    let v = self.pop();
                    self.push(Value::Bool(v.is_falsey()));
                }

                OpCode::OpNegate => {
                    let v = self.pop();
                    with_type!(
                        self,
                        v,
                        Value::Number(num),
                        self.push(Value::Number(-num))
                    );
                }

                OpCode::OpAdd => binary_op!(self, Number, +),
                OpCode::OpSubtract => binary_op!(self, Number, -),
                OpCode::OpMultiply => binary_op!(self, Number, *),
                OpCode::OpDivide => binary_op!(self, Number, /),
            }

            #[cfg(feature = "disassemble")]
            println!("=> {:?}", self.stack);
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
