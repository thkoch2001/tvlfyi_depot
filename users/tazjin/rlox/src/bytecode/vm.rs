use super::chunk;
use super::errors::*;
use super::interner::Interner;
use super::opcode::OpCode;
use super::value::{LoxString, Value};

pub struct VM {
    chunk: chunk::Chunk,

    // TODO(tazjin): Accessing array elements constantly is not ideal,
    // lets see if something clever can be done with iterators.
    ip: usize,

    stack: Vec<Value>,
    strings: Interner,
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
                OpCode::OpReturn => {
                    let val = self.pop();
                    return Ok(self.return_value(val));
                }

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

                OpCode::OpEqual => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Bool(a == b));
                }

                OpCode::OpLess => binary_op!(self, Number, Bool, <),
                OpCode::OpGreater => binary_op!(self, Number, Bool, >),

                OpCode::OpNegate => {
                    let v = self.pop();
                    with_type!(
                        self,
                        v,
                        Value::Number(num),
                        self.push(Value::Number(-num))
                    );
                }

                OpCode::OpSubtract => binary_op!(self, Number, -),
                OpCode::OpMultiply => binary_op!(self, Number, *),
                OpCode::OpDivide => binary_op!(self, Number, /),

                OpCode::OpAdd => {
                    let b = self.pop();
                    let a = self.pop();

                    match (a, b) {
                        (Value::String(s_a), Value::String(s_b)) => {
                            let mut new_s = self.resolve_str(&s_a).to_string();
                            new_s.push_str(self.resolve_str(&s_b));
                            self.push(Value::String(new_s.into()));
                        }

                        (Value::Number(n_a), Value::Number(n_b)) =>
                            self.push(Value::Number(n_a + n_b)),

                        _ => return Err(Error {
                            line: self.chunk.get_line(self.ip - 1),
                            kind: ErrorKind::TypeError(
                                "'+' operator only works on strings and numbers".into()
                            ),
                        })
                    }
                }
            }

            #[cfg(feature = "disassemble")]
            println!("=> {:?}", self.stack);
        }
    }

    // For some types of values (e.g. interned strings), returns
    // should no longer include any references into the interpreter.
    fn return_value(&self, val: Value) -> Value {
        match val {
            Value::String(string @ LoxString::Interned(_)) => {
                Value::String(self.resolve_str(&string).to_string().into())
            }
            _ => val,
        }
    }

    fn resolve_str<'a>(&'a self, string: &'a LoxString) -> &'a str {
        match string {
            LoxString::Heap(s) => s.as_str(),
            LoxString::Interned(id) => self.strings.lookup(*id),
        }
    }
}

pub fn interpret(strings: Interner, chunk: chunk::Chunk) -> LoxResult<Value> {
    let mut vm = VM {
        chunk,
        strings,
        ip: 0,
        stack: vec![],
    };

    vm.run()
}
