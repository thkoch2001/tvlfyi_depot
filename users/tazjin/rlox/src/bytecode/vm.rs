use std::collections::HashMap;

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

    globals: HashMap<LoxString, Value>,

    // Operations that consume values from the stack without pushing
    // anything leave their last value in this slot, which makes it
    // possible to return values from interpreters that ran code which
    // ended with a statement.
    last_drop: Option<Value>,
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
                    if !self.stack.is_empty() {
                        let val = self.pop();
                        return Ok(self.return_value(val));
                    } else if self.last_drop.is_some() {
                        let val = self.last_drop.take().unwrap();
                        return Ok(self.return_value(val));
                    } else {
                        return Ok(Value::Nil);
                    }
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

                OpCode::OpPrint => {
                    let val = self.pop();
                    println!("{}", self.print_value(val));
                }

                OpCode::OpPop => {
                    self.last_drop = Some(self.pop());
                }

                OpCode::OpDefineGlobal(name_idx) => {
                    let name = self.chunk.constant(*name_idx);
                    with_type!(self, name, Value::String(name), {
                        let name = name.clone();
                        let val = self.pop();
                        self.globals.insert(name, val);
                    });
                }

                OpCode::OpGetGlobal(name_idx) => {
                    let name = self.chunk.constant(*name_idx);
                    with_type!(self, name, Value::String(name), {
                        let val = match self.globals.get(name) {
                            None => unimplemented!("variable not found error"),
                            Some(val) => val.clone(),
                        };
                        self.push(val)
                    });
                }

                OpCode::OpSetGlobal(name_idx) => {
                    let name = self.chunk.constant(*name_idx).clone();
                    let new_val = self.pop();
                    with_type!(self, name, Value::String(name), {
                        match self.globals.get_mut(&name) {
                            None => unimplemented!("variable not found error"),
                            Some(val) => {
                                *val = new_val;
                            }
                        }
                    });
                }

                OpCode::OpGetLocal(local_idx) => {
                    let value = self.stack[local_idx.0].clone();
                    self.push(value);
                }

                OpCode::OpSetLocal(local_idx) => {
                    debug_assert!(self.stack.len() > local_idx.0, "stack is not currently large enough for local");
                    self.stack[local_idx.0] = self.stack.last().unwrap().clone();
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

    fn print_value(&self, val: Value) -> String {
        match val {
            Value::String(LoxString::Heap(s)) => s,
            Value::String(LoxString::Interned(id)) => {
                self.strings.lookup(id).into()
            }
            _ => format!("{:?}", val),
        }
    }
}

pub fn interpret(strings: Interner, chunk: chunk::Chunk) -> LoxResult<Value> {
    let mut vm = VM {
        chunk,
        strings,
        globals: HashMap::new(),
        ip: 0,
        stack: vec![],
        last_drop: None,
    };

    vm.run()
}
