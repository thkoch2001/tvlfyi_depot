use super::interner::InternedStr;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(LoxString),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LoxString {
    Heap(String),
    Interned(InternedStr),
}

impl From<String> for LoxString {
    fn from(s: String) -> Self {
        LoxString::Heap(s)
    }
}

impl From<InternedStr> for LoxString {
    fn from(s: InternedStr) -> Self {
        LoxString::Interned(s)
    }
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Nil => true,
            Value::Bool(false) => true,
            _ => false,
        }
    }
}
