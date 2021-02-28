#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
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
