use std::fmt;

pub struct Person {
    pub fname: String,
    pub lname: String,
    pub age: i8,
}

impl fmt::Display for Person {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}, {} ({} years old)", self.lname, self.fname, self.age)
    }
}
