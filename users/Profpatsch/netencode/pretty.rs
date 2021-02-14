extern crate netencode;

use netencode::{U, T, Tag};

pub enum Pretty {
    Single {
        leader: String,
        val: String,
        trailer: char,
    },
    Tag {
        leader: String,
        key: String,
        inner: char,
        val: Box<Pretty>,
        trailer: char,
    },
    Multi {
        leader: String,
        vals: Vec<Pretty>,
        trailer: char
    },
}

impl Pretty {
    pub fn from_u<'a>(u: U<'a>) -> Pretty {
        match u {
            U::Unit => Self::scalar("", "u"),
            U::N1(b) => Self::scalar("n1:", if b { "1" } else { "0" }),
            U::N3(n) => Self::scalar("n3:", n),
            U::N6(n) => Self::scalar("n6:", n),
            U::N7(n) => Self::scalar("n7:", n),
            U::I3(i) => Self::scalar("i3:", i),
            U::I6(i) => Self::scalar("i6:", i),
            U::I7(i) => Self::scalar("i7:", i),
            U::Text(s) => Pretty::Single {
                leader: format!("t{}:", s.len()),
                val: s.to_string(),
                trailer: ','
            },
            U::Binary(s) => Pretty::Single {
                leader: format!("b{}:", s.len()),
                // For pretty printing we want the string to be visible obviously.
                // Instead of not supporting binary, let’s use lossy conversion.
                val: String::from_utf8_lossy(s).into_owned(),
                trailer: ','
            },
            U::Sum(Tag{tag, val}) => Self::pretty_tag(tag, Self::from_u(*val)),
            U::Record(m) => Pretty::Multi {
                // TODO: we are losing the size here, should we recompute it? Keep it?
                leader: String::from("{"),
                vals: m.into_iter().map(|(k, v)| Self::pretty_tag(k, Self::from_u(v))).collect(),
                trailer: '}'
            },
            U::List(l) => Pretty::Multi {
                // TODO: we are losing the size here, should we recompute it? Keep it?
                leader: String::from("["),
                vals: l.into_iter().map(|v| Self::from_u(v)).collect(),
                trailer: ']',
            },
        }
    }

    fn scalar<D>(leader: &str, d: D) -> Pretty
    where D: std::fmt::Display
    {
        Pretty::Single {
            leader: leader.to_string(),
            val: format!("{}", d),
            trailer: ','
        }
    }

    fn pretty_tag(tag: &str, val: Pretty) -> Pretty {
        Pretty::Tag {
            leader: format!("<{}:", tag.len()),
            key: tag.to_string(),
            inner: '|',
            val: Box::new(val),
            trailer: ','
        }
    }

    pub fn print_multiline<W>(&self, mut w: &mut W) -> std::io::Result<()>
        where W: std::io::Write
    {
        Self::go(&mut w, self, 0)
    }

    fn go<W>(mut w: &mut W, p: &Pretty, depth: usize) -> std::io::Result<()>
        where W: std::io::Write
    {
        let i = &vec![b' '; depth*4];
        let i = unsafe {
            std::str::from_utf8_unchecked(i)
        };
        match p {
            Pretty::Single {leader, val, trailer} =>
                write!(&mut w, "{}{}{}{}", i, leader, val, trailer),
            Pretty::Tag { leader, key, inner, val, trailer } => {
                write!(&mut w, "{}{}{}{}", i, leader, key, inner)?;
                Self::go::<W>(&mut w, val, depth)?;
                write!(&mut w, "{}", trailer)
            },
            Pretty::Multi {leader, vals, trailer} => match vals.len() {
                0 => write!(&mut w, "{}{}{}", i, leader, trailer),
                1 => {
                    write!(&mut w, "{}{}", i, leader);
                    Self::go::<W>(&mut w, &vals[0], depth)?;
                    write!(&mut w, "{}{}", i, trailer)
                },
                more => {
                    write!(&mut w, "{}{}\n", i, leader)?;
                    for v in vals {
                        Self::go::<W>(&mut w, v, depth + 1)?;
                        write!(&mut w, "{}\n", i)?;
                    }
                    write!(&mut w, "{}{}\n", i, trailer)
                }
            },
        }
    }
}
