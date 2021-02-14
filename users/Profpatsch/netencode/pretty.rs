extern crate netencode;

use netencode::{U, T, Tag};

pub enum Pretty {
    Single {
        r#type: char,
        length: String,
        val: String,
        trailer: char,
    },
    Tag {
        r#type: char,
        length: String,
        key: String,
        inner: char,
        val: Box<Pretty>,
    },
    Multi {
        r#type: char,
        length: String,
        vals: Vec<Pretty>,
        trailer: char
    },
}

impl Pretty {
    pub fn from_u<'a>(u: U<'a>) -> Pretty {
        match u {
            U::Unit => Self::scalar('u', "", ""),
            U::N1(b) => Self::scalar('n', "1:", if b { "1" } else { "0" }),
            U::N3(n) => Self::scalar('n', "3:", n),
            U::N6(n) => Self::scalar('n', "6:", n),
            U::N7(n) => Self::scalar('n', "7:", n),
            U::I3(i) => Self::scalar('i', "3:", i),
            U::I6(i) => Self::scalar('i', "6:", i),
            U::I7(i) => Self::scalar('i', "7:", i),
            U::Text(s) => Pretty::Single {
                r#type: 't',
                length: format!("{}:", s.len()),
                val: s.to_string(),
                trailer: ','
            },
            U::Binary(s) => Pretty::Single {
                r#type: 'b',
                length: format!("{}:", s.len()),
                // For pretty printing we want the string to be visible obviously.
                // Instead of not supporting binary, let’s use lossy conversion.
                val: String::from_utf8_lossy(s).into_owned(),
                trailer: ','
            },
            U::Sum(Tag{tag, val}) => Self::pretty_tag(tag, Self::from_u(*val)),
            U::Record(m) => Pretty::Multi {
                r#type: '{',
                // TODO: we are losing the size here, should we recompute it? Keep it?
                length: String::from(""),
                vals: m.into_iter().map(|(k, v)| Self::pretty_tag(k, Self::from_u(v))).collect(),
                trailer: '}'
            },
            U::List(l) => Pretty::Multi {
                r#type: '[',
                // TODO: we are losing the size here, should we recompute it? Keep it?
                length: String::from(""),
                vals: l.into_iter().map(|v| Self::from_u(v)).collect(),
                trailer: ']',
            },
        }
    }

    fn scalar<D>(r#type: char, length: &str, d: D) -> Pretty
    where D: std::fmt::Display
    {
        Pretty::Single {
            r#type,
            length: length.to_string(),
            val: format!("{}", d),
            trailer: ','
        }
    }

    fn pretty_tag(tag: &str, val: Pretty) -> Pretty {
        Pretty::Tag {
            r#type: '<',
            length: format!("{}:", tag.len()),
            key: tag.to_string(),
            inner: '|',
            val: Box::new(val),
        }
    }

    pub fn print_multiline<W>(&self, mut w: &mut W) -> std::io::Result<()>
        where W: std::io::Write
    {
        Self::go(&mut w, self, 0, true);
        write!(w, "\n")
    }

    fn go<W>(mut w: &mut W, p: &Pretty, depth: usize, is_newline: bool) -> std::io::Result<()>
        where W: std::io::Write
    {
        const full : usize = 4;
        const half : usize = 2;
        let i = &vec![b' '; depth*full];
        let iandhalf = &vec![b' '; depth*full + half];
        let (i, iandhalf) = unsafe {(
            std::str::from_utf8_unchecked(i),
            std::str::from_utf8_unchecked(iandhalf),
        )};
        if is_newline {
            write!(&mut w, "{}", i);
        }
        match p {
            Pretty::Single {r#type, length, val, trailer} =>
                write!(&mut w, "{} {}{}", r#type, val, trailer),
            Pretty::Tag { r#type, length, key, inner, val } => {
                write!(&mut w, "{} {} {}", r#type, key, inner)?;
                Self::go::<W>(&mut w, val, depth, false)
            },
            // if the length is 0 or 1, we print on one line,
            // only if there’s more than one element we split the resulting value.
            // we never break lines on arbitrary column sizes, since that is just silly.
            Pretty::Multi {r#type, length, vals, trailer} => match vals.len() {
                0 => write!(&mut w, "{} {}", r#type, trailer),
                1 => {
                    write!(&mut w, "{} ", r#type);
                    Self::go::<W>(&mut w, &vals[0], depth, false)?;
                    write!(&mut w, "{}", trailer)
                },
                more => {
                    write!(&mut w, "\n{}{} \n", iandhalf, r#type)?;
                    for v in vals {
                        Self::go::<W>(&mut w, v, depth + 1, true)?;
                        write!(&mut w, "\n")?;
                    }
                    write!(&mut w, "{}{}", iandhalf, trailer)
                }
            },
        }
    }
}
