extern crate nom;
extern crate exec_helpers;

use std::collections::HashMap;
use std::io::{Write, Read};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum T {
    // Unit
    Unit,
    // Boolean
    N1(bool),
    // Naturals
    N3(u8),
    N6(u64),
    N7(u128),
    // Integers
    I3(i8),
    I6(i64),
    I7(i128),
    // Text
    // TODO: make into &str
    Text(String),
    Binary(Vec<u8>),
    // Tags
    // TODO: make into &str
    Sum(Tag<String, T>),
    // TODO: make into &str
    Record(HashMap<String, T>),
    List(Vec<T>),
}

impl T {
    fn to_u<'a>(&'a self) -> U<'a> {
        match self {
            T::Unit => U::Unit,
            T::N1(b) => U::N1(*b),
            T::N3(u) => U::N3(*u),
            T::N6(u) => U::N6(*u),
            T::N7(u) => U::N7(*u),
            T::I3(i) => U::I3(*i),
            T::I6(i) => U::I6(*i),
            T::I7(i) => U::I7(*i),
            T::Text(t) => U::Text(t.as_str()),
            T::Binary(v) => U::Binary(v),
            T::Sum(Tag { tag, val }) => U::Sum(
                Tag { tag: tag.as_str(), val: Box::new(val.to_u()) }
            ),
            T::Record(map) => U::Record(
                map.iter().map(|(k, v)| (k.as_str(), v.to_u())).collect()
            ),
            T::List(l) => U::List(
                l.iter().map(|v| v.to_u()).collect::<Vec<U<'a>>>()
            ),
        }
    }

    pub fn encode<'a>(&'a self) -> Vec<u8> {
        match self {
            // TODO: don’t go via U, inefficient
            o => o.to_u().encode()
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum U<'a> {
    Unit,
    // Boolean
    N1(bool),
    // Naturals
    N3(u8),
    N6(u64),
    N7(u128),
    // Integers
    I3(i8),
    I6(i64),
    I7(i128),
    // Text
    Text(&'a str),
    Binary(&'a [u8]),
    // Tags
    Sum(Tag<&'a str, U<'a>>),
    Record(HashMap<&'a str, U<'a>>),
    List(Vec<U<'a>>),
}

impl<'a> U<'a> {
    pub fn encode(&self) -> Vec<u8> {
        let mut c = std::io::Cursor::new(vec![]);
        encode(&mut c, self);
        c.into_inner()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Tag<S, A> {
    // TODO: make into &str
    pub tag: S,
    pub val: Box<A>
}

impl<S, A> Tag<S, A> {
    fn map<F, B>(self, f: F) -> Tag<S, B>
        where F: Fn(A) -> B {
          Tag {
              tag: self.tag,
              val: Box::new(f(*self.val))
          }
    }
}

fn encode_tag<W: Write>(w: &mut W, tag: &str, val: &U) -> std::io::Result<()> {
    write!(w, "<{}:{}|", tag.len(), tag)?;
    encode(w, val)?;
    Ok(())
}

pub fn encode<W: Write>(w: &mut W, u: &U) -> std::io::Result<()> {
  match u {
      U::Unit => write!(w, "u,"),
      U::N1(b) => if *b { write!(w, "n1:1,") } else { write!(w, "n1:0,") },
      U::N3(n) => write!(w, "n3:{},", n),
      U::N6(n) => write!(w, "n6:{},", n),
      U::N7(n) => write!(w, "n7:{},", n),
      U::I3(i) => write!(w, "i3:{},", i),
      U::I6(i) => write!(w, "i6:{},", i),
      U::I7(i) => write!(w, "i7:{},", i),
      U::Text(s) => {
          write!(w, "t{}:", s.len());
          w.write(s.as_bytes());
          write!(w, ",")
      }
      U::Binary(s) => {
          write!(w, "b{}:", s.len());
          w.write(&s);
          write!(w, ",")
      },
      U::Sum(Tag{tag, val}) => encode_tag(w, tag, val),
      U::Record(m) => {
          let mut c = std::io::Cursor::new(vec![]);
          for (k, v) in m {
              encode_tag(&mut c, k, v)?;
          }
          write!(w, "{{{}:", c.get_ref().len())?;
          w.write(c.get_ref())?;
          write!(w, "}}")
      },
      U::List(l) => {
          let mut c = std::io::Cursor::new(vec![]);
          for u in l {
              encode(&mut c, u)?;
          }
          write!(w, "[{}:", c.get_ref().len())?;
          w.write(c.get_ref())?;
          write!(w, "]")
      }
  }
}

pub fn text(s: String) -> T {
    T::Text(s)
}

pub fn t_from_stdin_or_die_user_error(prog_name: &str) -> T {
    let mut buf = vec![];
    std::io::stdin().lock().read_to_end(&mut buf);
    match parse::t_t(&buf) {
        Ok((rest, t)) => match rest {
            b"" => t,
            _ => exec_helpers::die_user_error(prog_name, format!("stdin contained some soup after netencode value: {:?}", rest))
        },
        Err(err) => exec_helpers::die_user_error(prog_name, format!("unable to parse netencode from stdin: {:?}", err))
    }
}

pub mod parse {
    use super::{T, Tag, U};

    use std::str::FromStr;
    use std::ops::Neg;
    use std::collections::HashMap;

    use nom::{IResult};
    use nom::bytes::complete::{tag, take};
    use nom::branch::{alt};
    use nom::character::complete::{digit1, char};
    use nom::sequence::{tuple};
    use nom::combinator::{map, map_res, flat_map, map_parser, opt};
    use nom::error::{context, ErrorKind, ParseError};

    fn unit_t(s: &[u8]) -> IResult<&[u8], ()> {
        let (s, _) = context("unit", tag("u,"))(s)?;
        Ok((s, ()))
    }

    fn usize_t(s: &[u8]) -> IResult<&[u8], usize> {
        context(
            "usize",
            map_res(
                map_res(digit1, |n| std::str::from_utf8(n)),
                |s| s.parse::<usize>())
        )(s)
    }

    fn sized(begin: char, end: char) -> impl Fn(&[u8]) -> IResult<&[u8], &[u8]> {
        move |s: &[u8]| {
            let (s, (_, len, _)) = tuple((
                char(begin),
                usize_t,
                char(':')
            ))(s)?;
            let (s, (res, _)) = tuple((
                take(len),
                char(end)
            ))(s)?;
            Ok((s, res))
        }
    }


    fn uint_t<'a, I: FromStr + 'a>(t: &'static str) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], I> {
        move |s: &'a [u8]| {
            let (s, (_, _, int, _)) = tuple((
                tag(t.as_bytes()),
                char(':'),
                map_res(
                    map_res(digit1, |n: &[u8]| std::str::from_utf8(n)),
                    |s| s.parse::<I>()
                ),
                char(',')
            ))(s)?;
            Ok((s, int))
        }
    }

    fn bool_t<'a>() -> impl Fn(&'a [u8]) -> IResult<&'a [u8], bool> {
        context("bool", alt((
            map(tag("n1:0,"), |_| false),
            map(tag("n1:1,"), |_| true),
        )))
    }

    fn int_t<'a, I: FromStr + Neg<Output=I>>(t: &'static str) -> impl Fn(&'a [u8]) -> IResult<&[u8], I> {
        context(
            t,
            move |s: &'a [u8]| {
                let (s, (_, _, neg, int, _)) = tuple((
                    tag(t.as_bytes()),
                    char(':'),
                    opt(char('-')),
                    map_res(
                        map_res(digit1, |n: &[u8]| std::str::from_utf8(n)),
                        |s| s.parse::<I>()
                    ),
                    char(',')
                ))(s)?;
                let res = match neg {
                    Some(_) => -int,
                    None => int,
                };
                Ok((s, res))
            }
        )
    }

    fn tag_t(s: &[u8]) -> IResult<&[u8], Tag<String, T>> {
        // recurses into the main parser
        map(tag_g(t_t),
            |Tag {tag, val}|
            Tag {
                tag: tag.to_string(),
                val
            })(s)
    }

    fn tag_g<'a, P, O>(inner: P) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Tag<&'a str, O>>
    where
        P: Fn(&'a [u8]) -> IResult<&'a [u8], O>
    {
        move |s: &[u8]| {
            let (s, tag) = sized('<', '|')(s)?;
            let (s, val) = inner(s)?;
            Ok((s, Tag {
                tag: std::str::from_utf8(tag)
                    .map_err(|_| nom::Err::Failure((s, ErrorKind::Char)))?,
                val: Box::new(val)
            }))

        }
    }

    /// parse text scalar (`t5:hello,`)
    fn text(s: &[u8]) -> IResult<&[u8], T> {
        let (s, res) = text_g(s)?;
        Ok((s, T::Text(res.to_string())))
    }

    fn text_g(s: &[u8]) -> IResult<&[u8], &str> {
        let (s, res) = sized('t', ',')(s)?;
        Ok((s,
            std::str::from_utf8(res)
                .map_err(|_| nom::Err::Failure((s, ErrorKind::Char)))?,
        ))
    }

    fn binary<'a>() -> impl Fn(&'a [u8]) -> IResult<&'a [u8], T> {
        map(binary_g(), |b| T::Binary(b.to_owned()))
    }

    fn binary_g() -> impl Fn(&[u8]) -> IResult<&[u8], &[u8]> {
        sized('b', ',')
    }

    fn list_t(s: &[u8]) -> IResult<&[u8], Vec<T>> {
        list_g(t_t)(s)
    }

    fn list_g<'a, P, O>(inner: P) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Vec<O>>
    where
        O: Clone,
        P: Fn(&'a [u8]) -> IResult<&'a [u8], O>
    {
        map_parser(
            sized('[', ']'),
            nom::multi::many0(inner)
        )
    }

    fn record_t<'a>(s: &'a [u8]) -> IResult<&'a [u8], HashMap<String, T>> {
        let (s, r) = record_g(t_t)(s)?;
        Ok((s,
            r.into_iter()
            .map(|(k, v)| (k.to_string(), v))
            .collect::<HashMap<_,_>>()))
    }

    fn record_g<'a, P, O>(inner: P) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], HashMap<&'a str, O>>
    where
        O: Clone,
        P: Fn(&'a [u8]) -> IResult<&'a [u8], O>
    {
        map_parser(
            sized('{', '}'),
            nom::multi::fold_many1(
                tag_g(inner),
                HashMap::new(),
                |mut acc: HashMap<_,_>, Tag { tag, mut val }| {
                    // ignore duplicated tag names that appear later
                    // according to netencode spec
                    if ! acc.contains_key(tag) {
                        acc.insert(tag, *val);
                    }
                    acc
                }
            )
        )
    }

    pub fn u_u(s: &[u8]) -> IResult<&[u8], U> {
        alt((
            map(text_g, U::Text),
            map(binary_g(), U::Binary),
            map(unit_t, |()| U::Unit),
            map(tag_g(u_u), |t| U::Sum(t)),
            map(list_g(u_u), U::List),
            map(record_g(u_u), U::Record),

            map(bool_t(), |u| U::N1(u)),
            map(uint_t("n3"), |u| U::N3(u)),
            map(uint_t("n6"), |u| U::N6(u)),
            map(uint_t("n7"), |u| U::N7(u)),
            map(int_t("i3"), |u| U::I3(u)),
            map(int_t("i6"), |u| U::I6(u)),
            map(int_t("i7"), |u| U::I7(u)),

            // less common
            map(uint_t("n2"), |u| U::N3(u)),
            map(uint_t("n4"), |u| U::N6(u)),
            map(uint_t("n5"), |u| U::N6(u)),
            map(int_t("i1"), |u| U::I3(u)),
            map(int_t("i2"), |u| U::I3(u)),
            map(int_t("i4"), |u| U::I6(u)),
            map(int_t("i5"), |u| U::I6(u)),
            // TODO: 8, 9 not supported
        ))(s)
    }

    pub fn t_t(s: &[u8]) -> IResult<&[u8], T>  {
        alt((
            text,
            binary(),
            map(unit_t, |_| T::Unit),
            map(tag_t, |t| T::Sum(t)),
            map(list_t, |l| T::List(l)),
            map(record_t, |p| T::Record(p)),

            map(bool_t(), |u| T::N1(u)),
            // 8, 64 and 128 bit
            map(uint_t("n3"), |u| T::N3(u)),
            map(uint_t("n6"), |u| T::N6(u)),
            map(uint_t("n7"), |u| T::N7(u)),
            map(int_t("i3"), |u| T::I3(u)),
            map(int_t("i6"), |u| T::I6(u)),
            map(int_t("i7"), |u| T::I7(u)),

            // less common
            map(uint_t("n2"), |u| T::N3(u)),
            map(uint_t("n4"), |u| T::N6(u)),
            map(uint_t("n5"), |u| T::N6(u)),
            map(int_t("i1"), |u| T::I3(u)),
            map(int_t("i2"), |u| T::I3(u)),
            map(int_t("i4"), |u| T::I6(u)),
            map(int_t("i5"), |u| T::I6(u)),
            // TODO: 8, 9 not supported
        ))(s)
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_parse_unit_t() {
            assert_eq!(
                unit_t("u,".as_bytes()),
                Ok(("".as_bytes(), ()))
            );
        }

        #[test]
        fn test_parse_bool_t() {
            assert_eq!(
                bool_t()("n1:0,".as_bytes()),
                Ok(("".as_bytes(), false))
            );
            assert_eq!(
                bool_t()("n1:1,".as_bytes()),
                Ok(("".as_bytes(), true))
            );
        }

        #[test]
        fn test_parse_usize_t() {
            assert_eq!(
                usize_t("32foo".as_bytes()),
                Ok(("foo".as_bytes(), 32))
            );
        }

        #[test]
        fn test_parse_int_t() {
            assert_eq!(
                uint_t::<u8>("n3")("n3:42,abc".as_bytes()),
                Ok(("abc".as_bytes(), 42))
            );
            assert_eq!(
                uint_t::<u8>("n3")("n3:1024,abc".as_bytes()),
                Err(nom::Err::Error(("1024,abc".as_bytes(), nom::error::ErrorKind::MapRes)))
            );
            assert_eq!(
                int_t::<i64>("i6")("i6:-23,abc".as_bytes()),
                Ok(("abc".as_bytes(), -23))
            );
            assert_eq!(
                int_t::<i128>("i3")("i3:0,:abc".as_bytes()),
                Ok((":abc".as_bytes(), 0))
            );
            assert_eq!(
                uint_t::<u8>("n7")("n7:09,".as_bytes()),
                Ok(("".as_bytes(), 9))
            );
            // assert_eq!(
            //     length("c"),
            //     Err(nom::Err::Error(("c", nom::error::ErrorKind::Digit)))
            // );
            // assert_eq!(
            //     length(":"),
            //     Err(nom::Err::Error((":", nom::error::ErrorKind::Digit)))
            // );
        }

        #[test]
        fn test_parse_text() {
            assert_eq!(
                text("t5:hello,".as_bytes()),
                Ok(("".as_bytes(), T::Text("hello".to_owned())))
            );
            assert_eq!(
                text("t4:fo,".as_bytes()),
                // TODO: way better parse error messages
                Err(nom::Err::Error(("fo,".as_bytes(), nom::error::ErrorKind::Eof)))
            );
            assert_eq!(
                text("t9:今日は,".as_bytes()),
                Ok(("".as_bytes(), T::Text("今日は".to_owned())))
            );
        }

        #[test]
        fn test_parse_binary() {
            assert_eq!(
                binary()("b5:hello,".as_bytes()),
                Ok(("".as_bytes(), T::Binary(Vec::from("hello".to_owned()))))
            );
            assert_eq!(
                binary()("b4:fo,".as_bytes()),
                // TODO: way better parse error messages
                Err(nom::Err::Error(("fo,".as_bytes(), nom::error::ErrorKind::Eof)))
            );
            assert_eq!(
                binary()("b9:今日は,".as_bytes()),
                Ok(("".as_bytes(), T::Binary(Vec::from("今日は".as_bytes()))))
            );
        }

        #[test]
        fn test_list() {
            assert_eq!(
                list_t("[0:]".as_bytes()),
                Ok(("".as_bytes(), vec![]))
            );
            assert_eq!(
                list_t("[6:u,u,u,]".as_bytes()),
                Ok(("".as_bytes(), vec![
                    T::Unit,
                    T::Unit,
                    T::Unit,
                ]))
            );
            assert_eq!(
                list_t("[15:u,[7:t3:foo,]u,]".as_bytes()),
                Ok(("".as_bytes(), vec![
                    T::Unit,
                    T::List(vec![T::Text("foo".to_owned())]),
                    T::Unit,
                ]))
            );
        }

        #[test]
        fn test_record() {
            assert_eq!(
                record_t("{21:<1:a|u,<1:b|u,<1:c|u,}".as_bytes()),
                Ok(("".as_bytes(), vec![
                    ("a".to_owned(), T::Unit),
                    ("b".to_owned(), T::Unit),
                    ("c".to_owned(), T::Unit),
                ].into_iter().collect::<HashMap<String, T>>()))
            );
            // duplicated keys are ignored (first is taken)
            assert_eq!(
                record_t("{25:<1:a|u,<1:b|u,<1:a|i1:-1,}".as_bytes()),
                Ok(("".as_bytes(), vec![
                    ("a".to_owned(), T::Unit),
                    ("b".to_owned(), T::Unit),
                ].into_iter().collect::<HashMap<_,_>>()))
            );
        }

        #[test]
        fn test_parse() {
            assert_eq!(
                t_t("n3:255,".as_bytes()),
                Ok(("".as_bytes(), T::N3(255)))
            );
            assert_eq!(
                t_t("t6:halloo,".as_bytes()),
                Ok(("".as_bytes(), T::Text("halloo".to_owned())))
            );
            assert_eq!(
                t_t("<3:foo|t6:halloo,".as_bytes()),
                Ok(("".as_bytes(), T::Sum (Tag {
                    tag: "foo".to_owned(),
                    val: Box::new(T::Text("halloo".to_owned()))
                })))
            );
            // { a: Unit
            // , foo: List <A: Unit | B: List i3> }
            assert_eq!(
                t_t("{52:<1:a|u,<3:foo|[33:<1:A|u,<1:A|n1:1,<1:B|[7:i3:127,]]}".as_bytes()),
                Ok(("".as_bytes(), T::Record(vec![
                    ("a".to_owned(), T::Unit),
                    ("foo".to_owned(), T::List(vec![
                        T::Sum(Tag { tag: "A".to_owned(), val: Box::new(T::Unit) }),
                        T::Sum(Tag { tag: "A".to_owned(), val: Box::new(T::N1(true)) }),
                        T::Sum(Tag { tag: "B".to_owned(), val: Box::new(T::List(vec![T::I3(127)])) }),
                    ]))
                ].into_iter().collect::<HashMap<String, T>>())))
            );
        }

    }
}

pub mod dec {
    use super::*;
    use std::collections::HashMap;

    pub struct DecodeError(pub String);

    pub trait Decoder {
        type A;
        fn dec(T) -> Result<Self::A, DecodeError>;
    }

    pub struct ScalarAsBytes;

    impl Decoder for ScalarAsBytes {
        type A = Vec<u8>;
        fn dec(t: T) -> Result<Self::A, DecodeError> {
            match t {
                T::N3(u) => Ok(format!("{}", u).into_bytes()),
                T::N6(u) => Ok(format!("{}", u).into_bytes()),
                T::N7(u) => Ok(format!("{}", u).into_bytes()),
                T::I3(i) => Ok(format!("{}", i).into_bytes()),
                T::I6(i) => Ok(format!("{}", i).into_bytes()),
                T::I7(i) => Ok(format!("{}", i).into_bytes()),
                T::Text(t) => Ok(t.into_bytes()),
                T::Binary(b) => Ok(b),
                o => Err(DecodeError(format!("Cannot decode {:?} into scalar", o))),
            }
        }
    }

    pub struct Record<T>(pub T);

    impl<Inner: Decoder> Decoder for Record<Inner> {
        type A = HashMap<String, Inner::A>;
        fn dec(t: T) -> Result<Self::A, DecodeError> {
            match t {
                T::Record(map) =>
                    map.into_iter()
                    .map(|(k, v)| Inner::dec(v).map(|v2| (k, v2)))
                    .collect::<Result<Self::A, _>>(),
                o => Err(DecodeError(format!("Cannot decode {:?} into record", o)))
            }
        }
    }
}
