//! This module implements `builtins.toXML`, which is a serialisation
//! of value information as well as internal tvix state that several
//! things in nixpkgs rely on.

use bstr::ByteSlice;
use std::borrow::{Borrow, Cow};
use std::collections::VecDeque;
use std::{io::Write, rc::Rc};

use crate::{ErrorKind, Value};

/// Recursively serialise a value to XML. The value *must* have been
/// deep-forced before being passed to this function.
pub fn value_to_xml<W: Write>(mut writer: W, value: Rc<Value>) -> Result<(), ErrorKind> {
    // Write a literal document declaration, using C++-Nix-style
    // single quotes.
    writeln!(writer, "<?xml version='1.0' encoding='utf-8'?>")?;

    let mut emitter = XmlEmitter::new(writer);

    emitter.write_open_tag("expr", &[])?;
    match value_variant_to_xml::<W>(value) {
        ToEmit::One(emit) => (emit.0)(&mut emitter)?,

        ToEmit::More(more) => {
            let mut vals: Vec<Rc<Value>> = Vec::new();
            let mut fmt_queue: VecDeque<EmitValue<W>> = VecDeque::from(more);
            loop {
                match fmt_queue.pop_front() {
                    None => break,
                    Some(EmitValue::Emit(emit)) => (emit.0)(&mut emitter)?,
                    Some(EmitValue::FormatValue(value)) => {
                        vals.push(value.clone());
                        match value_variant_to_xml(value) {
                            ToEmit::One(emit) => (emit.0)(&mut emitter)?,
                            ToEmit::More(more) => {
                                for el in more.into_iter().rev() {
                                    fmt_queue.push_front(el);
                                }
                            }
                        };
                    }
                }
            }
        }
    }

    emitter.write_closing_tag("expr")?;

    Ok(())
}

fn write_typed_value<'a, W: Write>(name_unescaped: &'a str, value: String) -> Emit<'a, W> {
    Emit(Box::new(move |w| {
        w.write_self_closing_tag(name_unescaped, &[("value", &value)])?;
        Ok(())
    }))
}

enum EmitValue<'a, W> {
    FormatValue(Rc<Value>),
    Emit(Emit<'a, W>),
}

struct Emit<'a, W>(Box<dyn FnOnce(&mut XmlEmitter<W>) -> Result<(), ErrorKind> + 'a>);

enum ToEmit<'a, W> {
    One(Emit<'a, W>),
    More(Vec<EmitValue<'a, W>>),
}

fn value_variant_to_xml<'a, W: Write>(value: Rc<Value>) -> ToEmit<'a, W> {
    match value.borrow() {
        Value::Thunk(t) => ToEmit::More(vec![EmitValue::FormatValue(Rc::new(
            t.clone().unwrap_or_clone(),
        ))]),

        Value::Null => ToEmit::One(Emit(Box::new(|w: &mut XmlEmitter<W>| {
            w.write_open_tag("null", &[])?;
            w.write_closing_tag("null")?;
            Ok(())
        }))),

        Value::Bool(b) => ToEmit::One(write_typed_value("bool", b.to_string())),
        Value::Integer(i) => ToEmit::One(write_typed_value("int", i.to_string())),
        Value::Float(f) => ToEmit::One(write_typed_value("float", f.to_string())),
        Value::String(s) => ToEmit::One(write_typed_value("string", s.as_bstr().to_string())),
        Value::Path(p) => {
            let s = p.to_string_lossy().into_owned();
            ToEmit::One(Emit(Box::new(|w| (write_typed_value("path", s).0)(w))))
        }

        Value::List(list) => {
            let mut v = Vec::new();
            v.push(EmitValue::Emit(Emit(Box::new(|w| {
                w.write_open_tag("list", &[])?;
                Ok(())
            }))));
            v.extend(
                list.into_iter()
                    .map(|v| EmitValue::FormatValue(Rc::new(v.clone())))
                    .collect::<Vec<_>>(),
            );
            v.push(EmitValue::Emit(Emit(Box::new(|w| {
                w.write_closing_tag("list")?;
                Ok(())
            }))));
            ToEmit::More(v)
        }

        Value::Attrs(attrs) => {
            let mut v = Vec::new();
            v.push(EmitValue::Emit(Emit(Box::new(|w| {
                w.write_open_tag("attrs", &[])?;
                Ok(())
            }))));

            v.extend(attrs.iter().flat_map(|elem| {
                let name = elem.0.to_str_lossy().into_owned();
                let val = elem.1.clone();
                vec![
                    EmitValue::Emit(Emit(Box::new(move |w| {
                        w.write_open_tag("attr", &[("name", &name)])?;
                        Ok(())
                    }))),
                    EmitValue::FormatValue(Rc::new(val)),
                    EmitValue::Emit(Emit(Box::new(move |w| {
                        w.write_closing_tag("attr")?;
                        Ok(())
                    }))),
                ]
            }));

            v.push(EmitValue::Emit(Emit(Box::new(|w| {
                w.write_closing_tag("attrs")?;
                Ok(())
            }))));

            ToEmit::More(v)
        },
        Value::Closure(c) =>{
            let formals = c.lambda.formals.clone();
            ToEmit::One(Emit(Box::new(move |w| {
                w.write_open_tag("function", &[])?;

                match formals {
                    Some(formals) => {
                        let mut attrs: Vec<(&str, &str)> = Vec::with_capacity(2);
                        if formals.ellipsis {
                            attrs.push(("ellipsis", "1"));
                        }
                        if let Some(ref name) = &formals.name {
                            attrs.push(("name", name.as_str()));
                        }

                        w.write_open_tag("attrspat", &attrs)?;
                        for arg in formals.arguments.iter() {
                            w.write_self_closing_tag("attr", &[("name", &arg.0.to_str_lossy().into_owned())])?;
                        }

                        w.write_closing_tag("attrspat")?;
                    }
                    None => {
                        // TODO(tazjin): tvix does not currently persist function
                        // argument names anywhere (whereas we do for formals, as
                        // that is required for other runtime behaviour). Because of
                        // this the implementation here is fake, always returning
                        // the same argument name.
                        //
                        // If we don't want to persist the data, we can re-parse the
                        // AST from the spans of the lambda's bytecode and figure it
                        // out that way, but it needs some investigating.
                        w.write_self_closing_tag("varpat", &[("name", /* fake: */ "x")])?;
                    }
                }

                w.write_closing_tag("function")?;
                Ok(())
            })))},
        Value::Builtin(_) => ToEmit::One(Emit(Box::new(|w| {
            w.write_open_tag("unevaluated", &[])?;
            w.write_closing_tag("unevaluated")?;
            Ok(())
        }))),

        Value::AttrNotFound
        | Value::Blueprint(_)
        | Value::DeferredUpvalue(_)
        | Value::UnresolvedPath(_)
        | Value::Json(..)
        | Value::FinaliseRequest(_) => ToEmit::One(Emit(Box::new(move |_| {
            Err(ErrorKind::TvixBug {
                msg: "internal value variant encountered in builtins.toXML",
                metadata: Some(value.clone()),
            })
        }))),

        Value::Catchable(_) => {
            panic!("tvix bug: value_to_xml() called on a value which had not been deep-forced")
        }
    }
}

/// A simple-stupid XML emitter, which implements only the subset needed for byte-by-byte compat with C++ nix’ `builtins.toXML`.
struct XmlEmitter<W> {
    /// The current indentation
    cur_indent: usize,
    writer: W,
}

impl<W: Write> XmlEmitter<W> {
    pub fn new(writer: W) -> Self {
        XmlEmitter {
            cur_indent: 0,
            writer,
        }
    }

    /// Write an open tag with the given name (which is not escaped!)
    /// and attributes (Keys are not escaped! Only attribute values are.)
    pub fn write_open_tag(
        &mut self,
        name_unescaped: &str,
        attrs: &[(&str, &str)],
    ) -> std::io::Result<()> {
        self.add_indent()?;
        self.writer.write_all(b"<")?;
        self.writer.write_all(name_unescaped.as_bytes())?;
        self.write_attrs_escape_vals(attrs)?;
        self.writer.write_all(b">\n")?;
        self.cur_indent += 2;
        Ok(())
    }

    /// Write a self-closing open tag with the given name (which is not escaped!)
    /// and attributes (Keys are not escaped! Only attribute values are.)
    pub fn write_self_closing_tag(
        &mut self,
        name_unescaped: &str,
        attrs: &[(&str, &str)],
    ) -> std::io::Result<()> {
        self.add_indent()?;
        self.writer.write_all(b"<")?;
        self.writer.write_all(name_unescaped.as_bytes())?;
        self.write_attrs_escape_vals(attrs)?;
        self.writer.write_all(b" />\n")?;
        Ok(())
    }

    /// Write a closing tag with the given name (which is not escaped!)
    pub fn write_closing_tag(&mut self, name_unescaped: &str) -> std::io::Result<()> {
        self.cur_indent -= 2;
        self.add_indent()?;
        self.writer.write_all(b"</")?;
        self.writer.write_all(name_unescaped.as_bytes())?;
        self.writer.write_all(b">\n")?;
        Ok(())
    }

    #[inline]
    fn add_indent(&mut self) -> std::io::Result<()> {
        self.writer.write_all(&b" ".repeat(self.cur_indent))
    }

    /// Write an attribute list
    fn write_attrs_escape_vals(&mut self, attrs: &[(&str, &str)]) -> std::io::Result<()> {
        for (name, val) in attrs {
            self.writer.write_all(b" ")?;
            self.writer.write_all(name.as_bytes())?;
            self.writer.write_all(br#"=""#)?;
            self.writer
                .write_all(Self::escape_attr_value(val).as_bytes())?;
            self.writer.write_all(b"\"")?;
        }
        Ok(())
    }

    /// Escape the given attribute value, making sure we only actually clone the string if we needed to replace something.
    fn escape_attr_value(s: &str) -> Cow<str> {
        let mut last_escape: usize = 0;
        let mut res: Cow<str> = Cow::Borrowed("");
        // iterating via char_indices gives us the ability to index the original string slice at character boundaries
        for (idx, c) in s.char_indices() {
            match Self::should_escape_char(c) {
                None => {}
                Some(new) => {
                    // add characters since the last escape we did
                    res += &s[last_escape..idx];
                    // add the escaped value
                    res += new;
                    last_escape = idx + 1;
                }
            }
        }
        // we did not need to escape anything, so borrow original string
        if last_escape == 0 {
            Cow::Borrowed(s)
        } else {
            // add the remaining characters
            res += &s[last_escape..];
            res
        }
    }

    fn should_escape_char(c: char) -> Option<&'static str> {
        match c {
            '<' => Some("&lt;"),
            '>' => Some("&gt;"),
            '"' => Some("&quot;"),
            '\'' => Some("&apos;"),
            '&' => Some("&amp;"),
            '\n' => Some("&#xA;"),
            '\r' => Some("&#xD;"),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use bytes::buf::Writer;
    use pretty_assertions::assert_eq;

    use crate::builtins::to_xml::XmlEmitter;
    use std::borrow::Cow;

    #[test]
    fn xml_gen() {
        let mut buf = Vec::new();
        let mut x = XmlEmitter::new(&mut buf);
        x.write_open_tag("hello", &[("hi", "it’s me"), ("no", "<escape>")])
            .unwrap();
        x.write_self_closing_tag("self-closing", &[("tag", "yay")])
            .unwrap();
        x.write_closing_tag("hello").unwrap();

        assert_eq!(
            std::str::from_utf8(&buf).unwrap(),
            r##"<hello hi="it’s me" no="&lt;escape&gt;">
  <self-closing tag="yay" />
</hello>
"##
        );
    }

    #[test]
    fn xml_escape() {
        match XmlEmitter::<Writer<Vec<u8>>>::escape_attr_value("ab<>c&de") {
            Cow::Owned(s) => assert_eq!(s, "ab&lt;&gt;c&amp;de".to_string(), "escape stuff"),
            Cow::Borrowed(s) => panic!("s should be owned {}", s),
        }
        match XmlEmitter::<Writer<Vec<u8>>>::escape_attr_value("") {
            Cow::Borrowed(s) => assert_eq!(s, "", "empty escape is borrowed"),
            Cow::Owned(s) => panic!("s should be borrowed {}", s),
        }
        match XmlEmitter::<Writer<Vec<u8>>>::escape_attr_value("hi!ŷbla") {
            Cow::Borrowed(s) => assert_eq!(s, "hi!ŷbla", "no escape is borrowed"),
            Cow::Owned(s) => panic!("s should be borrowed {}", s),
        }
        match XmlEmitter::<Writer<Vec<u8>>>::escape_attr_value("hi!<ŷ>bla") {
            Cow::Owned(s) => assert_eq!(
                s,
                "hi!&lt;ŷ&gt;bla".to_string(),
                "multi-byte chars are correctly used"
            ),
            Cow::Borrowed(s) => panic!("s should be owned {}", s),
        }
    }
}
