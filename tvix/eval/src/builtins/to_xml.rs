//! This module implements `builtins.toXML`, which is a serialisation
//! of value information as well as internal tvix state that several
//! things in nixpkgs rely on.

use std::{io::Write, rc::Rc};
use xml::common::XmlVersion;
use xml::writer::events::XmlEvent;
use xml::writer::EventWriter;

use crate::{ErrorKind, Value};

/// Recursively serialise a value to XML. The value *must* have been
/// deep-forced before being passed to this function.
pub(super) fn value_to_xml<W: Write>(writer: W, value: &Value) -> Result<(), ErrorKind> {
    let mut writer = EventWriter::new(writer);

    writer.write(XmlEvent::StartDocument {
        version: XmlVersion::Version10,
        encoding: Some("utf-8"),
        standalone: None,
    })?;

    writer.write(XmlEvent::start_element("expr"))?;
    value_variant_to_xml(&mut writer, value)?;
    writer.write(XmlEvent::end_element())?;

    Ok(())
}

fn write_typed_value<W: Write, V: ToString>(
    w: &mut EventWriter<W>,
    name: &str,
    value: V,
) -> Result<(), ErrorKind> {
    w.write(XmlEvent::start_element(name).attr("value", &value.to_string()))?;
    w.write(XmlEvent::end_element())?;
    Ok(())
}

fn value_variant_to_xml<W: Write>(w: &mut EventWriter<W>, value: &Value) -> Result<(), ErrorKind> {
    match value {
        Value::Thunk(t) => return value_variant_to_xml(w, &t.value()),

        Value::Null => {
            w.write(XmlEvent::start_element("null"))?;
            w.write(XmlEvent::end_element())
        }

        Value::Bool(b) => return write_typed_value(w, "bool", b),
        Value::Integer(i) => return write_typed_value(w, "int", i),
        Value::Float(f) => return write_typed_value(w, "float", f),
        Value::String(s) => return write_typed_value(w, "string", s.as_str()),
        Value::Path(p) => return write_typed_value(w, "path", p.to_string_lossy()),

        Value::List(list) => {
            w.write(XmlEvent::start_element("list"))?;

            for elem in list.into_iter() {
                value_variant_to_xml(w, &elem)?;
            }

            w.write(XmlEvent::end_element())
        }

        Value::Attrs(attrs) => {
            w.write(XmlEvent::start_element("attrs"))?;

            for elem in attrs.iter() {
                w.write(XmlEvent::start_element("attr").attr("name", elem.0.as_str()))?;
                value_variant_to_xml(w, &elem.1)?;
                w.write(XmlEvent::end_element())?;
            }

            w.write(XmlEvent::end_element())
        }

        Value::Closure(_) => todo!(),

        Value::Builtin(_) => {
            w.write(XmlEvent::start_element("unevaluated"))?;
            w.write(XmlEvent::end_element())
        }

        Value::AttrNotFound
        | Value::Blueprint(_)
        | Value::DeferredUpvalue(_)
        | Value::UnresolvedPath(_) => {
            return Err(ErrorKind::TvixBug {
                msg: "internal value variant encountered in builtins.toXML",
                metadata: Some(Rc::new(value.clone())),
            })
        }
    }?;

    Ok(())
}
