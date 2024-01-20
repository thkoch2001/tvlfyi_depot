//! This module implements the position tracking mechanisms exposed
//! in the Nix language.
//!
//! Those builtins are special because, like `import`, they need an access
//! to the current source code (more specifically, a complete codemap).

use crate::{self as tvix_eval, SourceCode, Value};
use builtin_macros::builtins;

#[builtins(state = "SourceCode")]
mod tracking_builtins {
    use super::*;
    use crate::generators::{self, GenCo};
    use crate::value::Value;
    use crate::{ErrorKind, NixAttrs, SourceCode, WarningKind};
    use genawaiter::rc::Gen;

    #[builtin("unsafeGetAttrPos")]
    async fn builtin_unsafe_get_attr_pos(
        state: SourceCode,
        co: GenCo,
        _name: Value,
        _attrset: Value,
    ) -> Result<Value, ErrorKind> {
        generators::emit_warning_kind(
            &co,
            WarningKind::NotImplemented("builtins.unsafeGetAttrsPos"),
        )
        .await;
        // TODO: find the span of `$attrset.$name`.
        // FUTUREWORK: Is this always a sufficient span? This is best effort anyway…
        let span = generators::request_span(&co).await;
        let span_loc = state.codemap().look_up_span(span.span());
        let res = [
            ("line", Value::Integer(span_loc.begin.line as i64)),
            ("column", Value::Integer(span_loc.begin.column as i64)),
            ("file", Value::String(span_loc.file.name().into())),
        ];
        Ok(Value::attrs(NixAttrs::from_iter(res.into_iter())))
    }
}

pub fn tracking_builtins(source_code: SourceCode) -> Vec<(&'static str, Value)> {
    tracking_builtins::builtins(source_code)
}
