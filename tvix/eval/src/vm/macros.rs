#[macro_export]
macro_rules! arithmetic_op {
    ( $self:ident, $op:tt ) => {{ // TODO: remove
        let b = $self.pop();
        let a = $self.pop();
        let result = fallible!($self, arithmetic_op!(&a, &b, $op));
        $self.push(result);
    }};

    ( $a:expr, $b:expr, $op:tt ) => {{
        match ($a, $b) {
            (Value::Integer(i1), Value::Integer(i2)) => Ok(Value::Integer(i1 $op i2)),
            (Value::Float(f1), Value::Float(f2)) => Ok(Value::Float(f1 $op f2)),
            (Value::Integer(i1), Value::Float(f2)) => Ok(Value::Float(*i1 as f64 $op f2)),
            (Value::Float(f1), Value::Integer(i2)) => Ok(Value::Float(f1 $op *i2 as f64)),

            (v1, v2) => Err(ErrorKind::TypeError {
                expected: "number (either int or float)",
                actual: if v1.is_number() {
                    v2.type_of()
                } else {
                    v1.type_of()
                },
            }),
        }
    }};
}

#[macro_export]
macro_rules! cmp_op {
    ( $vm:ident, $frame:ident, $span:ident, $op:tt ) => {{
        let b = $vm.stack_pop();
        let a = $vm.stack_pop();

        async fn compare(a: Value, b: Value, co: GenCo) -> Result<Value, ErrorKind> {
            let a = generators::request_force(&co, a).await;
            let b = generators::request_force(&co, b).await;
            let ordering = a.nix_cmp_ordering(b, co).await?;
            Ok(Value::Bool(cmp_op!(@order $op ordering)))
        }

        let gen_frame = Frame::Generator {
            span: $frame.current_light_span(),
            state: GeneratorState::Running,
            generator: Gen::new(|co| pin_generator(compare(a, b, co))),
        };

        $vm.push_call_frame($span, $frame);
        $vm.frames.push(gen_frame);
        return Ok(());
    }};

    (@order < $ordering:expr) => {
        $ordering == Some(Ordering::Less)
    };

    (@order > $ordering:expr) => {
        $ordering == Some(Ordering::Greater)
    };

    (@order <= $ordering:expr) => {
        !matches!($ordering, None | Some(Ordering::Greater))
    };

    (@order >= $ordering:expr) => {
        !matches!($ordering, None | Some(Ordering::Less))
    };
}

/// This macro wraps a computation that returns an ErrorKind or a
/// result, and wraps the ErrorKind in an Error struct if present.
///
/// The reason for this macro's existence is that calculating spans is
/// potentially expensive, so it should be avoided to the last moment
/// (i.e. definite instantiation of a runtime error) if possible.
// TODO: remove
#[macro_export]
macro_rules! fallible {
    ( $self:ident, $body:expr) => {
        match $body {
            Ok(result) => result,
            Err(kind) => return Err(Error::new(kind, $self.current_span())),
        }
    };
}
