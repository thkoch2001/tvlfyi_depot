//! Support for configurable generation of arbitrary nix values

use proptest::{prelude::*, strategy::BoxedStrategy};
use std::{ffi::OsString, rc::Rc};

use super::{NixAttrs, NixString, Value};

pub struct Parameters {
    pub generate_internal_values: bool,
}

impl Default for Parameters {
    fn default() -> Self {
        Self {
            generate_internal_values: false,
        }
    }
}

impl Arbitrary for Value {
    type Parameters = Parameters;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        if args.generate_internal_values {
            todo!("Generating internal values not implemented yet")
        } else {
            non_internal_value().boxed()
        }
    }
}

fn non_internal_value() -> impl Strategy<Value = Value> {
    use Value::*;

    prop_oneof![
        Just(Null),
        any::<bool>().prop_map(Bool),
        any::<i64>().prop_map(Integer),
        any::<f64>().prop_map(Float),
        any::<NixString>().prop_map(String),
        any::<OsString>().prop_map(|s| Path(s.into())),
        any::<NixAttrs>().prop_map(|a| Value::Attrs(Rc::new(a)))
    ]
}
