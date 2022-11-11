use crate::{value::NixString, ErrorKind, Value};

#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum Suspension {
    Fetchurl(NixString),
    FetchTarball {
        url: NixString,
        sha256: Option<NixString>,
    },
}

pub trait SuspensionHandler: Sync + Send {
    fn handle_suspension(&self, suspension: Suspension) -> Result<Value, ErrorKind>;
}

struct NoSuspensionsAllowed;

impl SuspensionHandler for NoSuspensionsAllowed {
    fn handle_suspension(&self, _suspension: Suspension) -> Result<Value, ErrorKind> {
        Err(ErrorKind::NoSuspensionsAllowed)
    }
}
