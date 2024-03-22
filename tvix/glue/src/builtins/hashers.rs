//! Contains builtins that perform hashing.

use crate::tvix_store_io::TvixStoreIO;
use nix_compat::nixhash::HashAlgo;
use std::rc::Rc;
use tvix_eval::builtin_macros::builtins;
use tvix_eval::builtins::coerce_value_to_path;
use tvix_eval::generators::{self, GenCo};
use tvix_eval::{ErrorKind, Value};

#[allow(unused_variables)] // for the `state` arg, for now
#[builtins(state = "Rc<TvixStoreIO>")]
pub(crate) mod hasher_builtins {
    use super::*;

    use tvix_eval::generators::Gen;

    #[builtin("hashString")]
    async fn builtin_hash_string(co: GenCo, algo: Value, s: Value) -> Result<Value, ErrorKind> {
        let algo = algo.to_str()?;
        let algo = HashAlgo::try_from(algo.as_bytes())
            .map_err(|_| ErrorKind::UnknownHashType(algo.into()))?;
        Ok(algo
            .hash_bytes(std::io::Cursor::new(s.to_str()?))
            .map(|hash| hash.to_plain_hex_string())
            .map(Value::from)?)
    }

    #[builtin("hashFile")]
    #[allow(non_snake_case)]
    async fn builtin_hashFile(co: GenCo, algo: Value, path: Value) -> Result<Value, ErrorKind> {
        let path = match coerce_value_to_path(&co, path).await? {
            Err(cek) => return Ok(Value::from(cek)),
            Ok(p) => p,
        };
        let r = generators::request_open_file(&co, path).await;
        let algo = algo.to_str()?;
        let algo = HashAlgo::try_from(algo.as_bytes())
            .map_err(|_| ErrorKind::UnknownHashType(algo.into()))?;
        Ok(algo
            .hash_bytes(r)
            .map(|hash| hash.to_plain_hex_string())
            .map(Value::from)?)
    }
}
