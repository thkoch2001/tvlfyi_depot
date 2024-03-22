use std::io;

use bstr::ByteSlice;
use data_encoding::HEXLOWER;
use md5::Md5;
use sha1::Sha1;
use sha2::{digest::Output, Digest, Sha256, Sha512};

use crate::ErrorKind;

fn hash<D: Digest + io::Write>(mut r: impl io::Read) -> Result<Output<D>, ErrorKind> {
    let mut hasher = D::new();
    io::copy(&mut r, &mut hasher)?;
    Ok(hasher.finalize())
}

pub fn hash_nix(algo: impl AsRef<[u8]>, r: impl io::Read) -> Result<String, ErrorKind> {
    match algo.as_ref() {
        b"md5" => Ok(HEXLOWER.encode(hash::<Md5>(r)?.as_bstr())),
        b"sha1" => Ok(HEXLOWER.encode(hash::<Sha1>(r)?.as_bstr())),
        b"sha256" => Ok(HEXLOWER.encode(hash::<Sha256>(r)?.as_bstr())),
        b"sha512" => Ok(HEXLOWER.encode(hash::<Sha512>(r)?.as_bstr())),
        _ => Err(ErrorKind::UnknownHashType(
            algo.as_ref().as_bstr().to_string(),
        )),
    }
}
