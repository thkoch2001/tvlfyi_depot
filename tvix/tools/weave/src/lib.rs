use anyhow::Result;
use rayon::prelude::*;
use std::{fs::File, slice};

use polars::{
    datatypes::BinaryChunked,
    export::arrow::array::BinaryArray,
    prelude::{ParquetReader, SerReader},
};

pub use crate::bytes::*;
mod bytes;

pub const INDEX_NULL: u32 = !0;
pub const DONE: &str = "\u{2714}";

pub fn hash64(h: &[u8; 20]) -> u64 {
    let mut buf = [0; 8];
    buf.copy_from_slice(&h[..8]);
    u64::from_le_bytes(buf)
}

pub fn load_ph_array() -> Result<FixedBytes<20>> {
    eprint!("… load store_path_hash\r");
    let ph_array = into_fixed_binary_rechunk::<20>(
        ParquetReader::new(File::open("narinfo.parquet").unwrap())
            .with_columns(Some(vec!["store_path_hash".into()]))
            .set_rechunk(true)
            .finish()?
            .column("store_path_hash")?
            .binary()?,
    );

    u32::try_from(ph_array.len()).expect("dataset exceeds 2^32");
    eprintln!("{DONE}");

    Ok(ph_array)
}

pub fn as_fixed_binary<const N: usize>(
    chunked: &BinaryChunked,
) -> impl Iterator<Item = &[[u8; N]]> {
    chunked.downcast_iter().map(|array| {
        assert_fixed_dense::<N>(array);
        exact_chunks(array.values()).unwrap()
    })
}

fn into_fixed_binary_rechunk<const N: usize>(chunked: &BinaryChunked) -> FixedBytes<N> {
    let chunked = chunked.rechunk();
    let mut iter = chunked.downcast_iter();
    let array = iter.next().unwrap();

    assert_fixed_dense::<N>(array);
    Bytes(array.values().clone()).map(|buf| exact_chunks(buf).unwrap())
}

fn assert_fixed_dense<const N: usize>(array: &BinaryArray<i64>) {
    if array.validity().is_some() {
        panic!("null values may be present");
    }

    let length_check = array
        .offsets()
        .as_slice()
        .par_windows(2)
        .all(|w| (w[1] - w[0]) == N as i64);

    if !length_check {
        panic!("lengths are inconsistent");
    }
}

fn exact_chunks<const K: usize>(buf: &[u8]) -> Option<&[[u8; K]]> {
    // SAFETY: We ensure that `buf.len()` is a multiple of K, and there are no alignment requirements.
    unsafe {
        let ptr = buf.as_ptr();
        let len = buf.len();

        if len % K != 0 {
            return None;
        }

        let ptr = ptr as *mut [u8; K];
        let len = len / K;

        Some(slice::from_raw_parts(ptr, len))
    }
}
