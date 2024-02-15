use anyhow::Result;
use hashbrown::{hash_table, HashTable};
use nix_compat::nixbase32;
use rayon::prelude::*;
use std::{
    collections::{BTreeMap, HashSet},
    fs::{self, File},
    ops::Index,
    slice,
    sync::atomic::{AtomicU32, Ordering},
};

use polars::{
    datatypes::StaticArray,
    export::arrow::{array::UInt32Array, offset::OffsetsBuffer},
    prelude::*,
};

const DONE: &str = "\u{2714}";

fn main() -> Result<()> {
    eprint!("… parse roots\r");
    let roots: PathSet32 = {
        let mut roots = Vec::new();
        fs::read("nixpkgs.roots")?
            .par_chunks_exact(32 + 1)
            .map(|e| nixbase32::decode_fixed::<20>(&e[0..32]).unwrap())
            .collect_into_vec(&mut roots);

        roots.iter().collect()
    };
    eprintln!("{DONE}");

    let narinfo_pq = LazyFrame::scan_parquet("narinfo.parquet", ScanArgsParquet::default())?;

    {
        eprint!("… load store_path_hash\r");
        let ph_array = narinfo_pq
            .clone()
            .select([col("store_path_hash")])
            .collect()?
            .column("store_path_hash")?
            .binary()?
            .rechunk();
        let ph_array = fixed_binary::<20>(&ph_array).next().unwrap();
        u32::try_from(ph_array.len()).expect("dataset exceeds 2^32");
        eprintln!("{DONE}");

        eprint!("… resolve roots\r");
        ph_array.par_iter().enumerate().for_each(|(idx, h)| {
            if let Some(idx_slot) = roots.find(h) {
                idx_slot
                    .compare_exchange(!0, idx as u32, Ordering::SeqCst, Ordering::SeqCst)
                    .expect("duplicate entry");
            }
        });
        eprintln!("{DONE}");
    }

    // TODO(edef): build a hashbrown::HashTable with prefix hashing
    let mut todo = HashSet::with_capacity(roots.len());
    {
        let mut unknown_roots = 0usize;
        for (_, idx) in roots.table {
            let idx = idx.into_inner();
            if idx == !0 {
                unknown_roots += 1;
                continue;
            }
            todo.insert(idx);
        }
        println!("skipping {unknown_roots} unknown roots");
    }

    eprint!("… load reference_idxs\r");
    let ri_array = ParquetReader::new(File::open("narinfo-references.parquet")?)
        .finish()?
        .column("reference_idxs")?
        .list()?
        .clone();

    let ri_array = {
        let chunks = ri_array.downcast_chunks();
        ChunkedList::new((0..chunks.len()).map(|index| {
            let chunk = chunks.get(index).unwrap();

            (
                chunk.offsets(),
                chunk
                    .values()
                    .as_any()
                    .downcast_ref::<UInt32Array>()
                    .unwrap()
                    .as_slice()
                    .unwrap(),
            )
        }))
    };
    eprintln!("{DONE}");

    let mut seen = todo.clone();
    while !todo.is_empty() {
        println!("todo: {} seen: {}", todo.len(), seen.len());

        todo = todo
            .par_iter()
            .flat_map(|&parent| {
                if parent == !0 {
                    return Default::default();
                }

                ri_array[parent as usize]
                    .iter()
                    .cloned()
                    .filter(|child| !seen.contains(child))
                    .collect::<Vec<u32>>()
            })
            .collect();

        for &index in &todo {
            seen.insert(index);
        }
    }

    println!("done: {} paths", seen.len());

    if seen.contains(&!0) {
        println!("WARNING: missing edges");
    }

    Ok(())
}

struct PathSet32 {
    table: HashTable<([u8; 20], AtomicU32)>,
}

impl PathSet32 {
    fn with_capacity(capacity: usize) -> Self {
        Self {
            table: HashTable::with_capacity(capacity),
        }
    }

    fn insert(&mut self, value: &[u8; 20]) -> bool {
        let hash = hash64(value);

        match self
            .table
            .entry(hash, |(x, _)| x == value, |(x, _)| hash64(x))
        {
            hash_table::Entry::Occupied(_) => false,
            hash_table::Entry::Vacant(entry) => {
                entry.insert((*value, AtomicU32::new(!0)));
                true
            }
        }
    }

    fn find(&self, value: &[u8; 20]) -> Option<&AtomicU32> {
        let hash = hash64(value);
        self.table
            .find(hash, |(x, _)| x == value)
            .as_ref()
            .map(|(_, x)| x)
    }

    fn len(&self) -> usize {
        self.table.len()
    }
}

impl<'a> FromIterator<&'a [u8; 20]> for PathSet32 {
    fn from_iter<T: IntoIterator<Item = &'a [u8; 20]>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let mut this = Self::with_capacity(iter.size_hint().0);

        for item in iter {
            this.insert(item);
        }

        this
    }
}

struct ChunkedList<'a, T> {
    by_offset: BTreeMap<usize, (&'a OffsetsBuffer<i64>, &'a [T])>,
}

impl<'a, T> ChunkedList<'a, T> {
    fn new(chunks: impl IntoIterator<Item = (&'a OffsetsBuffer<i64>, &'a [T])>) -> Self {
        let mut next_offset = 0usize;
        ChunkedList {
            by_offset: chunks
                .into_iter()
                .map(|(offsets, values)| {
                    let offset = next_offset;
                    next_offset = next_offset.checked_add(offsets.len_proxy()).unwrap();

                    (offset, (offsets, values))
                })
                .collect(),
        }
    }
}

impl<'a, T> Index<usize> for ChunkedList<'a, T> {
    type Output = [T];

    fn index(&self, index: usize) -> &Self::Output {
        let (&base, &(offsets, values)) = self.by_offset.range(..=index).next_back().unwrap();
        let (start, end) = offsets.start_end(index - base);
        &values[start..end]
    }
}

fn fixed_binary<const N: usize>(
    chunked: &ChunkedArray<BinaryType>,
) -> impl Iterator<Item = &[[u8; N]]> {
    let chunks = chunked.downcast_chunks();
    (0..chunks.len()).map(move |i| {
        let chunk = chunks.get(i).unwrap();
        assert!(chunk.validity().is_none(), "possible null values");

        let length_check = chunk
            .offsets()
            .as_slice()
            .par_windows(2)
            .all(|w| (w[1] - w[0]) == N as i64);

        assert!(length_check);

        let buf = chunk.values().as_slice();
        exact_chunks(buf).unwrap()
    })
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

fn hash64(h: &[u8; 20]) -> u64 {
    let mut buf = [0; 8];
    buf.copy_from_slice(&h[..8]);
    u64::from_le_bytes(buf)
}
