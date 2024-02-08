use anyhow::Result;
use hashbrown::HashTable;
use nix_compat::nixbase32;
use rayon::prelude::*;
use std::{
    collections::{BTreeMap, HashSet, VecDeque},
    fs::{self, File},
    ops::{Index, Range},
    slice,
};

use polars::{
    export::arrow::{array::BinaryArray, offset::OffsetsBuffer},
    prelude::*,
};

fn main() -> Result<()> {
    let mut roots = Vec::new();
    fs::read("nixpkgs.roots")?
        .par_chunks_exact(32 + 1)
        .map(|e| nixbase32::decode_fixed::<20>(&e[0..32]).unwrap())
        .collect_into_vec(&mut roots);

    println!("parsed roots");

    let df = IpcReader::new(File::open("narinfo-references.arrow")?)
        .memory_mapped(true)
        .finish()?;

    println!("got DataFrame {:?}", df.shape());

    let (ph_array, ph_map) = {
        let chunks = df.column("store_path_hash")?.binary()?;
        let chunks = ChunkedSlice::new(unsafe { fixed_binary_unchecked::<20>(chunks) });

        println!("building index");

        let output: Vec<HashTable<_>> = chunks
            .by_offset
            .par_iter()
            .map(|(&base, &slice)| {
                let base: u32 = base.try_into().unwrap();
                let mut table = HashTable::with_capacity(slice.len());

                for (offset, item) in slice.iter().enumerate() {
                    let offset = offset as u32;
                    let hash = hash64(item);
                    table.insert_unique(hash, (hash, base + offset), |&(hash, _)| hash);
                }

                table
            })
            .collect();

        let n: usize = output.iter().map(|t| t.len()).sum();
        println!("built chunked indexes over {n} elements");

        // TODO(edef): use Rayon to build merged indexes incrementally
        let mut table = HashTable::with_capacity(n as usize);
        for chunk in output {
            for (hash, index) in chunk {
                table.insert_unique(hash, (hash, index), |&(hash, _)| hash);
            }
        }

        println!("built merged index over {} elements", table.len());

        (chunks, table)
    };

    let references = ChunkedList::new({
        let chunks = df.column("references")?.list()?.downcast_chunks();
        (0..chunks.len()).map(move |chunk_index| {
            let chunk = chunks.get(chunk_index).unwrap();

            let value_offsets = chunk.offsets();
            assert_eq!(chunk.len() + 1, value_offsets.len());

            let values: &BinaryArray<i64> = chunk.values().as_any().downcast_ref().unwrap();
            let head = values.get(0).unwrap();
            assert_eq!(head.len(), 20);

            let values: &[[u8; 20]] =
                unsafe { slice::from_raw_parts(head.as_ptr().cast(), values.len()) };

            (value_offsets, values)
        })
    });

    let ph_lookup = |key: &[u8; 20]| -> Option<u32> {
        let hash = hash64(&key);
        ph_map
            .find(hash, |&(candidate_hash, candidate_index)| {
                candidate_hash == hash && &ph_array[candidate_index as usize] == key
            })
            .map(|&(_, index)| index)
    };

    let mut todo = HashSet::new();
    let mut unknown_roots = 0u64;
    for root in roots {
        if let Some(root) = ph_lookup(&root) {
            todo.insert(root);
        } else {
            unknown_roots += 1;
        }
    }
    println!("skipping {unknown_roots} unknown roots");

    let mut seen = todo.clone();
    while !todo.is_empty() {
        println!("todo: {} seen: {}", todo.len(), seen.len());

        todo = todo
            .par_iter()
            .flat_map(|&parent| {
                references[parent as usize]
                    .iter()
                    .map(|child| ph_lookup(child).unwrap())
                    .filter(|child| !seen.contains(child))
                    .collect::<Vec<u32>>()
            })
            .collect();

        for &index in &todo {
            seen.insert(index);
        }
    }

    println!("done: {} paths", seen.len());

    // for entry in seen {
    //     println!("{}", nixbase32::encode(&entry));
    // }

    Ok(())
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

struct ChunkedSlice<'a, T> {
    by_offset: BTreeMap<usize, &'a [T]>,
}

impl<'a, T> ChunkedSlice<'a, T> {
    fn new(chunks: impl IntoIterator<Item = &'a [T]>) -> Self {
        let mut next_offset = 0usize;
        ChunkedSlice {
            by_offset: chunks
                .into_iter()
                .map(|slice| {
                    let offset = next_offset;
                    next_offset = next_offset.checked_add(slice.len()).unwrap();
                    (offset, slice)
                })
                .collect(),
        }
    }
}

impl<'a, T> Index<usize> for ChunkedSlice<'a, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        let (&base, &chunk) = self.by_offset.range(..=index).next_back().unwrap();
        &chunk[index - base]
    }
}

impl<'a, T> Index<Range<usize>> for ChunkedSlice<'a, T> {
    type Output = [T];

    fn index(&self, mut index: Range<usize>) -> &Self::Output {
        let (&base, &chunk) = self.by_offset.range(..=index.start).next_back().unwrap();
        index.start -= base;
        index.end -= base;
        &chunk[index]
    }
}

unsafe fn fixed_binary_unchecked<const N: usize>(
    chunked: &ChunkedArray<BinaryType>,
) -> impl Iterator<Item = &[[u8; N]]> {
    let chunks = chunked.downcast_chunks();
    (0..chunks.len()).map(move |i| unsafe {
        let chunk = chunks.get(i).unwrap();
        let head = chunk.get(0).unwrap();
        assert_eq!(head.len(), N);

        slice::from_raw_parts(head.as_ptr().cast::<[u8; N]>(), chunk.len())
    })
}

fn hash64(h: &[u8; 20]) -> u64 {
    let mut buf = [0; 8];
    buf.copy_from_slice(&h[..8]);
    u64::from_le_bytes(buf)
}
