use anyhow::Result;
use hashbrown::HashTable;
use polars::prelude::*;
use rayon::prelude::*;
use std::{
    collections::BTreeMap,
    fs::File,
    ops::{Index, Range},
    slice,
};

const DONE: &str = "\u{2714}";

#[tokio::main]
async fn main() -> Result<()> {
    let narinfo_pq = LazyFrame::scan_parquet("narinfo.parquet", ScanArgsParquet::default())?;

    eprint!("… load store_path_hash\r");
    let ph_array = narinfo_pq
        .clone()
        .select([col("store_path_hash")])
        .collect()?
        .column("store_path_hash")?
        .binary()?
        .clone();
    let ph_array = &*Box::leak(Box::new(ph_array));

    let ph_array = ChunkedSlice::new(fixed_binary::<20>(ph_array));
    let ph_array = &*Box::leak(Box::new(ph_array));

    u32::try_from(ph_array.len()).expect("dataset exceeds 2^32");
    eprintln!("{DONE}");

    // TODO(edef): re-parallelise this
    // We originally parallelised on chunks, but ph_array is only a single chunk, due to how Parquet loading works.
    // all the ChunkedSlice magic is redundant in the face of that
    // TODO(edef): outline the 64-bit hash prefix? it's an indirection, but it saves ~2G of memory
    eprint!("… build index\r");
    let ph_map: HashTable<(u64, u32)> = {
        let mut ph_map = HashTable::with_capacity(ph_array.len());

        for (&base, &slice) in &ph_array.by_offset {
            let base = base as u32;
            for (offset, item) in slice.iter().enumerate() {
                let offset = offset as u32;
                let hash = hash64(item);
                ph_map.insert_unique(hash, (hash, base + offset), |&(hash, _)| hash);
            }
        }

        ph_map
    };
    let ph_map = &*Box::leak(Box::new(ph_map));
    eprintln!("{DONE}");

    eprint!("… swizzle references\r");
    let mut pq = ParquetReader::new(File::open("narinfo.parquet")?)
        .with_columns(Some(vec!["references".into()]))
        .batched(1 << 16)?;

    let mut reference_idxs =
        Series::new_empty("reference_idxs", &DataType::List(DataType::UInt32.into()));

    let mut bounce = vec![];
    while let Some(batches) = pq.next_batches(48).await? {
        batches
            .into_par_iter()
            .map(|df| -> ListChunked {
                df.column("references")
                    .unwrap()
                    .list()
                    .unwrap()
                    .apply_to_inner(&|series: Series| -> PolarsResult<Series> {
                        let series = series.binary()?;
                        let mut out: Vec<u32> = Vec::with_capacity(series.len());

                        out.extend(fixed_binary::<20>(series).flat_map(|xs| xs).map(|key| {
                            let hash = hash64(&key);
                            ph_map
                                .find(hash, |&(candidate_hash, candidate_index)| {
                                    candidate_hash == hash
                                        && &ph_array[candidate_index as usize] == key
                                })
                                .map(|&(_, index)| index)
                                .unwrap_or(!0)
                        }));

                        Ok(Series::from_vec("reference_idxs", out))
                    })
                    .unwrap()
            })
            .collect_into_vec(&mut bounce);

        for batch in bounce.drain(..) {
            reference_idxs.append(&batch.into_series())?;
        }
    }
    eprintln!("{DONE}");

    eprint!("… writing output\r");
    ParquetWriter::new(File::create("narinfo-references.parquet")?).finish(&mut df! {
        "reference_idxs" => reference_idxs,
    }?)?;
    eprintln!("{DONE}");

    Ok(())
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

    fn len(&self) -> usize {
        self.by_offset
            .iter()
            .next_back()
            .map(|(&base, &slice)| base + slice.len())
            .unwrap_or_default()
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

// TODO(edef): drop the OffsetsBuffer early (it's like 1.5G, and we are explicitly ignoring it)
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
