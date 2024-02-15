use anyhow::Result;
use hashbrown::HashTable;
use polars::prelude::*;
use rayon::prelude::*;
use std::fs::File;

use weave::{as_fixed_binary, hash64, load_ph_array, DONE, INDEX_NULL};

#[tokio::main]
async fn main() -> Result<()> {
    let ph_array = load_ph_array()?;

    // TODO(edef): re-parallelise this
    // We originally parallelised on chunks, but ph_array is only a single chunk, due to how Parquet loading works.
    // TODO(edef): outline the 64-bit hash prefix? it's an indirection, but it saves ~2G of memory
    eprint!("… build index\r");
    let ph_map: HashTable<(u64, u32)> = {
        let mut ph_map = HashTable::with_capacity(ph_array.len());

        for (offset, item) in ph_array.iter().enumerate() {
            let offset = offset as u32;
            let hash = hash64(item);
            ph_map.insert_unique(hash, (hash, offset), |&(hash, _)| hash);
        }

        ph_map
    };
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

                        out.extend(as_fixed_binary::<20>(series).flat_map(|xs| xs).map(|key| {
                            let hash = hash64(&key);
                            ph_map
                                .find(hash, |&(candidate_hash, candidate_index)| {
                                    candidate_hash == hash
                                        && &ph_array[candidate_index as usize] == key
                                })
                                .map(|&(_, index)| index)
                                .unwrap_or(INDEX_NULL)
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
