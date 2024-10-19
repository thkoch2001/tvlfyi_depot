use std::fs::File;

use hashbrown::HashTable;
use nix_compat::nixbase32;
use polars::{lazy::dsl::SpecialEq, prelude::*};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use weave::{hash64, into_fixed_binary_rechunk};

fn main() -> anyhow::Result<()> {
    let ph_map = {
        let mut pq = ParquetReader::new(File::open("sort.parquet")?);
        let meta = pq.get_metadata()?.clone();
        let mut pq = pq.batched(1 << 16)?;

        let mut ph_map: HashTable<(u32, [u8; 20], [u8; 16])> =
            HashTable::with_capacity(meta.num_rows);
        let runtime = tokio::runtime::Runtime::new()?;

        let bar = indicatif::ProgressBar::new(meta.num_rows as u64);
        while let Some(batches) = runtime.block_on(pq.next_batches(48))? {
            let len = batches.iter().map(|df| df.shape().0).sum::<usize>();

            let ents = batches
                .into_par_iter()
                .flat_map(|df| {
                    let ph_iter = df
                        .column("store_path_hash_str")
                        .unwrap()
                        .binary()
                        .unwrap()
                        .into_iter()
                        .map(Option::unwrap);

                    let ts_iter = df
                        .column("timestamp")
                        .unwrap()
                        .u32()
                        .unwrap()
                        .into_iter()
                        .map(Option::unwrap);

                    let md5_iter = df
                        .column("md5")
                        .unwrap()
                        .binary()
                        .unwrap()
                        .into_iter()
                        .map(Option::unwrap);

                    ph_iter
                        .zip(ts_iter.zip(md5_iter))
                        .map(|(ph, (ts, md5))| {
                            let ph = nixbase32::decode_fixed::<20>(ph).unwrap();
                            let md5: [u8; 16] = md5.try_into().unwrap();

                            (ts, ph, md5)
                        })
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>();

            for ent in ents {
                ph_map.insert_unique(hash64(&ent.1), ent, |(_, ph, _)| hash64(ph));
            }

            bar.inc(len as u64);
        }

        ph_map
    };

    println!("built index");

    LazyFrame::scan_parquet("narinfo.pq/*", ScanArgsParquet::default())?
        .with_column(
            as_struct(vec![col("store_path_hash"), col("narinfo_md5")])
                .map(
                    move |series: Series| -> PolarsResult<Option<Series>> {
                        let df = series.struct_()?.clone().unnest();

                        let ph_array = into_fixed_binary_rechunk::<20>(
                            df.column("store_path_hash")?.binary()?,
                        );
                        let md5_array =
                            into_fixed_binary_rechunk::<16>(df.column("narinfo_md5")?.binary()?);

                        assert_eq!(series.len(), ph_array.len());
                        assert_eq!(series.len(), md5_array.len());

                        let ts_array = Int64Chunked::from_iter_values(
                            "timestamp",
                            ph_array.iter().zip(md5_array.iter()).map(|(ph, md5)| {
                                let &(ts, _, ref narinfo_md5) = ph_map
                                    .find(hash64(ph), |(_, candidate, _)| ph == candidate)
                                    .unwrap();

                                assert_eq!(md5, narinfo_md5);
                                (ts as i64) * 1_000
                            }),
                        );

                        assert_eq!(series.len(), ts_array.len());

                        Ok(Some(
                            ts_array
                                .into_datetime(TimeUnit::Milliseconds, None)
                                .into_series(),
                        ))
                    },
                    SpecialEq::from_type(DataType::Datetime(TimeUnit::Milliseconds, None)),
                )
                .alias("timestamp"),
        )
        .with_streaming(true)
        .sink_parquet(
            "narinfo-ts.parquet".into(),
            ParquetWriteOptions {
                statistics: true,
                maintain_order: false,
                ..Default::default()
            },
        )?;

    Ok(())
}
