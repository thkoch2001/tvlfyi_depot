use std::sync::Arc;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use tempfile::tempdir;
use tvix_castore::blobservice::{BlobService, MemoryBlobService};
use tvix_castore::directoryservice::{DirectoryService, MemoryDirectoryService};

#[allow(unused)]
fn main() {
    use criterion::BenchmarkId;
    use criterion::Criterion;
    use criterion::{criterion_group, criterion_main};

    async fn do_ingest<BS: BlobService + 'static, DS: DirectoryService + 'static>(
        p: &std::path::Path,
        blob_service: Arc<BS>,
        directory_service: Arc<DS>,
    ) {
        tvix_castore::import::ingest_path(blob_service, directory_service, p)
            .await
            .unwrap();
    }

    fn bench_ingest_path(c: &mut Criterion) {
        let temp_dir = tempdir().unwrap();
        let blob_service = Arc::new(MemoryBlobService::default());
        let directory_service = Arc::new(MemoryDirectoryService::default());

        c.bench_function("bench_ingest_path", |b| {
            b.to_async(tokio::runtime::Runtime::new().unwrap())
                .iter(|| {
                    do_ingest(
                        temp_dir.path(),
                        blob_service.clone(),
                        directory_service.clone(),
                    )
                })
        });
    }

    criterion_group!(benches, bench_ingest_path);
    criterion_main!(benches);
}
