use std::sync::Arc;

use futures::stream::BoxStream;
use tonic::{
    async_trait,
    transport::{Endpoint, Server, Uri},
};

use super::BSDSPS;
use crate::{
    pathinfoservice::{GRPCPathInfoService, MemoryPathInfoService, PathInfoService},
    proto::{
        path_info_service_client::PathInfoServiceClient,
        path_info_service_server::PathInfoServiceServer, GRPCPathInfoServiceWrapper, PathInfo,
    },
    tests::fixtures::{blob_service, directory_service},
};
use tvix_castore::proto as castorepb;
use tvix_castore::Error;

/// Constructs and returns a gRPC PathInfoService.
/// We also return memory-based {Blob,Directory}Service,
/// as the consumer of this function accepts a 3-tuple.
pub async fn make_grpc_path_info_service_client() -> super::BSDSPS {
    let (left, right) = tokio::io::duplex(64);

    let blob_service = blob_service();
    let directory_service = directory_service();

    // spin up a server, which will only connect once, to the left side.
    tokio::spawn({
        let blob_service = blob_service.clone();
        let directory_service = directory_service.clone();
        async move {
            let path_info_service: Arc<dyn PathInfoService> =
                Arc::from(MemoryPathInfoService::new(blob_service, directory_service));

            // spin up a new DirectoryService
            let mut server = Server::builder();
            let router = server.add_service(PathInfoServiceServer::new(
                GRPCPathInfoServiceWrapper::new(path_info_service),
            ));

            router
                .serve_with_incoming(tokio_stream::once(Ok::<_, std::io::Error>(left)))
                .await
        }
    });

    // Create a client, connecting to the right side. The URI is unused.
    let mut maybe_right = Some(right);

    let path_info_service = Box::new(GRPCPathInfoService::from_client(
        PathInfoServiceClient::new(
            Endpoint::try_from("http://[::]:50051")
                .unwrap()
                .connect_with_connector(tower::service_fn(move |_: Uri| {
                    let right = maybe_right.take().unwrap();
                    async move { Ok::<_, std::io::Error>(right) }
                }))
                .await
                .unwrap(),
        ),
    ));

    (blob_service, directory_service, path_info_service)
}

/// A PathInfo Service holding onto other things.
/// This can be used to construct a PathInfoService cleaning up test resources on drop.
struct WrappedPathInfoService<T, O> {
    inner: T,
    #[allow(dead_code)]
    others: O,
}
#[async_trait]
impl<T, O> PathInfoService for WrappedPathInfoService<T, O>
where
    T: PathInfoService,
    O: Send + Sync,
{
    async fn get(&self, digest: [u8; 20]) -> Result<Option<PathInfo>, Error> {
        self.inner.get(digest).await
    }

    async fn put(&self, path_info: PathInfo) -> Result<PathInfo, Error> {
        self.inner.put(path_info).await
    }

    async fn calculate_nar(
        &self,
        root_node: &castorepb::node::Node,
    ) -> Result<(u64, [u8; 32]), Error> {
        self.inner.calculate_nar(root_node).await
    }

    fn list(&self) -> BoxStream<'static, Result<PathInfo, Error>> {
        self.inner.list()
    }
}

#[cfg(feature = "cloud")]
/// Returns a `Box<dyn PathInfoService>` that's connected to a fresh Google
/// BigTable emulator.
pub async fn make_bigtable() -> BSDSPS {
    use std::time::Duration;

    use async_process::{Command, Stdio};
    use tempfile::TempDir;
    use tokio_retry::{strategy::ExponentialBackoff, Retry};
    use tvix_castore::{blobservice::BlobService, directoryservice::DirectoryService};

    use crate::pathinfoservice::BigtablePathInfoService;

    let project_id = "project-1";
    let instance_name = "instance-1";

    let tmpdir = TempDir::new().unwrap();

    let socket_path = tmpdir.path().join("cbtemulator.sock");

    let emulator_process = Command::new("cbtemulator")
        .arg("-address")
        .arg(socket_path.clone())
        .stderr(Stdio::piped())
        .stdout(Stdio::piped())
        .kill_on_drop(true)
        .spawn()
        .expect("failed to spwan emulator");

    Retry::spawn(
        ExponentialBackoff::from_millis(20).max_delay(Duration::from_secs(1)),
        || async {
            if socket_path.exists() {
                Ok(())
            } else {
                Err(())
            }
        },
    )
    .await
    .expect("failed to wait for socket");

    let table_name = "table-1";
    let family_name = "cf1";

    // populate the emulator
    for cmd in &[
        vec!["createtable", table_name],
        vec!["createfamily", table_name, family_name],
    ] {
        Command::new("cbt")
            .args({
                let mut args = vec!["-instance", instance_name, "-project", project_id];
                args.extend_from_slice(cmd);
                args
            })
            .env(
                "BIGTABLE_EMULATOR_HOST",
                format!("unix://{}", socket_path.to_string_lossy()),
            )
            .output()
            .await
            .expect("failed to run cbt setup command");
    }

    let bigtable_conn = bigtable_rs::bigtable::BigTableConnection::new_with_emulator(
        &format!("unix://{}", socket_path.to_string_lossy()),
        project_id,
        instance_name,
        false,
        None,
    )
    .await
    .expect("must connect");

    let blob_service: Arc<dyn BlobService> = tvix_castore::blobservice::from_addr("memory://")
        .await
        .unwrap()
        .into();
    let directory_service: Arc<dyn DirectoryService> =
        tvix_castore::directoryservice::from_addr("memory://")
            .await
            .unwrap()
            .into();

    (
        blob_service,
        directory_service,
        Box::new(WrappedPathInfoService {
            inner: BigtablePathInfoService::from_client(
                bigtable_conn.client(),
                table_name.into(),
                family_name.into(),
                "default".into(),
            ),
            others: (tmpdir, emulator_process),
        }),
    )
}
