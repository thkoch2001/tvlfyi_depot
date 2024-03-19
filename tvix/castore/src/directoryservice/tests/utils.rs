use crate::directoryservice::{DirectoryPutter, DirectoryService, GRPCDirectoryService};
use crate::proto::directory_service_client::DirectoryServiceClient;
use crate::proto::{self, GRPCDirectoryServiceWrapper};
use crate::{
    directoryservice::MemoryDirectoryService,
    proto::directory_service_server::DirectoryServiceServer,
};
use crate::{B3Digest, Error};
use futures::stream::BoxStream;
use tonic::async_trait;
use tonic::transport::{Endpoint, Server, Uri};

#[cfg(feature = "cloud")]
use crate::directoryservice::BigtableDirectoryService;

/// Constructs and returns a gRPC DirectoryService.
/// The server part is a [MemoryDirectoryService], exposed via the
/// [GRPCDirectoryServiceWrapper], and connected through a DuplexStream.
pub async fn make_grpc_directory_service_client() -> Box<dyn DirectoryService> {
    let (left, right) = tokio::io::duplex(64);

    // spin up a server, which will only connect once, to the left side.
    tokio::spawn(async {
        let directory_service =
            Box::<MemoryDirectoryService>::default() as Box<dyn DirectoryService>;

        let mut server = Server::builder();
        let router = server.add_service(DirectoryServiceServer::new(
            GRPCDirectoryServiceWrapper::new(directory_service),
        ));

        router
            .serve_with_incoming(tokio_stream::once(Ok::<_, std::io::Error>(left)))
            .await
    });

    // Create a client, connecting to the right side. The URI is unused.
    let mut maybe_right = Some(right);
    Box::new(GRPCDirectoryService::from_client(
        DirectoryServiceClient::new(
            Endpoint::try_from("http://[::]:50051")
                .unwrap()
                .connect_with_connector(tower::service_fn(move |_: Uri| {
                    let right = maybe_right.take().unwrap();
                    async move { Ok::<_, std::io::Error>(right) }
                }))
                .await
                .unwrap(),
        ),
    ))
}

/// A Directory Service holding onto other things.
/// This can be used to construct a DirectoryService cleaning up test resources on drop.
struct WrappedDirectoryService<T, O> {
    inner: T,
    #[allow(dead_code)]
    others: O,
}

#[async_trait]
impl<T, O> DirectoryService for WrappedDirectoryService<T, O>
where
    T: DirectoryService,
    O: Send + Sync,
{
    async fn get(&self, digest: &B3Digest) -> Result<Option<proto::Directory>, Error> {
        self.inner.get(digest).await
    }
    async fn put(&self, directory: proto::Directory) -> Result<B3Digest, Error> {
        self.inner.put(directory).await
    }
    fn get_recursive(
        &self,
        root_directory_digest: &B3Digest,
    ) -> BoxStream<Result<proto::Directory, Error>> {
        self.inner.get_recursive(root_directory_digest)
    }

    fn put_multiple_start(&self) -> Box<(dyn DirectoryPutter + 'static)> {
        self.inner.put_multiple_start()
    }
}

#[cfg(feature = "cloud")]
/// Returns a `Box<dyn DirectoryService>` that's connected to a fresh Google
/// BigTable emulator.
pub async fn make_bigtable() -> Box<dyn DirectoryService> {
    use std::time::Duration;

    use async_process::{Command, Stdio};
    use tempfile::TempDir;
    use tokio_retry::{strategy::ExponentialBackoff, Retry};

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

    Box::new(WrappedDirectoryService {
        inner: BigtableDirectoryService::from_client(
            bigtable_conn.client(),
            table_name.into(),
            family_name.into(),
            "default".into(),
        ),
        others: (tmpdir, emulator_process),
    })
}
