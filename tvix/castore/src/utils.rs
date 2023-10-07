//! A crate containing constructors to provide instances of a BlobService and
//! DirectoryService.
//! Only used for testing purposes, but across crates.
//! Should be removed once we have a better concept of a "Service registry".

use async_stream::stream;
use core::time;
use pin_project_lite::pin_project;
use std::{path::Path, sync::Arc, task::Poll, thread};
use tokio::io::{AsyncRead, AsyncWrite, DuplexStream};
use tonic::transport::{server::Connected, Channel, Endpoint, Server, Uri};

use crate::{
    blobservice::{BlobService, MemoryBlobService},
    directoryservice::{DirectoryService, MemoryDirectoryService},
    proto::{
        directory_service_client::DirectoryServiceClient,
        directory_service_server::DirectoryServiceServer, GRPCDirectoryServiceWrapper,
    },
};

pub fn gen_blob_service() -> Arc<dyn BlobService> {
    Arc::new(MemoryBlobService::default())
}

pub fn gen_directory_service() -> Arc<dyn DirectoryService> {
    Arc::new(MemoryDirectoryService::default())
}

pin_project! {
    /// A wrapper around [DuplexStreamStream],
    /// implementing [AsyncRead] and [Connected].
    pub struct DuplexStreamWrapper {
            #[pin]
        inner: DuplexStream
    }
}

impl DuplexStreamWrapper {
    fn new(inner: DuplexStream) -> Self {
        Self { inner }
    }
}
impl AsyncRead for DuplexStreamWrapper {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        self.project().inner.poll_read(cx, buf)
    }
}

impl AsyncWrite for DuplexStreamWrapper {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, std::io::Error>> {
        todo!()
    }

    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        todo!()
    }

    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        todo!()
    }
}

impl Connected for DuplexStreamWrapper {
    type ConnectInfo = ();

    fn connect_info(&self) -> Self::ConnectInfo {
        ()
    }
}

/// This will spawn a separate thread, with its own tokio runtime, and start a gRPC server there.
/// Once it's listening, it'll start a gRPC client from the original thread, and return it.
/// FUTUREWORK: accept a closure to create the service, so we can test this with different ones.
#[allow(dead_code)]
pub(crate) async fn gen_directorysvc_grpc_client(tmpdir: &Path) -> DirectoryServiceClient<Channel> {
    let socket_path = tmpdir.join("socket");

    // TODO: not sure if a duplex can span different runtimes?
    let (mut s1, mut s2) = tokio::io::duplex(64);
    // Spin up a server, in a thread far away, which spawns its own tokio runtime,
    // and blocks on the task.
    thread::spawn(move || {
        // Create the runtime
        let rt = tokio::runtime::Runtime::new().unwrap();
        // Get a handle from this runtime
        let handle = rt.handle();

        let task = handle.spawn(async {
            // spin up a new DirectoryService
            let mut server = Server::builder();
            let router = server.add_service(DirectoryServiceServer::new(
                GRPCDirectoryServiceWrapper::from(gen_directory_service()),
            ));

            router
                .serve_with_incoming(stream! {
                    yield {
                        Result::<_, std::io::Error>::Ok(DuplexStreamWrapper::new(s1))
                    };
                })
                .await
        });

        handle.block_on(task)
    });

    // wait for the socket to be created
    // TODO: pass around FDs instead?
    {
        let mut socket_created = false;
        for _try in 1..20 {
            if socket_path.exists() {
                socket_created = true;
                break;
            }
            tokio::time::sleep(time::Duration::from_millis(20)).await;
        }

        assert!(
            socket_created,
            "expected socket path to eventually get created, but never happened"
        );
    }

    // Create a channel, connecting to the uds at socket_path.
    // The URI is unused.
    let channel = Endpoint::try_from("http://[::]:50051")
        .unwrap()
        .connect_with_connector(tower::service_fn(move |_: Uri| {
            stream! {
                yield Result::<_, std::io::Error>::Ok(DuplexStreamWrapper::new(s2));
            }
        }));

    let grpc_client = DirectoryServiceClient::new(channel);

    grpc_client
}
