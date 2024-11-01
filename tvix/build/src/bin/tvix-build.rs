use clap::Parser;
use clap::Subcommand;
use tokio_listener::Listener;
use tokio_listener::SystemOptions;
use tokio_listener::UserOptions;
use tonic::{self, transport::Server};
use tracing::info;
use tvix_build::{
    buildservice,
    proto::{build_service_server::BuildServiceServer, GRPCBuildServiceWrapper},
};
use tvix_castore::blobservice;
use tvix_castore::directoryservice;

#[cfg(feature = "tonic-reflection")]
use tvix_build::proto::FILE_DESCRIPTOR_SET;
#[cfg(feature = "tonic-reflection")]
use tvix_castore::proto::FILE_DESCRIPTOR_SET as CASTORE_FILE_DESCRIPTOR_SET;

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}
#[derive(Subcommand)]
enum Commands {
    /// Runs the tvix-build daemon.
    Daemon {
        #[arg(long, short = 'l')]
        listen_address: Option<String>,

        #[arg(long, env, default_value = "grpc+http://[::1]:8000")]
        blob_service_addr: String,

        #[arg(long, env, default_value = "grpc+http://[::1]:8000")]
        directory_service_addr: String,

        #[arg(long, env, default_value = "dummy://")]
        build_service_addr: String,
    },
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let cli = Cli::parse();

    tvix_tracing::TracingBuilder::default()
        .enable_progressbar()
        .build()?;

    match cli.command {
        Commands::Daemon {
            listen_address,
            blob_service_addr,
            directory_service_addr,
            build_service_addr,
        } => {
            // initialize stores
            let blob_service = blobservice::from_addr(&blob_service_addr).await?;
            let directory_service = directoryservice::from_addr(&directory_service_addr).await?;

            let build_service =
                buildservice::from_addr(&build_service_addr, blob_service, directory_service)
                    .await?;

            let listen_address = listen_address
                .unwrap_or_else(|| "[::]:8000".to_string())
                .parse()
                .unwrap();

            let mut server = Server::builder();

            #[allow(unused_mut)]
            let mut router = server.add_service(BuildServiceServer::new(
                GRPCBuildServiceWrapper::new(build_service),
            ));

            #[cfg(feature = "tonic-reflection")]
            {
                router = router.add_service(
                    tonic_reflection::server::Builder::configure()
                        .register_encoded_file_descriptor_set(CASTORE_FILE_DESCRIPTOR_SET)
                        .register_encoded_file_descriptor_set(FILE_DESCRIPTOR_SET)
                        .build_v1alpha()?,
                );
                router = router.add_service(
                    tonic_reflection::server::Builder::configure()
                        .register_encoded_file_descriptor_set(CASTORE_FILE_DESCRIPTOR_SET)
                        .register_encoded_file_descriptor_set(FILE_DESCRIPTOR_SET)
                        .build_v1()?,
                );
            }

            info!(listen_address=%listen_address, "listening");

            let listener = Listener::bind(
                &listen_address,
                &SystemOptions::default(),
                &UserOptions::default(),
            )
            .await?;

            router.serve_with_incoming(listener).await?;
        }
    }

    Ok(())
}
