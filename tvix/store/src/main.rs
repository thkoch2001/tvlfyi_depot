use clap::Parser;
use std::path::PathBuf;
use tonic::{transport::Server, Result};
use tracing::{info, Level};

use crate::proto::blob_service_server::BlobServiceServer;
use crate::proto::directory_service_server::DirectoryServiceServer;
use crate::proto::path_info_service_server::PathInfoServiceServer;
use crate::proto::FILE_DESCRIPTOR_SET;

mod file_blob_service;
mod memory_directory_service;
mod memory_path_info_service;
// mod null_blob_service;
mod null_directory_service;
mod null_path_info_service;
mod proto;
mod sled_directory_service;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(long)]
    store_path: Option<PathBuf>,

    #[arg(short, long)]
    log_level: Option<Level>,

    #[arg()]
    listen_address: Option<String>,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let listen_address = cli
        .listen_address
        .unwrap_or("[::]:8000".to_string())
        .parse()
        .unwrap();

    let level = cli.log_level.unwrap_or(Level::INFO);
    let subscriber = tracing_subscriber::fmt().with_max_level(level).finish();
    tracing::subscriber::set_global_default(subscriber).ok();

    let blob_service = crate::file_blob_service::FileBlobService {
        store_path: cli
            .store_path
            .unwrap_or(PathBuf::from("tvix-store"))
            .join("blob"),
    };
    // let directory_service = crate::memory_directory_service::MemoryDirectoryService::default();
    let directory_service = crate::sled_directory_service::SledDirectoryService::new(
        PathBuf::from("directories.sled"),
    )?;
    let path_info_service = crate::memory_path_info_service::MemoryPathInfoService::default();

    info!("tvix-store listening on {}", listen_address);

    let mut server = Server::builder();
    let mut router = server
        .add_service(BlobServiceServer::new(blob_service))
        .add_service(DirectoryServiceServer::new(directory_service))
        .add_service(PathInfoServiceServer::new(path_info_service));
    #[cfg(feature = "reflection")]
    {
        let reflection_svc = tonic_reflection::server::Builder::configure()
            .register_encoded_file_descriptor_set(FILE_DESCRIPTOR_SET)
            .build()?;
        router = router.add_service(reflection_svc);
    }

    router.serve(listen_address).await?;
    Ok(())
}
