use clap::Parser;
use std::path::PathBuf;
use tonic::{transport::Server, Result};

use crate::proto::blob_service_server::BlobServiceServer;
use crate::proto::directory_service_server::DirectoryServiceServer;
use crate::proto::path_info_service_server::PathInfoServiceServer;

mod file_blob_service;
mod memory_directory_service;
mod memory_path_info_service;
mod null_blob_service;
mod null_directory_service;
mod null_path_info_service;
mod proto;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(long)]
    store_path: Option<PathBuf>,

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
    let blob_service = crate::file_blob_service::FileBlobService {
        store_path: cli
            .store_path
            .unwrap_or(PathBuf::from("tvix-store"))
            .join("blob"),
    };
    let directory_service = crate::memory_directory_service::MemoryDirectoryService::default();
    let path_info_service = crate::memory_path_info_service::MemoryPathInfoService::default();

    println!("tvix-store listening on {}", listen_address);
    Server::builder()
        .add_service(BlobServiceServer::new(blob_service))
        .add_service(DirectoryServiceServer::new(directory_service))
        .add_service(PathInfoServiceServer::new(path_info_service))
        .serve(listen_address)
        .await?;
    Ok(())
}
