use clap::Parser;
use std::path::PathBuf;
use tonic::{transport::Server, Result};

use crate::proto::blob_service_server::BlobServiceServer;

mod file_blob_service;
mod null_blob_service;

pub mod proto {
    tonic::include_proto!("tvix.store.v1");
}

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
        .unwrap_or("localhost:8000".to_string())
        .parse()
        .unwrap();
    let service = crate::file_blob_service::FileBlobService {
        store_path: cli.store_path.unwrap_or(PathBuf::from("tvix-store")),
    };
    println!("BlobService listening on {}", listen_address);

    Server::builder()
        .add_service(BlobServiceServer::new(service))
        .serve(listen_address)
        .await?;
    Ok(())
}
