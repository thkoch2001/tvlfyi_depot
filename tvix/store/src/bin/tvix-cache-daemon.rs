use clap::Parser;
use tokio_listener::Listener;
use tokio_listener::SystemOptions;
use tokio_listener::UserOptions;
use tonic::transport::Server;
use tracing::info;
use tvix_castore::proto::blob_service_server::BlobServiceServer;
use tvix_castore::proto::directory_service_server::DirectoryServiceServer;
use tvix_castore::proto::GRPCBlobServiceWrapper;
use tvix_castore::proto::GRPCDirectoryServiceWrapper;
use tvix_store::nar::NarCalculationService;
use tvix_store::nar::SimpleRenderer;
use tvix_store::pathinfoservice;
use tvix_store::pathinfoservice::CachePathInfoService;
use tvix_store::pathinfoservice::PathInfoService;
use tvix_store::proto::path_info_service_server::PathInfoServiceServer;
use tvix_store::proto::GRPCPathInfoServiceWrapper;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(
        long,
        env,
        default_value = "objectstore+file:///var/lib/tvix-store/blobs.object_store"
    )]
    blob_service_addr: String,

    #[arg(
        long,
        env,
        default_value = "sled:///var/lib/tvix-store/directories.sled"
    )]
    directory_service_addr: String,

    /// The PathInfoService to insert PathInfos in after downloading NARs
    #[arg(long, env, default_value = "sled:///var/lib/tvix-store/pathinfo.sled")]
    path_info_service_addr: String,

    /// The nix+https:// URL for the binary cache to use.
    #[arg(
        long,
        env,
        default_value = "nix+https://cache.nixos.org?trusted-public-keys=cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    )]
    binary_cache_url: String,

    /// The address to listen on.
    #[arg(long, short = 'l')]
    listen_address: Option<String>,

    #[cfg(feature = "otlp")]
    /// Whether to configure OTLP. Set --otlp=false to disable.
    #[arg(long, default_missing_value = "true", default_value = "true", num_args(0..=1), require_equals(true), action(clap::ArgAction::Set))]
    otlp: bool,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let listen_address = cli
        .listen_address
        .unwrap_or_else(|| "[::]:8000".to_string())
        .parse()
        .unwrap();

    let _tracing_handle = {
        #[allow(unused_mut)]
        let mut builder = tvix_tracing::TracingBuilder::default();
        #[cfg(feature = "otlp")]
        {
            if cli.otlp {
                builder = builder.enable_otlp("tvix.store");
            }
        }
        builder.build()?
    };

    // initialize stores
    let (blob_service, directory_service, local_path_info_service, _nar_calculation_service) =
        tvix_store::utils::construct_services(
            cli.blob_service_addr,
            cli.directory_service_addr,
            cli.path_info_service_addr,
        )
        .await?;

    let binary_cache_path_info_service = pathinfoservice::from_addr(
        &cli.binary_cache_url,
        blob_service.clone(),
        directory_service.clone(),
    )
    .await?;

    // connect the two PathinfoServices with a cache combinator, so PathInfo
    // calculated by parsing the NARs will be inserted into the PathInfoService.
    let path_info_service =
        CachePathInfoService::new(local_path_info_service, binary_cache_path_info_service);

    let mut server = Server::builder();

    let router = server
        .add_service(PathInfoServiceServer::new(GRPCPathInfoServiceWrapper::new(
            Box::new(path_info_service) as Box<dyn PathInfoService>,
            Box::new(SimpleRenderer::new(
                blob_service.clone(),
                directory_service.clone(),
            )) as Box<dyn NarCalculationService>,
        )))
        .add_service(BlobServiceServer::new(GRPCBlobServiceWrapper::new(
            blob_service,
        )))
        .add_service(DirectoryServiceServer::new(
            GRPCDirectoryServiceWrapper::new(directory_service),
        ));

    info!(listen_address=%listen_address, "starting daemon");

    let listener = Listener::bind(
        &listen_address,
        &SystemOptions::default(),
        &UserOptions::default(),
    )
    .await?;

    router.serve_with_incoming(listener).await?;

    Ok(())
}
