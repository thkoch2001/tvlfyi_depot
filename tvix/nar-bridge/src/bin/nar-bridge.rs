use axum::routing::get;
use axum::Router;
use clap::Parser;
use nar_bridge::get_nar;
use nar_bridge::get_narinfo;
use nar_bridge::AppState;
use tracing::info;

/// Expose the Nix HTTP Binary Cache protocol for a tvix-store.
#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(long, env, default_value = "grpc+http://[::1]:8000")]
    blob_service_addr: String,

    #[arg(long, env, default_value = "grpc+http://[::1]:8000")]
    directory_service_addr: String,

    #[arg(long, env, default_value = "grpc+http://[::1]:8000")]
    path_info_service_addr: String,

    /// The priority to announce at the `nix-cache-info` endpoint.
    /// A lower number means it's *more preferred.
    #[arg(long, env, default_value_t = 39)]
    priority: u64,

    /// The address to listen on.
    #[clap(flatten)]
    listen_address: Option<tokio_listener::ListenerAddressLFlag>,

    #[cfg(feature = "otlp")]
    /// Whether to configure OTLP. Set --otlp=false to disable.
    #[arg(long, default_missing_value = "true", default_value = "true", num_args(0..=1), require_equals(true), action(clap::ArgAction::Set))]
    otlp: bool,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let listener_arg = cli
        .listen_address
        .unwrap_or_else(|| tokio_listener::ListenerAddressLFlag {
            listen_address: Some(
                "[::1]:3000"
                    .parse()
                    .expect("invalid fallback listen address"),
            ),
            listener_options: tokio_listener::UserOptions::default(),
        });

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
    let (blob_service, directory_service, path_info_service, _nar_calculation_service) =
        tvix_store::utils::construct_services(
            cli.blob_service_addr,
            cli.directory_service_addr,
            cli.path_info_service_addr,
        )
        .await?;

    let state = AppState::new(blob_service, directory_service, path_info_service.into());

    let app = Router::new()
        .route("/", get(root))
        .route("/nar/:root_node_enc", get(get_nar))
        .route("/:narinfo_str", get(get_narinfo))
        .route("/nix-cache-info", get(move || nix_cache_info(cli.priority)))
        .with_state(state.clone());

    let listener = listener_arg.bind().await.expect("invalid listener")?;
    info!(listen_address=%listener_arg.listen_address.unwrap(), "starting daemon");

    tokio_listener::axum07::serve(
        listener,
        app.into_make_service_with_connect_info::<tokio_listener::SomeSocketAddrClonable>(),
    )
    .await?;

    Ok(())
}

async fn root() -> &'static str {
    "Hello from nar-bridge"
}

async fn nix_cache_info(priority: u64) -> String {
    format!(
        "StoreDir: /nix/store\nWantMassQuery: 1\nPriority: {}\n",
        priority
    )
}
