use clap::Parser;
use clap::Subcommand;

use futures::StreamExt;
use futures::TryStreamExt;
use nix_compat::nixhash::CAHash;
use nix_compat::nixhash::NixHash;
use nix_compat::wire::de::Error;
use nix_compat::{path_info::ExportedPathInfo, store_path::StorePath};
use serde::Deserialize;
use serde::Serialize;
use std::path::PathBuf;
use std::sync::Arc;
use tonic::transport::Server;
use tower::ServiceBuilder;
use tower_http::trace::{DefaultMakeSpan, TraceLayer};
use tracing::{debug, info, info_span, instrument, warn, Instrument, Level, Span};
use tracing_indicatif::span_ext::IndicatifSpanExt;
use tvix_castore::import::fs::ingest_path;
use tvix_store::import::path_to_name;
use tvix_store::nar::NarCalculationService;
use tvix_store::utils::{ServiceUrls, ServiceUrlsGrpc};
use tvix_tracing::TracingHandle;

use tvix_castore::proto::blob_service_server::BlobServiceServer;
use tvix_castore::proto::directory_service_server::DirectoryServiceServer;
use tvix_castore::proto::GRPCBlobServiceWrapper;
use tvix_castore::proto::GRPCDirectoryServiceWrapper;
use tvix_store::pathinfoservice::{PathInfo, PathInfoService};
use tvix_store::proto::path_info_service_server::PathInfoServiceServer;
use tvix_store::proto::GRPCPathInfoServiceWrapper;

#[cfg(any(feature = "fuse", feature = "virtiofs"))]
use tvix_store::pathinfoservice::make_fs;

#[cfg(feature = "fuse")]
use tvix_castore::fs::fuse::FuseDaemon;

#[cfg(feature = "virtiofs")]
use tvix_castore::fs::virtiofs::start_virtiofs_daemon;

#[cfg(feature = "tonic-reflection")]
use tvix_castore::proto::FILE_DESCRIPTOR_SET as CASTORE_FILE_DESCRIPTOR_SET;
#[cfg(feature = "tonic-reflection")]
use tvix_store::proto::FILE_DESCRIPTOR_SET;

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Whether to configure OTLP. Set --otlp=false to disable.
    #[arg(long, default_missing_value = "true", default_value = "true", num_args(0..=1), require_equals(true), action(clap::ArgAction::Set))]
    otlp: bool,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Runs the tvix-store daemon.
    Daemon {
        /// The address to listen on.
        #[clap(flatten)]
        listen_args: tokio_listener::ListenerAddressLFlag,

        #[clap(flatten)]
        service_addrs: ServiceUrls,
    },
    /// Imports a list of paths into the store, print the store path for each of them.
    Import {
        #[clap(value_name = "PATH")]
        paths: Vec<PathBuf>,

        #[clap(flatten)]
        service_addrs: ServiceUrlsGrpc,
    },

    /// Copies a list of store paths on the system into tvix-store.
    Copy {
        #[clap(flatten)]
        service_addrs: ServiceUrlsGrpc,

        /// A path pointing to a JSON file produced by the Nix
        /// `__structuredAttrs` containing reference graph information provided
        /// by the `exportReferencesGraph` feature.
        ///
        /// This can be used to invoke tvix-store inside a Nix derivation
        /// copying to a Tvix store (or outside, if the JSON file is copied
        /// out).
        ///
        /// Currently limited to the `closure` key inside that JSON file.
        #[arg(value_name = "NIX_ATTRS_JSON_FILE", env = "NIX_ATTRS_JSON_FILE")]
        reference_graph_path: PathBuf,
    },
    /// Mounts a tvix-store at the given mountpoint
    #[cfg(feature = "fuse")]
    Mount {
        #[clap(value_name = "PATH")]
        dest: PathBuf,

        #[clap(flatten)]
        service_addrs: ServiceUrlsGrpc,

        /// Number of FUSE threads to spawn.
        #[arg(long, env, default_value_t = default_threads())]
        threads: usize,

        #[arg(long, env, default_value_t = false)]
        /// Whether to configure the mountpoint with allow_other.
        /// Requires /etc/fuse.conf to contain the `user_allow_other`
        /// option, configured via `programs.fuse.userAllowOther` on NixOS.
        allow_other: bool,

        /// Whether to list elements at the root of the mount point.
        /// This is useful if your PathInfoService doesn't provide an
        /// (exhaustive) listing.
        #[clap(long, short, action)]
        list_root: bool,

        #[arg(long, default_value_t = true)]
        /// Whether to expose blob and directory digests as extended attributes.
        show_xattr: bool,
    },
    /// Starts a tvix-store virtiofs daemon at the given socket path.
    #[cfg(feature = "virtiofs")]
    #[command(name = "virtiofs")]
    VirtioFs {
        #[clap(value_name = "PATH")]
        socket: PathBuf,

        #[clap(flatten)]
        service_addrs: ServiceUrlsGrpc,

        /// Whether to list elements at the root of the mount point.
        /// This is useful if your PathInfoService doesn't provide an
        /// (exhaustive) listing.
        #[clap(long, short, action)]
        list_root: bool,

        #[arg(long, default_value_t = true)]
        /// Whether to expose blob and directory digests as extended attributes.
        show_xattr: bool,
    },
}

#[cfg(feature = "fuse")]
fn default_threads() -> usize {
    std::thread::available_parallelism()
        .map(|threads| threads.into())
        .unwrap_or(4)
}

#[instrument(skip_all)]
async fn run_cli(
    cli: Cli,
    tracing_handle: TracingHandle,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    match cli.command {
        Commands::Daemon {
            listen_args,
            service_addrs,
        } => {
            // initialize stores
            let (blob_service, directory_service, path_info_service, nar_calculation_service) =
                tvix_store::utils::construct_services(service_addrs).await?;

            let mut server = Server::builder().layer(
                ServiceBuilder::new()
                    .layer(
                        TraceLayer::new_for_grpc().make_span_with(
                            DefaultMakeSpan::new()
                                .level(Level::INFO)
                                .include_headers(true),
                        ),
                    )
                    .map_request(tvix_tracing::propagate::tonic::accept_trace),
            );

            let (_health_reporter, health_service) = tonic_health::server::health_reporter();

            #[allow(unused_mut)]
            let mut router = server
                .add_service(health_service)
                .add_service(BlobServiceServer::new(GRPCBlobServiceWrapper::new(
                    blob_service,
                )))
                .add_service(DirectoryServiceServer::new(
                    GRPCDirectoryServiceWrapper::new(directory_service),
                ))
                .add_service(PathInfoServiceServer::new(GRPCPathInfoServiceWrapper::new(
                    path_info_service,
                    nar_calculation_service,
                )));

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

            let listen_address = &listen_args.listen_address.unwrap_or_else(|| {
                "[::]:8000"
                    .parse()
                    .expect("invalid fallback listen address")
            });

            let listener = tokio_listener::Listener::bind(
                listen_address,
                &Default::default(),
                &listen_args.listener_options,
            )
            .await?;

            info!(listen_address=%listen_address, "starting daemon");

            router.serve_with_incoming(listener).await?;
        }
        Commands::Import {
            paths,
            service_addrs,
        } => {
            // FUTUREWORK: allow flat for single files?
            let (blob_service, directory_service, path_info_service, nar_calculation_service) =
                tvix_store::utils::construct_services(service_addrs).await?;

            // Arc NarCalculationService, as we clone it .
            let nar_calculation_service: Arc<dyn NarCalculationService> =
                nar_calculation_service.into();

            // For each path passed, construct the name, or bail out if it's invalid.
            let paths_and_names = paths
                .into_iter()
                .map(|p| match path_to_name(&p) {
                    Ok(name) => {
                        let name = name.to_owned();
                        Ok((p, name))
                    }
                    Err(e) => Err(e),
                })
                .collect::<Result<Vec<_>, _>>()?;

            let imports_span =
                info_span!("import paths", "indicatif.pb_show" = tracing::field::Empty);
            imports_span.pb_set_message("Importing");
            imports_span.pb_set_length(paths_and_names.len() as u64);
            imports_span.pb_set_style(&tvix_tracing::PB_PROGRESS_STYLE);
            imports_span.pb_start();

            futures::stream::iter(paths_and_names)
                .map(|(path, name)| {
                    let blob_service = blob_service.clone();
                    let directory_service = directory_service.clone();
                    let path_info_service = path_info_service.clone();
                    let nar_calculation_service = nar_calculation_service.clone();
                    let imports_span = imports_span.clone();
                    let tracing_handle = tracing_handle.clone();

                    async move {
                        let span = Span::current();
                        span.pb_set_style(&tvix_tracing::PB_SPINNER_STYLE);
                        span.pb_set_message(&format!("Ingesting {:?}", path));
                        span.pb_start();

                        // Ingest the contents at the given path into castore.
                        let root_node = ingest_path::<_, _, _, &[u8]>(
                            blob_service,
                            directory_service,
                            &path,
                            None,
                        )
                        .await
                        .map_err(std::io::Error::custom)?;

                        span.pb_set_message(&format!("NAR Calculation for {:?}", path));

                        // Ask for the NAR size and sha256
                        let (nar_size, nar_sha256) =
                            nar_calculation_service.calculate_nar(&root_node).await?;

                        // Calculate the output path. This might still fail, as some names are illegal.
                        // FUTUREWORK: express the `name` at the type level to be valid and check for this earlier.
                        let ca = CAHash::Nar(NixHash::Sha256(nar_sha256));
                        let output_path: StorePath<String> =
                            nix_compat::store_path::build_ca_path::<&str, _, _>(
                                &name,
                                &ca,
                                [],
                                false,
                            )
                            .map_err(|e| {
                                warn!(err=%e, "unable to build CA path");
                                std::io::Error::custom(e)
                            })?;

                        // Construct and insert PathInfo
                        match path_info_service
                            .as_ref()
                            .put(PathInfo {
                                store_path: output_path.to_owned(),
                                node: root_node,
                                // There's no reference scanning on imported paths
                                references: vec![],
                                nar_size,
                                nar_sha256,
                                signatures: vec![],
                                deriver: None,
                                ca: Some(ca),
                            })
                            .await
                        {
                            // If the import was successful, print the path to stdout.
                            Ok(path_info) => {
                                use std::io::Write;
                                debug!(store_path=%path_info.store_path.to_absolute_path(), "imported path");
                                writeln!(&mut tracing_handle.get_stdout_writer(), "{}", path_info.store_path.to_absolute_path())?;
                                imports_span.pb_inc(1);
                                Ok(())
                            }
                            Err(e) => {
                                warn!(?path, err=%e, "failed to import");
                                Err(e)
                            }
                        }
                    }.instrument(info_span!("import path", "indicatif.pb_show" = tracing::field::Empty))
                })
                .buffer_unordered(50)
                .try_collect::<()>()
                .await?;
        }
        Commands::Copy {
            service_addrs,
            reference_graph_path,
        } => {
            let (blob_service, directory_service, path_info_service, _nar_calculation_service) =
                tvix_store::utils::construct_services(service_addrs).await?;

            // Parse the file at reference_graph_path.
            let reference_graph_json = tokio::fs::read(&reference_graph_path).await?;

            #[derive(Deserialize, Serialize)]
            struct ReferenceGraph<'a> {
                #[serde(borrow)]
                closure: Vec<ExportedPathInfo<'a>>,
            }

            let reference_graph: ReferenceGraph<'_> =
                serde_json::from_slice(reference_graph_json.as_slice())?;

            let lookups_span = info_span!(
                "lookup pathinfos",
                "indicatif.pb_show" = tracing::field::Empty
            );
            lookups_span.pb_set_length(reference_graph.closure.len() as u64);
            lookups_span.pb_set_style(&tvix_tracing::PB_PROGRESS_STYLE);
            lookups_span.pb_start();

            // From our reference graph, lookup all pathinfos that might exist.
            let elems: Vec<_> = futures::stream::iter(reference_graph.closure)
                .map(|elem| {
                    let path_info_service = path_info_service.clone();
                    async move {
                        let resp = path_info_service
                            .get(*elem.path.digest())
                            .await
                            .map(|resp| (elem, resp));

                        Span::current().pb_inc(1);
                        resp
                    }
                })
                .buffer_unordered(50)
                // Filter out all that are already uploaded.
                // TODO: check if there's a better combinator for this
                .try_filter_map(|(elem, path_info)| {
                    std::future::ready(if path_info.is_none() {
                        Ok(Some(elem))
                    } else {
                        Ok(None)
                    })
                })
                .try_collect()
                .await?;

            // Run ingest_path on all of them.
            let uploads: Vec<_> = futures::stream::iter(elems)
                .map(|elem| {
                    // Map to a future returning the root node, alongside with the closure info.
                    let blob_service = blob_service.clone();
                    let directory_service = directory_service.clone();
                    async move {
                        // Ingest the given path.

                        ingest_path::<_, _, _, &[u8]>(
                            blob_service,
                            directory_service,
                            PathBuf::from(elem.path.to_absolute_path()),
                            None,
                        )
                        .await
                        .map(|root_node| (elem, root_node))
                    }
                })
                .buffer_unordered(10)
                .try_collect()
                .await?;

            // Insert them into the PathInfoService.
            // FUTUREWORK: do this properly respecting the reference graph.
            for (elem, root_node) in uploads {
                // Create and upload a PathInfo pointing to the root_node,
                // annotated with information we have from the reference graph.
                let path_info = PathInfo {
                    store_path: elem.path.to_owned(),
                    node: root_node,
                    references: elem.references.iter().map(StorePath::to_owned).collect(),
                    nar_size: elem.nar_size,
                    nar_sha256: elem.nar_sha256,
                    signatures: vec![],
                    deriver: None,
                    ca: None,
                };

                path_info_service.put(path_info).await?;
            }
        }
        #[cfg(feature = "fuse")]
        Commands::Mount {
            dest,
            service_addrs,
            list_root,
            threads,
            allow_other,
            show_xattr,
        } => {
            let (blob_service, directory_service, path_info_service, _nar_calculation_service) =
                tvix_store::utils::construct_services(service_addrs).await?;

            let fuse_daemon = tokio::task::spawn_blocking(move || {
                let fs = make_fs(
                    blob_service,
                    directory_service,
                    path_info_service,
                    list_root,
                    show_xattr,
                );
                info!(mount_path=?dest, "mounting");

                FuseDaemon::new(fs, &dest, threads, allow_other)
            })
            .await??;

            // Wait for a ctrl_c and then call fuse_daemon.unmount().
            tokio::spawn({
                let fuse_daemon = fuse_daemon.clone();
                async move {
                    tokio::signal::ctrl_c().await.unwrap();
                    info!("interrupt received, unmounting…");
                    tokio::task::spawn_blocking(move || fuse_daemon.unmount()).await??;
                    info!("unmount occured, terminating…");
                    Ok::<_, std::io::Error>(())
                }
            });

            // Wait for the server to finish, which can either happen through it
            // being unmounted externally, or receiving a signal invoking the
            // handler above.
            tokio::task::spawn_blocking(move || fuse_daemon.wait()).await?
        }
        #[cfg(feature = "virtiofs")]
        Commands::VirtioFs {
            socket,
            service_addrs,
            list_root,
            show_xattr,
        } => {
            let (blob_service, directory_service, path_info_service, _nar_calculation_service) =
                tvix_store::utils::construct_services(service_addrs).await?;

            tokio::task::spawn_blocking(move || {
                let fs = make_fs(
                    blob_service,
                    directory_service,
                    path_info_service,
                    list_root,
                    show_xattr,
                );
                info!(socket_path=?socket, "starting virtiofs-daemon");

                start_virtiofs_daemon(fs, socket)
            })
            .await??;
        }
    };
    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let cli = Cli::parse();

    let tracing_handle = {
        let mut builder = tvix_tracing::TracingBuilder::default();
        builder = builder.enable_progressbar();
        #[cfg(feature = "otlp")]
        {
            if cli.otlp {
                builder = builder.enable_otlp("tvix.store");
            }
        }
        builder.build()?
    };

    tokio::select! {
        res = tokio::signal::ctrl_c() => {
            res?;
            if let Err(e) = tracing_handle.force_shutdown().await {
                eprintln!("failed to shutdown tracing: {e}");
            }
            Ok(())
        },
        res = run_cli(cli, tracing_handle.clone()) => {
            if let Err(e) = tracing_handle.shutdown().await {
                eprintln!("failed to shutdown tracing: {e}");
            }
            res
        }
    }
}
