use clap::Parser;
use mimalloc::MiMalloc;
use std::rc::Rc;
use tracing_indicatif::span_ext::IndicatifSpanExt;
use tvix_glue::tvix_store_io::TvixStoreIO;

mod cli;
mod derivation;
mod errors;
mod walker;

use walker::Walker;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn init_io_handle(tokio_runtime: &tokio::runtime::Runtime, args: &cli::Args) -> Rc<TvixStoreIO> {
    let (blob_service, directory_service, path_info_service, nar_calculation_service) =
        tokio_runtime
            .block_on(tvix_store::utils::construct_services(
                args.service_addrs.clone(),
            ))
            .expect("unable to setup {blob|directory|pathinfo}service before interpreter setup");

    let build_service = tokio_runtime
        .block_on({
            let blob_service = blob_service.clone();
            let directory_service = directory_service.clone();
            async move {
                tvix_build::buildservice::from_addr(
                    &args.build_service_addr,
                    blob_service.clone(),
                    directory_service.clone(),
                )
                .await
            }
        })
        .expect("unable to setup buildservice before interpreter setup");

    Rc::new(TvixStoreIO::new(
        blob_service,
        directory_service,
        path_info_service,
        nar_calculation_service.into(),
        build_service.into(),
        tokio_runtime.handle().clone(),
    ))
}

fn main() {
    let args = cli::Args::parse();

    let _ = tvix_tracing::TracingBuilder::default()
        .enable_progressbar()
        .build()
        .expect("unable to set up tracing subscriber");

    let span = tracing::Span::current();
    let tokio_runtime = tokio::runtime::Runtime::new().expect("failed to setup tokio runtime");
    let store = init_io_handle(&tokio_runtime, &args);
    let mut walker: Walker = Walker {
        nix_search_path: args.nix_search_path,
        force_recurse: args.force_recurse,
        include_meta: args.include_meta,
        globals: None,
        store,
    };

    span.pb_start();
    span.pb_set_style(&tvix_tracing::PB_SPINNER_STYLE);
    span.pb_set_message("Booting…");

    walker.setup();
    walker.boot(args.expr.unwrap());
}
