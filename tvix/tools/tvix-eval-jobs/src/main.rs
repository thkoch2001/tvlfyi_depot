use clap::Parser;
use color_eyre::{
    eyre::{eyre, Context, OptionExt},
    Result,
};
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

fn init_io_handle(
    tokio_handle: &tokio::runtime::Handle,
    args: &cli::Args,
) -> Result<Rc<TvixStoreIO>> {
    let (blob_service, directory_service, path_info_service, nar_calculation_service) =
        tokio_handle
            .block_on(tvix_store::utils::construct_services(
                args.service_addrs.clone(),
            ))
            .map_err(|e| eyre!(e))
            .wrap_err_with(|| {
                "Failed to setup {blob|directory|pathinfo}service before interpreter setup"
            })?;

    let build_service = tokio_handle
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
        .wrap_err_with(|| "Failed to setup buildservice before interpreter setup")?;

    Ok(Rc::new(TvixStoreIO::new(
        blob_service,
        directory_service,
        path_info_service,
        nar_calculation_service.into(),
        build_service.into(),
        tokio_handle.clone(),
    )))
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let args = cli::Args::parse();

    let _ = tvix_tracing::TracingBuilder::default()
        .enable_progressbar()
        .build()?;

    let span = tracing::Span::current();
    let tokio_runtime = tokio::runtime::Runtime::new()?;

    let store = init_io_handle(tokio_runtime.handle(), &args);
    let mut walker: Walker = Walker {
        nix_search_path: args.nix_search_path,
        force_recurse: args.force_recurse,
        include_meta: args.include_meta,
        globals: None,
        store: store?,
    };

    span.pb_start();
    span.pb_set_style(&tvix_tracing::PB_SPINNER_STYLE);
    span.pb_set_message("Booting…");

    walker.setup();
    walker.boot(args.expr.ok_or_eyre("No expression provided")?)?;

    Ok(())
}
