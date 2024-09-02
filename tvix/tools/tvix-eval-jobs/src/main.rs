use std::rc::Rc;

use clap::Parser;
use mimalloc::MiMalloc;
use nix_compat::derivation::Derivation;
use tracing::Level;
use tracing_indicatif::span_ext::IndicatifSpanExt;
use tvix_eval::{EvalIO, GlobalsMap, NixAttrs, Value};
use tvix_glue::{configure_nix_path, tvix_io::TvixIO, tvix_store_io::TvixStoreIO};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

/// Provides a CLI interface to trigger mass evaluation jobs
///
/// Does not configure a store or a build component.
/// Sets a default set of builtins similar to these present in Nix.
///
/// No `.drv` is written to the local `/nix/store` location.
///
/// The CLI interface is not stable and subject to change.
#[derive(Parser, Clone)]
struct Args {
    /// A global log level to use when printing logs.
    /// It's also possible to set `RUST_LOG` according to
    /// `tracing_subscriber::filter::EnvFilter`, which will always have
    /// priority.
    #[arg(long, default_value_t=Level::INFO)]
    pub log_level: Level,

    #[clap(long, short = 'E')]
    pub expr: Option<String>,

    /// A colon-separated list of directories to use to resolve `<...>`-style paths
    #[clap(long, short = 'I', env = "NIX_PATH")]
    pub nix_search_path: Option<String>,

    #[clap(flatten)]
    pub service_addrs: tvix_store::utils::ServiceUrlsMemory,

    #[arg(long, env, default_value = "dummy://")]
    pub build_service_addr: String,
}

#[derive(Debug)]
struct DerivationInfo {
    drv_path: String,
    out_path: String,
}

impl DerivationInfo {
    fn new(v: &Value) -> Self {
        match v {
            Value::Attrs(attrs) => Self {
                drv_path: attrs
                    .select_required("drvPath")
                    .map(|s| s.to_str())
                    .expect("Did not found a derivation path in a derivation")
                    .expect("Did not found a string derivation path")
                    .to_string(),
                out_path: attrs
                    .select_required("outPath")
                    .map(|s| s.to_str())
                    .expect("Did not found an output path in a derivation")
                    .expect("Did not found a string output path")
                    .to_string(),
            },
            _ => panic!("Trying to create a derivation on a non-attrs"),
        }
    }
}

struct Walker {
    nix_search_path: Option<String>,
    pub derivations: Vec<DerivationInfo>,
    globals: Option<Rc<GlobalsMap>>,
    store: Rc<TvixStoreIO>,
}

pub fn init_io_handle(tokio_runtime: &tokio::runtime::Runtime, args: &Args) -> Rc<TvixStoreIO> {
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
        blob_service.clone(),
        directory_service.clone(),
        path_info_service,
        nar_calculation_service.into(),
        build_service.into(),
        tokio_runtime.handle().clone(),
    ))
}

fn is_derivation(v: &Value) -> bool {
    match v {
        Value::Attrs(attrs) => {
            // TODO: force the type?
            let r#type = attrs.select("type").map(|v| v.to_str());

            if let Some(Ok(r#type)) = r#type {
                r#type == "derivation"
            } else {
                // TODO: throw an error?
                false
            }
        }
        Value::Thunk(_) => {
            panic!("encountered thunk to check if it's a derivation, force it beforehand!")
        }
        _ => false,
    }
}

impl<'co, 'ro, 'env> Walker {
    fn evaluation_builder(&self) -> tvix_eval::EvaluationBuilder<'co, 'ro, 'env, Box<dyn EvalIO>> {
        let mut eval_builder = tvix_eval::Evaluation::builder(Box::new(
            tvix_glue::tvix_io::TvixIO::new(self.store.clone() as Rc<dyn EvalIO>),
        ) as Box<dyn EvalIO>)
        .enable_import()
        .env(None);

        match &self.globals {
            Some(globals) => {
                eval_builder = eval_builder.with_globals(globals.clone());
            }
            None => {
                eval_builder = eval_builder.add_builtins(tvix_eval::builtins::impure_builtins());
                eval_builder = tvix_glue::builtins::add_derivation_builtins(
                    eval_builder,
                    Rc::clone(&self.store),
                );
                eval_builder =
                    tvix_glue::builtins::add_fetcher_builtins(eval_builder, Rc::clone(&self.store));
                eval_builder =
                    tvix_glue::builtins::add_import_builtins(eval_builder, Rc::clone(&self.store));
            }
        }

        configure_nix_path(eval_builder, &self.nix_search_path)
    }

    fn boot(&mut self, expr: String) {
        let evaluator = self.evaluation_builder().build();

        self.globals = Some(evaluator.globals());
        let result = evaluator.evaluate(expr, None);

        println!("{:?}", result);

        self.recurse_for_derivations(result.value.unwrap(), result.span.unwrap(), None);
    }

    fn recurse_for_derivations(
        &mut self,
        toplevel: Value,
        span: codemap::Span,
        path: Option<String>,
    ) {
        println!("path: {:?}", path);
        println!("recursing into {:?}", toplevel);
        match toplevel {
            Value::Attrs(attrs) => {
                let mut recurse =
                    attrs.keys().any(|key| *key == "recurseForDerivations") || path.is_none();

                println!("Will recurse? {:?}", recurse);

                for (key, value) in attrs.iter_sorted() {
                    if *key == "recurseForDerivations" {
                        recurse = value
                            .as_bool()
                            .expect("expecting a boolean while evaluating `recurseForDerivations`");
                    }
                }

                println!("Will recurse? {:?}", recurse);

                if recurse {
                    for (key, value) in attrs.into_iter_sorted() {
                        if *key != "recurseForDerivations" {
                            let evaluator = self.evaluation_builder().build();
                            self.globals = Some(evaluator.globals());
                            let forced_result = evaluator.shallow_force(value, span);
                            let value = forced_result
                                .value
                                .as_ref()
                                .expect("Failed to evaluate an attribute value");
                            println!("=> {}", value);
                            if is_derivation(&value) {
                                self.derivations.push(DerivationInfo::new(value));
                            }
                            self.recurse_for_derivations(
                                forced_result.value.unwrap(),
                                forced_result.span.unwrap(),
                                Some(path.as_ref().map_or_else(
                                    || key.to_string(),
                                    |p| format!("{}.{}", p, key.to_string()),
                                )),
                            );
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

fn main() {
    let args = Args::parse();

    let _ = tvix_tracing::TracingBuilder::default()
        .level(args.log_level)
        .enable_progressbar()
        .build()
        .expect("unable to set up tracing subscriber");

    let span = tracing::Span::current();
    let tokio_runtime = tokio::runtime::Runtime::new().expect("failed to setup tokio runtime");
    let store = init_io_handle(&tokio_runtime, &args);
    let mut walker: Walker = Walker {
        nix_search_path: args.nix_search_path,
        derivations: Vec::new(),
        globals: None,
        store,
    };

    span.pb_start();
    span.pb_set_style(&tvix_tracing::PB_SPINNER_STYLE);
    span.pb_set_message("Booting…");

    walker.boot(args.expr.unwrap());

    println!("drvs: {:#?}", walker.derivations);
}
