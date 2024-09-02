use std::{collections::HashMap, path::PathBuf, rc::Rc, sync::Arc};

use bstr::{BString, ByteSlice};
use clap::Parser;
use mimalloc::MiMalloc;
use nix_compat::derivation::Derivation;
use serde::Serialize;
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

    /// Force recursion into attribute sets until derivations are encountered.
    /// This ignores any `recurseForDerivations` field.
    #[clap(long, default_value_t = false)]
    pub force_recurse: bool,

    /// Include the `meta` attribute of derivations
    #[clap(long, default_value_t = false)]
    pub include_meta: bool,

    /// Directory where derivation recipes (`.drv`) will be accumulated.
    #[clap(long)]
    pub derivation_directory: Option<PathBuf>,

    /// A colon-separated list of directories to use to resolve `<...>`-style paths
    #[clap(long, short = 'I', env = "NIX_PATH")]
    pub nix_search_path: Option<String>,

    #[clap(flatten)]
    pub service_addrs: tvix_store::utils::ServiceUrlsMemory,

    #[arg(long, env, default_value = "dummy://")]
    pub build_service_addr: String,
}

#[derive(Debug, Serialize)]
struct DerivationInfo {
    name: String,
    attr_path: String,
    drv_path: String,
    out_path: String,
    meta: Option<HashMap<String, serde_json::Value>>,
}

impl DerivationInfo {
    fn new(attr_path: String, name: Value, drv_path: Value, out_path: Value) -> Self {
        Self {
            attr_path,
            name: name
                .to_str()
                .expect("Did not found a name")
                .as_bytes()
                .to_str_lossy()
                .to_string(),
            drv_path: drv_path
                .to_contextful_str()
                .expect("Did not found a string derivation path")
                .as_bytes()
                .to_str_lossy()
                .to_string(),
            out_path: out_path
                .to_contextful_str()
                .expect("Did not found a string output path")
                .as_bytes()
                .to_str_lossy()
                .to_string(),
            meta: None,
        }
    }
}

#[derive(Debug, thiserror::Error, Serialize)]
enum EvaluationError {
    #[error("expecting `{0}` attribute in a derivation")]
    MissingAttribute(String),
    #[error("expecting an attribute set in a derivation, found `{0}`")]
    MalformedDerivation(String),
    #[error("shallow force failure")]
    ShallowForceFailure {
        /// Which attribute were we evaluating
        value_path: String,
        // error_pretty_fmt: String,
    },
    #[error("deep force failure")]
    DeepForceFailure {
        /// Which attribute were we evaluating
        value_path: String,
        // error_pretty_fmt: String,
    },
}

struct Walker {
    nix_search_path: Option<String>,
    force_recurse: bool,
    include_meta: bool,
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

    fn evaluation_build(&self) -> tvix_eval::Evaluation<'co, 'ro, 'env, Box<dyn EvalIO>> {
        self.evaluation_builder().build()
    }

    /// Setups the GlobalMap once.
    fn setup(&mut self) {
        let evaluator = self.evaluation_build();
        self.globals = Some(evaluator.globals());
    }

    fn shallow_force(&self, value: Value, span: codemap::Span) -> tvix_eval::EvaluationResult {
        self.evaluation_build().shallow_force(value, span)
    }

    fn deep_force(&self, value: Value, span: codemap::Span) -> tvix_eval::EvaluationResult {
        self.evaluation_build().deep_force(value, span)
    }

    fn is_derivation(&self, v: &Value, span: codemap::Span) -> bool {
        match v {
            Value::Attrs(attrs) => {
                // TODO: force the type?
                let r#type = attrs.select("type");

                if let Some(r#type) = r#type {
                    let r#type = self.shallow_force(r#type.clone(), span).value.map(|v| {
                        debug_assert!(
                            !matches!(v, Value::Thunk(_)),
                            "type information is still a thunk after forcing!"
                        );
                        v.to_str()
                    });

                    if let Some(Ok(r#type)) = r#type {
                        r#type == "derivation"
                    } else {
                        // TODO: throw an error?
                        false
                    }
                } else {
                    false
                }
            }
            Value::Thunk(_) => {
                panic!("encountered thunk to check if it's a derivation, force it beforehand!")
            }
            _ => false,
        }
    }

    fn grab_derivation(
        &self,
        attr_path: String,
        v: &Value,
        span: codemap::Span,
    ) -> Result<DerivationInfo, EvaluationError> {
        if let Value::Attrs(attrs) = v {
            let drv_path = attrs
                .select_required("drvPath")
                .map_err(|_| EvaluationError::MissingAttribute("drvPath".to_string()))?;
            let out_path = attrs
                .select_required("outPath")
                .map_err(|_| EvaluationError::MissingAttribute("outPath".to_string()))?;
            let name = attrs
                .select_required("name")
                .map_err(|_| EvaluationError::MissingAttribute("name".to_string()))?;
            let drv_path_result = self.shallow_force(drv_path.clone(), span);
            let out_path_result = self.shallow_force(out_path.clone(), span);
            let name_result = self.shallow_force(name.clone(), span);

            /*let meta_attrs: Option<Box<NixAttrs>>;
            if self.include_meta {
                let meta_result =
                    self.deep_force(attrs.select_required("meta").ok()?.clone(), span);
                meta_attrs = meta_result.value?.to_attrs().ok();
            } else {
                meta_attrs = None;
            }*/

            Ok(DerivationInfo::new(
                attr_path.clone(),
                name_result
                    .value
                    .ok_or(EvaluationError::ShallowForceFailure {
                        value_path: format!("{}.{}", attr_path, "name"),
                    })?,
                drv_path_result
                    .value
                    .ok_or(EvaluationError::ShallowForceFailure {
                        value_path: format!("{}.{}", attr_path, "drvPath"),
                    })?,
                out_path_result
                    .value
                    .ok_or(EvaluationError::ShallowForceFailure {
                        value_path: format!("{}.{}", attr_path, "outPath"),
                    })?,
            ))
        } else {
            Err(EvaluationError::MalformedDerivation(
                v.type_of().to_string(),
            ))
        }
    }

    fn boot(&self, expr: String) {
        assert!(self.globals.is_some(), "Walker has not been initialized");
        let result = self.evaluation_build().evaluate(expr, None);
        self.recurse_for_derivations(result.value.unwrap(), result.span.unwrap(), None);
    }

    fn recurse_for_derivations(&self, toplevel: Value, span: codemap::Span, path: Option<String>) {
        match toplevel {
            Value::Attrs(attrs) => {
                let mut recurse = attrs.keys().any(|key| *key == "recurseForDerivations")
                    || path.is_none()
                    || self.force_recurse;

                for (key, value) in attrs.iter_sorted() {
                    if *key == "recurseForDerivations" {
                        let evaluated = self.shallow_force(value.clone(), span);
                        let recurse_for_derivations = evaluated
                            .value
                            .expect("Failed to force `recurseForDerivations`");
                        recurse = recurse_for_derivations.as_bool().unwrap_or(false)
                            || self.force_recurse;
                    }
                }

                if recurse {
                    for (key, value) in attrs.into_iter_sorted() {
                        if *key != "recurseForDerivations" {
                            let forced_result = self.shallow_force(value, span);
                            let attr_path = path.as_ref().map_or_else(
                                || key.ident_str().to_string(),
                                |p| format!("{}.{}", p, key.ident_str()),
                            );

                            if let Some(value) = forced_result.value {
                                if self.is_derivation(&value, span) && path.is_some() {
                                    match self.grab_derivation(path.clone().unwrap(), &value, span)
                                    {
                                        Ok(info) => {
                                            println!("{}", serde_json::to_string(&info).unwrap())
                                        }
                                        Err(err) => eprintln!(
                                            "{}",
                                            serde_json::to_string(&serde_json::json!({
                                                "attr": key.ident_str().to_string(),
                                                "attrPath": attr_path,
                                                "error": err
                                            }))
                                            .unwrap()
                                        ),
                                    }
                                } else {
                                    self.recurse_for_derivations(
                                        value,
                                        forced_result.span.unwrap(),
                                        Some(attr_path),
                                    );
                                }
                            } else {
                                println!(
                                    "{}",
                                    serde_json::to_string(&serde_json::json!({
                                        "attr": key.ident_str().to_string(),
                                        "attrPath": attr_path,
                                        "error": "bad thing happened while shallow forcing",
                                    }))
                                    .unwrap()
                                );
                            }
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
        force_recurse: args.force_recurse,
        include_meta: args.include_meta,
        derivations: Vec::new(),
        globals: None,
        store,
    };

    span.pb_start();
    span.pb_set_style(&tvix_tracing::PB_SPINNER_STYLE);
    span.pb_set_message("Booting…");

    walker.setup();
    walker.boot(args.expr.unwrap());
}
