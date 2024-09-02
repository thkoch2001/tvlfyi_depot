use clap::Parser;
use mimalloc::MiMalloc;
use tracing::{Level, Span};
use tracing_indicatif::span_ext::IndicatifSpanExt;
use tvix_eval::Value;
use tvix_glue::configure_nix_path;

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

    /// Strictly evaluate values, traversing them and forcing e.g.
    /// elements of lists and attribute sets before printing the
    /// return value.
    #[clap(long)]
    pub strict: bool,
}

fn recurse_for_derivations(toplevel: Value, path: Option<String>) {
    println!("path: {:?}", path);
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
                        println!("{:?}", value);
                        recurse_for_derivations(
                            value,
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

fn main() {
    let args = Args::parse();

    let _ = tvix_tracing::TracingBuilder::default()
        .level(args.log_level)
        .enable_progressbar()
        .build()
        .expect("unable to set up tracing subscriber");

    let span = Span::current();
    span.pb_start();
    span.pb_set_style(&tvix_tracing::PB_SPINNER_STYLE);
    span.pb_set_message("Setting up evaluator…");

    let mut eval_builder = tvix_eval::Evaluator::builder_impure()
        .enable_import()
        .with_strict(args.strict)
        .env(None);

    eval_builder = configure_nix_path(eval_builder, &args.nix_search_path);
    span.pb_set_message("Evaluating…");

    let eval = eval_builder.build();
    let result = eval.evaluate(args.expr.unwrap(), None);
    println!("Result: {:?}", result);
    let toplevel = result.value.unwrap();

    recurse_for_derivations(toplevel, None);
}
