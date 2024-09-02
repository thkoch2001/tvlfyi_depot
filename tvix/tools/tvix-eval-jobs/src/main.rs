use clap::Parser;
use mimalloc::MiMalloc;
use tracing::Level;
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

struct Walker {
    nix_search_path: Option<String>,
}

impl<'co, 'ro, 'env> Walker {
    fn evaluation_builder(
        &self,
    ) -> tvix_eval::EvaluationBuilder<'co, 'ro, 'env, Box<dyn tvix_eval::EvalIO>> {
        let eval_builder = tvix_eval::Evaluation::builder_impure()
            .enable_import()
            .env(None);

        configure_nix_path(eval_builder, &self.nix_search_path)
    }

    fn boot(&self, expr: String) {
        let evaluator = self.evaluation_builder().build();

        let result = evaluator.evaluate(expr, None);

        self.recurse_for_derivations(result.value.unwrap(), result.span.unwrap(), None);
    }

    fn recurse_for_derivations(&self, toplevel: Value, span: codemap::Span, path: Option<String>) {
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
                            let forced_result = evaluator.shallow_force(value, span);
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
    let walker = Walker {
        nix_search_path: args.nix_search_path,
    };

    span.pb_start();
    span.pb_set_style(&tvix_tracing::PB_SPINNER_STYLE);
    span.pb_set_message("Booting…");

    walker.boot(args.expr.unwrap());
}
