use tvix_eval::{EvalIO, GlobalsMap, Value};
use tvix_glue::{configure_nix_path, tvix_store_io::TvixStoreIO};

use crate::errors::EvaluationError;

use super::derivation::DerivationInfo;
use color_eyre::{eyre::OptionExt, Result};
use std::rc::Rc;

pub struct Walker {
    pub nix_search_path: Option<String>,
    pub force_recurse: bool,
    pub include_meta: bool,
    pub globals: Option<Rc<GlobalsMap>>,
    pub store: Rc<TvixStoreIO>,
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
    pub fn setup(&mut self) {
        let evaluator = self.evaluation_build();
        self.globals = Some(evaluator.globals());
    }

    fn shallow_force(&self, value: Value, span: codemap::Span) -> tvix_eval::EvaluationResult {
        self.evaluation_build().shallow_force(value, span)
    }

    fn deep_force(&self, value: Value, span: codemap::Span) -> tvix_eval::EvaluationResult {
        self.evaluation_build().deep_force(value, span)
    }

    fn is_derivation(&self, value: &Value, span: codemap::Span) -> bool {
        match value {
            Value::Attrs(attrs) => {
                // TODO: force the type?
                attrs.select("type").is_some_and(|r#type| {
                    self.shallow_force(r#type.clone(), span)
                        .value
                        .map(|v| {
                            debug_assert!(
                                !matches!(v, Value::Thunk(_)),
                                "type information is still a thunk after forcing!"
                            );
                            v.to_str()
                        })
                        .is_some_and(|r#type| r#type.is_ok_and(|r#type| r#type == "derivation"))
                })
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
        value: &Value,
        span: codemap::Span,
    ) -> Result<DerivationInfo> {
        match value {
            Value::Attrs(attrs) => {
                let drv_path = attrs
                    .select_required("drvPath")
                    .map_err(|_| EvaluationError::MissingAttribute("drvPath"))?;
                let out_path = attrs
                    .select_required("outPath")
                    .map_err(|_| EvaluationError::MissingAttribute("outPath"))?;
                let name = attrs
                    .select_required("name")
                    .map_err(|_| EvaluationError::MissingAttribute("name"))?;
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
                )?)
            }
            _ => Err(EvaluationError::MalformedDerivation(value.type_of()).into()),
        }
    }

    pub fn boot(&self, expr: String) -> Result<()> {
        assert!(self.globals.is_some(), "Walker has not been initialized");
        let result = self.evaluation_build().evaluate(expr, None);
        self.recurse_for_derivations(result.value.unwrap(), result.span.unwrap(), None)?;
        Ok(())
    }

    #[tracing::instrument(skip_all, fields(path))]
    fn recurse_for_derivations(
        &self,
        toplevel: Value,
        span: codemap::Span,
        path: Option<String>,
    ) -> Result<()> {
        if let Value::Attrs(attrs) = toplevel {
            let recursable_attrs = attrs
                .iter_sorted()
                .filter(|(key, _)| **key == "recurseForDerivations");

            let mut should_recurse = attrs.keys().any(|key| *key == "recurseForDerivations")
                || path.is_none()
                || self.force_recurse;

            for (_, value) in recursable_attrs {
                let evaluated = self.shallow_force(value.clone(), span);
                let recurse_for_derivations = evaluated
                    .value
                    .ok_or_eyre("Failed to force `recurseForDerivations`")?;

                should_recurse = recurse_for_derivations
                    .as_bool()
                    .unwrap_or(self.force_recurse);
            }

            if should_recurse {
                let non_recursable_attrs = attrs
                    .into_iter_sorted()
                    .filter(|(key, _)| *key != "recurseForDerivations");

                for (key, value) in non_recursable_attrs {
                    let forced_result = self.shallow_force(value, span);
                    let attr_path = path.as_ref().map_or_else(
                        || key.ident_str().to_string(),
                        |p| format!("{}.{}", p, key.ident_str()),
                    );

                    match forced_result.value {
                        // A derivation was found, print its information.
                        Some(ref value) if self.is_derivation(value, span) && path.is_some() => {
                            match self.grab_derivation(path.clone().unwrap(), value, span) {
                                Ok(info) => println!("{}", serde_json::to_string(&info)?),
                                Err(err) => eprintln!(
                                    "{}",
                                    serde_json::to_string(&serde_json::json!({
                                        "attr": key.ident_str(),
                                        "attrPath": attr_path,
                                        "error": err.to_string()
                                    }))?
                                ),
                            }
                        }
                        // Keep recursing further.
                        Some(value) => {
                            self.recurse_for_derivations(
                                value,
                                forced_result.span.unwrap(),
                                Some(attr_path),
                            )?;
                        }
                        // An error of some sort occured.
                        _ => {
                            println!(
                                "{}",
                                serde_json::to_string(&serde_json::json!({
                                    "attr": key.ident_str(),
                                    "attrPath": attr_path,
                                    "error": "An error occured while shallow forcing",
                                }))?
                            );
                        }
                    }
                }
            }
        }
        Ok(())
    }
}
