use tvix_eval::{EvalIO, GlobalsMap, Value};
use tvix_glue::{configure_nix_path, tvix_store_io::TvixStoreIO};

use crate::errors::EvaluationError;

use super::derivation::DerivationInfo;
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

    pub fn boot(&self, expr: String) {
        assert!(self.globals.is_some(), "Walker has not been initialized");
        let result = self.evaluation_build().evaluate(expr, None);
        self.recurse_for_derivations(result.value.unwrap(), result.span.unwrap(), None);
    }

    #[tracing::instrument(skip_all, fields(path))]
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
