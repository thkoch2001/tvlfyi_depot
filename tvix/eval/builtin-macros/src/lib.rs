extern crate proc_macro;

use std::str::FromStr;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned, ToTokens};
use syn::parse::{Parse, Parser};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse2, parse_macro_input, parse_quote, parse_quote_spanned, Attribute, FnArg, GenericParam,
    Generics, Ident, Item, ItemMod, LitStr, Meta, NestedMeta, Pat, PatIdent, PatType, Token, Type,
    WherePredicate,
};

#[derive(Default)]
struct ModuleArguments {
    state_type: Option<Type>,
    state_generics: Option<Generics>,
}

/// Description of a single argument passed to a builtin
struct BuiltinArgument {
    /// The name of the argument, to be used in docstrings and error messages
    name: Ident,

    /// Type of the argument.
    ty: Box<Type>,

    /// Whether the argument should be forced before the underlying builtin
    /// function is called.
    strict: bool,

    /// Span at which the argument was defined.
    span: Span,
}

fn extract_docstring(attrs: &[Attribute]) -> Option<String> {
    // Rust docstrings are transparently written pre-macro expansion into an attribute that looks
    // like:
    //
    // #[doc = "docstring here"]
    //
    // Multi-line docstrings yield multiple attributes in order, which we assemble into a single
    // string below.

    #[allow(dead_code)]
    #[derive(Debug)]
    struct Docstring {
        eq: Token![=],
        doc: LitStr,
    }

    impl Parse for Docstring {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            Ok(Self {
                eq: input.parse()?,
                doc: input.parse()?,
            })
        }
    }

    attrs
        .iter()
        .filter(|attr| attr.path.get_ident().into_iter().any(|id| id == "doc"))
        .filter_map(|attr| parse2::<Docstring>(attr.tokens.clone()).ok())
        .map(|docstring| docstring.doc.value())
        .reduce(|mut fst, snd| {
            if snd.is_empty() {
                // An empty string represents a spacing newline that was added in the
                // original doc comment.
                fst.push_str("\n\n");
            } else {
                fst.push_str(&snd);
            }

            fst
        })
}

/// Parse arguments to the `builtins` macro itself, such as `#[builtins(state = Rc<State>)]`
/// or `#[builtins(state(Rc<T>, T))]` or `#[builtins(state(Rc<T>, T, T: Clone))]`
fn parse_module_args(args: TokenStream) -> ModuleArguments {
    if args.is_empty() {
        return Default::default();
    }

    let meta: Meta = syn::parse(args).expect("could not parse arguments to `builtins`-attribute");
    let (state_type_lit, state_generics) = match meta {
        Meta::NameValue(nv) => {
            // In this case, this is the simple state = ... case.
            // There can be no type parameters here.

            if nv.path.get_ident().unwrap() != "state" {
                return Default::default();
            }

            (nv.lit, None)
        }
        Meta::List(xs) => {
            // In this case, this is the `state(state type, type parameters, where clauses)`
            // case.

            if xs.nested.len() < 2 {
                panic!("use `state = ...` if you don't specify any type parameter");
            }

            let state_type = if let NestedMeta::Lit(ref lit) = xs.nested[0] {
                lit.clone()
            } else {
                panic!("state attribute must be a literal");
            };

            let type_params = if let NestedMeta::Lit(ref lit) = xs.nested[1] {
                if let syn::Lit::Str(tts) = lit {
                    tts.clone()
                } else {
                    panic!("state type parameters must be a string literal");
                }
            } else {
                panic!("state type parameters must be a literal");
            };

            let tokens = TokenStream::from_str(&type_params.value())
                .expect("Failed to re-interpret the type params as token stream");
            let gparam_parser = Punctuated::<GenericParam, Token![,]>::parse_terminated;
            let generic_params = gparam_parser.parse(tokens).expect(
                "state type parameters must be a list of generics parameter separated by ,",
            );

            let where_predicates = if xs.nested.len() > 2 {
                if let NestedMeta::Lit(lit) = &xs.nested[2] {
                    if let syn::Lit::Str(tts) = lit {
                        Some(
                                Punctuated::<WherePredicate, Token![,]>::parse_terminated.parse(
                                    TokenStream::from_str(tts.value().as_str())
                                    .expect("failed to re-interpret the trait bounds as token stream")
                                ).expect("state trait bounds must be a list of where predicates separated by,")
                            )
                    } else {
                        panic!("state trait bounds must be a string literal");
                    }
                } else {
                    panic!("where predicate must be a literal")
                }
            } else {
                None
            };

            let generics = Generics {
                lt_token: None,
                params: generic_params,
                gt_token: None,
                where_clause: where_predicates.map(|ps| syn::WhereClause {
                    where_token: syn::token::Where(Span::call_site()),
                    predicates: ps,
                }),
            };

            (state_type, Some(generics))
        }
        _ => panic!(
            "arguments to `builtins`-attribute must be of the form `name = value` or `name(...)`"
        ),
    };

    let state_type = if let syn::Lit::Str(type_name) = state_type_lit {
        syn::parse_str(&type_name.value()).expect("failed to parse builtins state type")
    } else {
        panic!("state attribute must be a quoted Rust type");
    };

    ModuleArguments {
        state_type: Some(state_type),
        state_generics,
    }
}

/// Mark the annotated module as a module for defining Nix builtins.
///
/// An optional type definition may be specified as an argument (e.g. `#[builtins(Rc<State>)]`),
/// which will add a parameter to the `builtins` function of that type which is passed to each
/// builtin upon instantiation. Using this, builtins that close over some external state can be
/// written.
///
/// The type of each function is rewritten to receive a `Vec<Value>`, containing each `Value`
/// argument that the function receives. The body of functions is accordingly rewritten to "unwrap"
/// values from this vector and bind them to the correct names, so unless a static error occurs this
/// transformation is mostly invisible to users of the macro.
///
/// A function `fn builtins() -> Vec<Builtin>` will be defined within the annotated module,
/// returning a list of [`tvix_eval::Builtin`] for each function annotated with the `#[builtin]`
/// attribute within the module. If a `state` type is specified, the `builtins` function will take a
/// value of that type.
///
/// Each invocation of the `#[builtin]` annotation within the module should be passed a string
/// literal for the name of the builtin.
///
/// # Examples
/// ```ignore
/// # use tvix_eval;
/// # use tvix_eval_builtin_macros::builtins;
///
/// #[builtins]
/// mod builtins {
///     use tvix_eval::{GenCo, ErrorKind, Value};
///
///     #[builtin("identity")]
///     pub async fn builtin_identity(co: GenCo, x: Value) -> Result<Value, ErrorKind> {
///         Ok(x)
///     }
///
///     // Builtins can request their argument not be forced before being called by annotating the
///     // argument with the `#[lazy]` attribute
///
///     #[builtin("tryEval")]
///     pub async fn builtin_try_eval(co: GenCo, #[lazy] x: Value) -> Result<Value, ErrorKind> {
///         todo!()
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn builtins(args: TokenStream, item: TokenStream) -> TokenStream {
    let mut module = parse_macro_input!(item as ItemMod);

    // parse the optional state type, which users might want to pass to builtins
    let module_args = parse_module_args(args);

    let (_, items) = match &mut module.content {
        Some(content) => content,
        None => {
            return (quote_spanned!(module.span() =>
                compile_error!("Builtin modules must be defined in-line")
            ))
            .into();
        }
    };

    let mut builtins = vec![];
    for item in items.iter_mut() {
        if let Item::Fn(f) = item {
            if let Some(builtin_attr_pos) = f
                .attrs
                .iter()
                .position(|attr| attr.path.get_ident().iter().any(|id| *id == "builtin"))
            {
                let builtin_attr = f.attrs.remove(builtin_attr_pos);
                let name: LitStr = match builtin_attr.parse_args() {
                    Ok(args) => args,
                    Err(err) => return err.into_compile_error().into(),
                };

                if f.sig.inputs.len() <= 1 {
                    return (quote_spanned!(
                        f.sig.inputs.span() =>
                            compile_error!("Builtin functions must take at least two arguments")
                    ))
                    .into();
                }

                // Inspect the first argument to determine if this function is
                // taking the state parameter.
                // TODO(tazjin): add a test in //tvix/eval that covers this
                let mut captures_state = false;
                if let FnArg::Typed(PatType { pat, .. }) = &f.sig.inputs[0] {
                    if let Pat::Ident(PatIdent { ident, .. }) = pat.as_ref() {
                        if *ident == "state" {
                            if module_args.state_type.is_none() {
                                panic!("builtin captures a `state` argument, but no state type was defined");
                            }

                            captures_state = true;
                        }
                    }
                }

                let mut rewritten_args = std::mem::take(&mut f.sig.inputs)
                    .into_iter()
                    .collect::<Vec<_>>();

                // Split out the value arguments from the static arguments.
                let split_idx = if captures_state { 2 } else { 1 };
                let value_args = rewritten_args.split_off(split_idx);

                let builtin_arguments = value_args
                    .into_iter()
                    .map(|arg| {
                        let span = arg.span();
                        let mut strict = true;
                        let (name, ty) = match arg {
                            FnArg::Receiver(_) => {
                                return Err(quote_spanned!(span => {
                                    compile_error!("unexpected receiver argument in builtin")
                                }))
                            }
                            FnArg::Typed(PatType {
                                mut attrs, pat, ty, ..
                            }) => {
                                attrs.retain(|attr| {
                                    attr.path.get_ident().into_iter().any(|id| {
                                        if id == "lazy" {
                                            strict = false;
                                            false
                                        } else {
                                            true
                                        }
                                    })
                                });
                                match pat.as_ref() {
                                    Pat::Ident(PatIdent { ident, .. }) => {
                                        (ident.clone(), ty.clone())
                                    }
                                    _ => panic!("ignored value parameters must be named, e.g. `_x` and not just `_`"),
                                }
                            }
                        };

                        Ok(BuiltinArgument {
                            strict,
                            span,
                            name,
                            ty,
                        })
                    })
                    .collect::<Result<Vec<BuiltinArgument>, _>>();

                let builtin_arguments = match builtin_arguments {
                    Err(err) => return err.into(),

                    // reverse argument order, as they are popped from the stack
                    // slice in opposite order
                    Ok(args) => args,
                };

                // Rewrite the argument to the actual function to take a
                // `Vec<Value>`, which is then destructured into the
                // user-defined values in the function header.
                let sig_span = f.sig.span();
                rewritten_args.push(parse_quote_spanned!(sig_span=> mut values: Vec<Value>));
                f.sig.inputs = rewritten_args.into_iter().collect();

                // Rewrite the body of the function to do said argument forcing.
                //
                // This is done by creating a new block for each of the
                // arguments that evaluates it, and wraps the inner block.
                for arg in &builtin_arguments {
                    let block = &f.block;
                    let ty = &arg.ty;
                    let ident = &arg.name;

                    if arg.strict {
                        f.block = Box::new(parse_quote_spanned! {arg.span=> {
                            let #ident: #ty = tvix_eval::generators::request_force(&co, values.pop()
                              .expect("Tvix bug: builtin called with incorrect number of arguments")).await;

                            #block
                        }});
                    } else {
                        f.block = Box::new(parse_quote_spanned! {arg.span=> {
                            let #ident: #ty = values.pop()
                              .expect("Tvix bug: builtin called with incorrect number of arguments");

                            #block
                        }})
                    }
                }

                let fn_name = f.sig.ident.clone();
                let arg_count = builtin_arguments.len();
                let docstring = match extract_docstring(&f.attrs) {
                    Some(docs) => quote!(Some(#docs)),
                    None => quote!(None),
                };

                if captures_state {
                    builtins.push(quote_spanned! { builtin_attr.span() => {
                        let inner_state = state.clone();
                        tvix_eval::Builtin::new(
                            #name,
                            #docstring,
                            #arg_count,
                            move |values| Gen::new(|co| tvix_eval::generators::pin_generator(#fn_name(inner_state.clone(), co, values))),
                        )
                    }});
                } else {
                    builtins.push(quote_spanned! { builtin_attr.span() => {
                        tvix_eval::Builtin::new(
                            #name,
                            #docstring,
                            #arg_count,
                            |values| Gen::new(|co| tvix_eval::generators::pin_generator(#fn_name(co, values))),
                        )
                    }});
                }
            }
        }
    }

    if let Some(state_type) = module_args.state_type {
        if let Some(state_generics) = module_args.state_generics {
            let (_, type_generics, where_clause) = state_generics.split_for_impl();
            items.push(parse_quote! {
                pub fn builtins #type_generics (state: #state_type) #where_clause -> Vec<(&'static str, Value)> {
                    vec![#(#builtins),*].into_iter().map(|b| (b.name(), Value::Builtin(b))).collect()
                }
            });
        } else {
            items.push(parse_quote! {
                pub fn builtins(state: #state_type) -> Vec<(&'static str, Value)> {
                    vec![#(#builtins),*].into_iter().map(|b| (b.name(), Value::Builtin(b))).collect()
                }
            });
        }
    } else {
        items.push(parse_quote! {
            pub fn builtins() -> Vec<(&'static str, Value)> {
                vec![#(#builtins),*].into_iter().map(|b| (b.name(), Value::Builtin(b))).collect()
            }
        });
    }

    module.into_token_stream().into()
}
