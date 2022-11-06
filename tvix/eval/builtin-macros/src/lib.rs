extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned, ToTokens};
use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    bracketed, parse2, parse_macro_input, parse_quote, Attribute, FnArg, Ident, Item, ItemMod,
    LitBool, LitStr, Pat, PatIdent, PatType, Token,
};

struct BuiltinArgs {
    name: LitStr,
    strictness: Punctuated<LitBool, Token![,]>,
}

impl Parse for BuiltinArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        let _ = input.parse::<Token![,]>()?;
        let strictness_content;
        let _ = bracketed!(strictness_content in input);
        let strictness = strictness_content.parse_terminated(LitBool::parse)?;

        Ok(BuiltinArgs { name, strictness })
    }
}

fn extract_docstring(attrs: &[Attribute]) -> Option<LitStr> {
    // Rust docstrings are transparently written pre-macro expansion into an attribute that looks
    // like:
    //
    // #[doc = "docstring here"]

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
        .find_map(|attr| parse2::<Docstring>(attr.tokens.clone()).ok())
        .map(|docstring| docstring.doc)
}

/// Mark the annotated module as a module for defining Nix builtins.
///
/// A function `fn builtins() -> Vec<Builtin>` will be defined within the annotated module,
/// returning a list of [`tvix_eval::Builtin`] for each function annotated with the `#[builtin]`
/// attribute within the module.
///
/// Each invocation of the `#[builtin]` annotation within the module should be passed a string
/// literal for the name of the builtin, and an array of booleans representing the "strictness" of
/// the corresponding argument
///
/// # Examples
/// ```ignore
/// # use tvix_eval_builtin_macros::builtins;
/// # mod value {
/// #     pub use tvix_eval::Builtin;
/// # }
///
/// #[builtins]
/// mod builtins {
///     use tvix_eval::{ErrorKind, Value, VM};
///
///     #[builtin("identity", [true])]
///     pub fn builtin_identity(_vm: &mut VM, x: Value) -> Result<Value, ErrorKind> {
///         Ok(x)
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn builtins(_args: TokenStream, item: TokenStream) -> TokenStream {
    let mut module = parse_macro_input!(item as ItemMod);

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
                let BuiltinArgs { name, strictness } = match builtin_attr.parse_args() {
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

                let fn_name = f.sig.ident.clone();
                let num_args = f.sig.inputs.len() - 1;
                let args = (0..num_args)
                    .map(|n| Ident::new(&format!("arg_{n}"), Span::call_site()))
                    .collect::<Vec<_>>();
                let mut reversed_args = args.clone();
                reversed_args.reverse();

                if strictness.len() != num_args {
                    return (quote_spanned!(f.sig.inputs.span() =>
                       compile_error!("Function arguments and strictness map have different lengths")
                    )).into();
                }

                let builtin_arguments = strictness
                    .into_iter()
                    .zip(f.sig.inputs.iter().skip(1))
                    .map(|(strict, arg)| {
                        let name = match arg {
                            FnArg::Receiver(_) => {
                                return Err(quote_spanned!(arg.span() => {
                                    compile_error!("Unexpected receiver argument in builtin")
                                }))
                            }
                            FnArg::Typed(PatType { pat, .. }) => match pat.as_ref() {
                                Pat::Ident(PatIdent { ident, .. }) => ident.to_string(),
                                _ => "unknown".to_string(),
                            },
                        };
                        Ok(quote_spanned!(arg.span() => {
                            crate::internal::BuiltinArgument {
                                strict: #strict,
                                name: #name,
                            }
                        }))
                    })
                    .collect::<Result<Vec<_>, _>>();

                let builtin_arguments = match builtin_arguments {
                    Ok(args) => args,
                    Err(err) => return err.into(),
                };

                let docstring = match extract_docstring(&f.attrs) {
                    Some(docs) => quote!(Some(#docs)),
                    None => quote!(None),
                };

                builtins.push(quote_spanned! { builtin_attr.span() => {
                    crate::internal::Builtin::new(
                        #name,
                        &[#(#builtin_arguments),*],
                        #docstring,
                        |mut args: Vec<crate::Value>, vm: &mut crate::internal::VM| {
                            #(let #reversed_args = args.pop().unwrap();)*
                            #fn_name(vm, #(#args),*)
                        }
                    )
                }});
            }
        }
    }

    items.push(parse_quote! {
        pub fn builtins() -> Vec<crate::internal::Builtin> {
            vec![#(#builtins),*]
        }
    });

    module.into_token_stream().into()
}
