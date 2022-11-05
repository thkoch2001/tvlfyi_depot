extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote_spanned, ToTokens};
use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    bracketed, parse_macro_input, parse_quote, Ident, Item, ItemMod, LitBool, LitStr, Token,
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
                let args = (0..(f.sig.inputs.len() - 1))
                    .map(|n| Ident::new(&format!("arg_{n}"), Span::call_site()))
                    .collect::<Vec<_>>();
                let mut reversed_args = args.clone();
                reversed_args.reverse();

                builtins.push(quote_spanned! { builtin_attr.span() => {
                    crate::value::Builtin::new(
                        #name,
                        &[#strictness],
                        |mut args: Vec<Value>, vm: &mut VM| {
                            #(let #reversed_args = args.pop().unwrap();)*
                            #fn_name(vm, #(#args),*)
                        }
                    )
                }});
            }
        }
    }

    items.push(parse_quote! {
        pub fn builtins() -> Vec<crate::value::Builtin> {
            vec![#(#builtins),*]
        }
    });

    module.into_token_stream().into()
}
