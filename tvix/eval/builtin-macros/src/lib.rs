extern crate proc_macro;

use std::sync::Mutex;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned, ToTokens};
use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{bracketed, parse_macro_input, Ident, ItemFn, LitBool, LitStr, Token};

struct Builtin {
    name: String,
    strictness: Vec<bool>,
    num_args: usize,
    fn_name: String,
}

impl ToTokens for Builtin {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let Builtin {
            name,
            strictness,
            num_args,
            fn_name,
        } = self;

        let strictness = quote!(&[#(#strictness),*]);
        let fn_name = Ident::new(&fn_name, Span::call_site());
        let args = (0..*num_args)
            .map(|n| Ident::new(&format!("arg_{n}"), Span::call_site()))
            .collect::<Vec<_>>();
        let mut reversed_args = args.clone();
        reversed_args.reverse();

        (quote! {
            crate::value::Builtin::new(
                #name,
                #strictness,
                |mut args: Vec<Value>, vm: &mut VM| {
                    #(let #reversed_args = args.pop().unwrap();)*
                    #fn_name(vm, #(#args),*)
                }
            )
        })
        .to_tokens(tokens)
    }
}

static BUILTINS: Mutex<Vec<Builtin>> = Mutex::new(Vec::new());

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

/// Mark the annotated function as a Nix builtin, registering it for later inclusion in the vector
/// returned from [`builtins`]
///
/// Builtin functions must take an `&mut VM` and at least one argument of type `Value`. This
/// attribute should be given the name of the builtin function as a string literal, and an array
/// containing booleans representing the strictness of the arguments
///
/// # Examples
///
/// ```
/// # use tvix_eval::{ErrorKind, Value, VM};
/// # use tvix_eval_builtin_macros::{builtin, builtins};
/// // Register a single builtin named `identity`, with a single lazy argument
///
/// #[builtin("identity", [false])]
/// pub fn builtin_identity(_vm: &mut VM, x: Value) -> Result<Value, ErrorKind> {
///     Ok(x)
/// }
/// ```
#[proc_macro_attribute]
pub fn builtin(args: TokenStream, item: TokenStream) -> TokenStream {
    let BuiltinArgs { name, strictness } = parse_macro_input!(args as BuiltinArgs);
    let item = parse_macro_input!(item as ItemFn);

    let fn_name = item.sig.ident.clone();

    if item.sig.inputs.len() <= 1 {
        return (quote_spanned!(
            item.sig.inputs.span() =>
                compile_error!("Builtin functions must take at least two arguments")
        ))
        .into();
    }

    let n_args = item.sig.inputs.len() - 1;

    BUILTINS.lock().unwrap().push(Builtin {
        name: name.value(),
        strictness: strictness.into_iter().map(|val| val.value()).collect(),
        num_args: n_args,
        fn_name: fn_name.to_string(),
    });

    item.into_token_stream().into()
}

/// Construct a vector of all the builtins previously registered with [`builtins`]
#[proc_macro]
pub fn builtins(_args: TokenStream) -> TokenStream {
    let builtins = BUILTINS.lock().unwrap();
    (quote! {{ vec![#(#builtins),*] }}).into()
}
