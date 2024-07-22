use internal::inputs::RemoteInput;
use proc_macro::TokenStream;
use syn::{parse_quote, DeriveInput};

mod de;
mod internal;

#[cfg(not(feature="external"))]
#[proc_macro_derive(NixDeserialize, attributes(nix))]
pub fn derive_nix_deserialize(item: TokenStream) -> TokenStream {
    let mut input = syn::parse_macro_input!(item as DeriveInput);
    let nnixrs: syn::Path = parse_quote!(crate);
    de::expand_nix_deserialize(nnixrs, &mut input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[cfg(feature="external")]
#[proc_macro_derive(NixDeserialize, attributes(nix))]
pub fn derive_nix_deserialize(item: TokenStream) -> TokenStream {
    let mut input = syn::parse_macro_input!(item as DeriveInput);
    let nnixrs: syn::Path = parse_quote!(::nix_compat);
    de::expand_nix_deserialize(nnixrs, &mut input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}


#[cfg(not(feature="external"))]
#[proc_macro]
pub fn nix_deserialize_remote(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as RemoteInput);
    let crate_path = parse_quote!(crate);
    de::expand_nix_deserialize_remote(crate_path, &input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[cfg(feature="external")]
#[proc_macro]
pub fn nix_deserialize_remote(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as RemoteInput);
    let crate_path = parse_quote!(::nix_compat);
    de::expand_nix_deserialize_remote(crate_path, &input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
