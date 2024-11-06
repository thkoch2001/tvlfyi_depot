use internal::inputs::RemoteInput;
use proc_macro::TokenStream;
use syn::{parse_quote, DeriveInput};

mod de;
mod internal;
mod ser;

#[proc_macro_derive(NixDeserialize, attributes(nix))]
pub fn derive_nix_deserialize(item: TokenStream) -> TokenStream {
    let mut input = syn::parse_macro_input!(item as DeriveInput);
    let crate_path: syn::Path = parse_quote!(::nix_serialize);
    de::expand_nix_deserialize(crate_path, &mut input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro_derive(NixSerialize, attributes(nix))]
pub fn derive_nix_serialize(item: TokenStream) -> TokenStream {
    let mut input = syn::parse_macro_input!(item as DeriveInput);
    let crate_path: syn::Path = parse_quote!(::nix_serialize);
    ser::expand_nix_serialize(crate_path, &mut input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro]
pub fn nix_deserialize_remote(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as RemoteInput);
    let crate_path = parse_quote!(::nix_serialize);
    de::expand_nix_deserialize_remote(crate_path, &input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro]
pub fn nix_serialize_remote(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as RemoteInput);
    let crate_path = parse_quote!(::nix_serialize);
    ser::expand_nix_serialize_remote(crate_path, &input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
