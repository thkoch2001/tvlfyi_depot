use proc_macro2::TokenStream;
use syn::{spanned::Spanned, DeriveInput, Generics, Path, Type};

use quote::{quote, quote_spanned};

use crate::internal::{Container, Context, Data, Field, Style};

pub fn expand_nix_serialize(nnixrs: Path, input: &mut DeriveInput) -> syn::Result<TokenStream> {
    let cx = Context::new();
    let cont = Container::from_ast(&cx, nnixrs, input);
    cx.check()?;
    let cont = cont.unwrap();

    let ty = cont.ident_type();
    let body = nix_serialize_body(&cont);
    let crate_path = cont.crate_path();

    Ok(nix_serialize_impl(
        crate_path,
        &ty,
        &cont.original.generics,
        body,
    ))
}

fn nix_serialize_impl(
    crate_path: &Path,
    ty: &Type,
    generics: &Generics,
    body: TokenStream,
) -> TokenStream {
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        #[automatically_derived]
        impl #impl_generics #crate_path::nix_daemon::en::NixSerialize for #ty #ty_generics
            #where_clause
        {
            #[allow(clippy::manual_async_fn)]
            fn serialize<W>(self, writer: &mut W) -> impl ::std::future::Future<Output=Result<(), W::Error>> + Send
                where W: ?Sized + #crate_path::nix_daemon::en::NixWrite + Send,
            {
                #body
            }
        }
    }
}

fn nix_serialize_body(cont: &Container) -> TokenStream {
    match &cont.data {
        Data::Struct(style, fields) => nix_serialize_struct(*style, fields),
        Data::Enum(_) => quote! {
            compile_error!("derive(NixSerialize) does not support enums (yet?).")
        },
    }
}

fn nix_serialize_struct(_style: Style, fields: &[Field<'_>]) -> TokenStream {
    let write_fields = fields.iter().map(|f| {
        let field = &f.member;
        let ty = f.ty;
        let write = quote_spanned! {
            ty.span()=>
                writer.write(self.#field).await?;
        };
        if let Some(version) = f.attrs.version.as_ref() {
            quote! {
                if (#version).contains(&writer.version().minor()) {
                    #write
                };
            }
        } else {
            quote! {
                #write;
            }
        }
    });
    quote! {
        #[allow(unused_assignments)]
        async move {
            #(#write_fields)*
            Ok(())
        }
    }
}
