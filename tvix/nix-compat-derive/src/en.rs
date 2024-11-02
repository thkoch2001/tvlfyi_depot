use crate::internal::{Container, Context, Data, Field, Style, Variant};
use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use syn::{spanned::Spanned, DeriveInput, Generics, Path, Type};
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

            async fn serialize<'a, W>(
                &'a self,
                writer: &'a mut W,
            ) -> Result<(), W::Error> where
            W: #crate_path::nix_daemon::en::NixWrite + Send,
            {
                #body
            }
        }
    }
}
fn nix_serialize_body(cont: &Container) -> TokenStream {
    match &cont.data {
        Data::Struct(style, fields) => nix_serialize_struct(*style, fields),
        Data::Enum(variants) => nix_serialize_enum(variants),
    }
}
fn nix_serialize_struct(_style: Style, fields: &[Field<'_>]) -> TokenStream {
    let write_fields = fields.iter().map(|f| {
        let field = &f.member;
        let ty = f.ty;
        let write = quote_spanned! {
            ty.span()=>
                writer.write(&self.#field).await?;
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
        #(#write_fields)*
        Ok(())
    }
}

fn nix_serialize_variant(variant: &Variant<'_>) -> TokenStream {
    let ident = variant.ident;
    let write_fields = variant.fields.iter().map(|f| {
        let field = f.var_ident();
        let ty = f.ty;
        let write = quote_spanned! {
            ty.span()=>
                #field.serialize(writer).await?;
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
    let field_names = variant.fields.iter().map(|f| f.var_ident());
    let match_variant = match variant.style {
        Style::Struct => {
            quote! {
                Self::#ident { #(#field_names),* }
            }
        }
        Style::Tuple => {
            quote! {
                Self::#ident(#(#field_names),*)
            }
        }
        Style::Unit => quote!(Self::#ident),
    };
    quote! {
        #match_variant => {
            #(#write_fields)*
        },
    }
}

fn nix_serialize_enum(variants: &[Variant<'_>]) -> TokenStream {
    let match_variant = variants
        .iter()
        .map(|variant| nix_serialize_variant(variant));
    quote! {
        match self {
            #(#match_variant)*
        }
        Ok(())
    }
}
