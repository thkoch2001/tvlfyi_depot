use proc_macro::{self, TokenStream};
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, Data, DeriveInput, Ident, Index};

fn empty_impl(ident: Ident) -> TokenStream {
    (quote!(
        impl PackedEncode for #ident {
            #[inline]
            fn push(&self, _out: &mut Vec<u8>) {}

            #[inline]
            unsafe fn read(data: &[u8]) -> (Self, usize) {
                (#ident, 0)
            }
        }
    ))
    .into()
}

#[proc_macro_derive(PackedEncode)]
pub fn derive(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, data, .. } = parse_macro_input!(input);
    match data {
        Data::Enum(ed) => {
            let data = Ident::new("data", ident.span());

            let push_arms =
                ed.variants
                    .clone()
                    .into_iter()
                    .enumerate()
                    .map(|(discrim, variant)| {
                        assert!(variant.discriminant.is_none());
                        let discrim = u8::try_from(discrim).unwrap_or_else(|_| {
                            panic!("Cannot use more than {} variants", u8::MAX)
                        });

                        let label = variant.ident;
                        match variant.fields {
                            syn::Fields::Named(_) => todo!(),
                            syn::Fields::Unnamed(fields) => {
                                let field = fields
                                    .unnamed
                                    .iter()
                                    .enumerate()
                                    .map(|(i, f)| Ident::new(&format!("field{i}"), f.span()));
                                let push = field
                                    .clone()
                                    .map(|f| quote_spanned!(f.span() => #f.push(#data);));
                                quote_spanned! { label.span() =>
                                    Self::#label(#(#field,)*) => {
                                        #data.push(#discrim);
                                        #(#push)*
                                    }
                                }
                            }
                            syn::Fields::Unit => quote!(
                                Self::#label => {
                                    #data.push(#discrim);
                                    return;
                                }
                            ),
                        }
                    });

            let data = Ident::new("data", ident.span());
            let bytes_read = Ident::new("bytes_read", ident.span());
            let read_arms = ed
                .variants
                .into_iter()
                .enumerate()
                .map(|(discrim, variant)| {
                    let discrim = u8::try_from(discrim)
                        .unwrap_or_else(|_| panic!("Cannot use more than {} variants", u8::MAX));
                    let span = variant.span();
                    let label = variant.ident;
                    let body = match variant.fields {
                        syn::Fields::Named(_) => quote!(todo!()),
                        syn::Fields::Unnamed(fields) => {
                            let field_type = fields.unnamed.into_iter().map(|f| f.ty);
                            quote_spanned! { label.span() =>
                                Self::#label(#({
                                    let (val, size) = <#field_type as PackedEncode>::read(data);
                                    #data = &#data[size..];
                                    #bytes_read += size;
                                    val
                                })*)
                            }
                        }
                        syn::Fields::Unit => quote!(Self::#label),
                    };
                    quote_spanned!(span => #discrim => { #body })
                });

            (quote!(
                impl PackedEncode for #ident {
                    #[inline]
                    fn push(&self, #data: &mut Vec<u8>) {
                        match self {
                            #(#push_arms)*
                        }
                    }

                    #[inline]
                    #[allow(unused_assignments)]
                    unsafe fn read(mut #data: &[u8]) -> (Self, usize) {
                        let discrim = #data[0];
                        let mut #bytes_read = 1;
                        #data = &#data[1..];
                        let res = match discrim {
                            #(#read_arms)*
                            other => {
                                if cfg!(debug_assertions) {
                                    panic!("Unexpected discriminant {other}")
                                } else {
                                    std::hint::unreachable_unchecked()
                                }
                            }
                        };
                        (res, #bytes_read)
                    }
                }
            ))
            .into()
        }
        Data::Struct(struct_data) => match struct_data.fields {
            syn::Fields::Unit => empty_impl(ident),
            syn::Fields::Named(fields) => {
                let field1 = fields
                    .named
                    .clone()
                    .into_iter()
                    .map(|f| f.ident.expect("Named fields"));
                let field2 = field1.clone();
                let field_type = fields.named.into_iter().map(|f| f.ty);
                (quote!(
                    impl PackedEncode for #ident {
                        #[inline]
                        fn push(&self, out: &mut Vec<u8>) {
                            #(self.#field1.push(out);)*
                        }

                        #[inline]
                        unsafe fn read(mut data: &[u8]) -> (Self, usize) {
                            let mut total_size = 0;
                            let res = #ident {
                               #(#field2 : {
                                   let (val, size) = <#field_type as PackedEncode>::read(data);
                                   data = &data[size..];
                                   total_size += size;
                                   val
                               },)*
                            };
                            (res, total_size)
                        }
                    }
                ))
                .into()
            }
            syn::Fields::Unnamed(fields) => {
                let field = (0..fields.unnamed.len()).map(Index::from);
                let field_type = fields.unnamed.into_iter().map(|f| f.ty);
                (quote!(
                    impl PackedEncode for #ident {
                        #[inline]
                        fn push(&self, out: &mut Vec<u8>) {
                            #(self.#field.push(out);)*
                        }

                        #[inline]
                        unsafe fn read(mut data: &[u8]) -> (Self, usize) {
                            let mut total_size = 0;
                            let res = #ident(
                               #({
                                   let (val, size) = <#field_type as PackedEncode>::read(data);
                                   data = &data[size..];
                                   total_size += size;
                                   val
                               })*
                            );
                            (res, total_size)
                        }
                    }
                ))
                .into()
            }
        },
        Data::Union(_) => todo!(),
    }
}
