extern crate proc_macro;

use comrak::{markdown_to_html, ComrakOptions};
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, LitStr};

#[proc_macro]
pub fn markdown(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    let mut options = ComrakOptions::default();
    options.extension.strikethrough = true;
    options.extension.tagfilter = true;
    options.extension.table = true;
    options.extension.autolink = true;

    let rendered_html = markdown_to_html(&input.value(), &options);

    let tokens = quote! {
        yew::virtual_dom::VNode::VRaw(yew::virtual_dom::VRaw {
            html: #rendered_html.into(),
        })
    };

    tokens.into()
}
