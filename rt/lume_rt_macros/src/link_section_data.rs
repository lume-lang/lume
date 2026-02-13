use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{ForeignItemStatic, parse_macro_input};

const LUMEC_SEG_MACOS: &str = "__LUMEC";
const LUMEC_SEG_LINUX: &str = "__lumec";

#[derive(Debug, darling::FromMeta)]
#[darling(derive_syn_parse)]
struct LinkSectionDataArgs {
    section: String,
}

pub(crate) fn link_section_data(attr: TokenStream, item: TokenStream) -> TokenStream {
    let LinkSectionDataArgs { section } = match syn::parse(attr) {
        Ok(v) => v,
        Err(e) => return e.to_compile_error().into(),
    };

    let (link_name_start, link_name_end) = if cfg!(target_os = "macos") {
        (
            format!("section$start${LUMEC_SEG_MACOS}$__{section}"),
            format!("section$end${LUMEC_SEG_MACOS}$__{section}"),
        )
    } else if cfg!(target_os = "linux") {
        (
            format!("{LUMEC_SEG_LINUX}_{section}_start"),
            format!("{LUMEC_SEG_LINUX}_{section}_end"),
        )
    } else {
        unimplemented!("target platform not supported")
    };

    let input = parse_macro_input!(item as ForeignItemStatic);
    let ForeignItemStatic {
        attrs, vis, ident, ty, ..
    } = input;

    let section_start_ptr_ident = format_ident!("__{ident}_START");
    let section_end_ptr_ident = format_ident!("__{ident}_END");

    let section_start_ident = format_ident!("{ident}_START");
    let section_end_ident = format_ident!("{ident}_END");

    quote! {
        unsafe extern "C" {
            #[link_name = #link_name_start]
            static #section_start_ptr_ident: #ty;

            #[link_name = #link_name_end]
            static #section_end_ptr_ident: #ty;
        }

        #(#attrs)*
        #vis const #section_start_ident: *const #ty = &raw const #section_start_ptr_ident as *const #ty;

        #(#attrs)*
        #vis const #section_end_ident: *const #ty = &raw const #section_end_ptr_ident as *const #ty;

        #(#attrs)*
        #vis const #ident: *const #ty = #section_start_ident;
    }
    .into()
}
