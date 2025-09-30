use quote::quote;
use syn::{DeriveInput, parse_macro_input};

mod node_impl;

#[proc_macro_derive(Location)]
pub fn location_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let cmd = match node_impl::LocationImpl::from(input) {
        Ok(cmd) => cmd.tokens(),
        Err(err) => return err.to_compile_error().into(),
    };

    quote!(#cmd).into()
}
