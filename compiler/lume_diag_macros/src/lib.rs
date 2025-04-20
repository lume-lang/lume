use diagnostic::Diagnostic;
use quote::quote;
use syn::{DeriveInput, parse_macro_input};

mod args;
mod diagnostic;
mod fmt;

#[proc_macro_derive(Diagnostic, attributes(diagnostic, span, label, source))]
pub fn derive_diagnostic(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let cmd = match Diagnostic::from(input) {
        Ok(cmd) => cmd.tokens().unwrap_or_else(|err| return err.to_compile_error().into()),
        Err(err) => return err.to_compile_error().into(),
    };

    quote!(#cmd).into()
}
