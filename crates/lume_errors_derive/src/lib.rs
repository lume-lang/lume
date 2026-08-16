use diagnostic::AttrDiagnostic;
use quote::quote;
use syn::{DeriveInput, parse_macro_input};

mod args;
mod diagnostic;
mod fields;
mod fmt;
mod tokens;

#[proc_macro_derive(Diagnostic, attributes(diagnostic, span, label, related))]
pub fn derive_diagnostic(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let cmd = match AttrDiagnostic::from(input) {
        Ok(cmd) => cmd.tokens().unwrap_or_else(|err| err.to_compile_error()),
        Err(err) => return err.to_compile_error().into(),
    };

    quote!(#cmd).into()
}
