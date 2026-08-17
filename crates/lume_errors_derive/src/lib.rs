use darling::FromDeriveInput;
use parse::DiagnosticArgs;
use quote::ToTokens;
use syn::{DeriveInput, parse_macro_input};

mod fmt;
mod parse;
mod tokens;

#[proc_macro_derive(Diagnostic, attributes(diagnostic, message, primary_span, label, related))]
pub fn derive_diagnostic(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match DiagnosticArgs::from_derive_input(&input) {
        Ok(cmd) => cmd.into_token_stream().into(),
        Err(err) => err.write_errors().into(),
    }
}
