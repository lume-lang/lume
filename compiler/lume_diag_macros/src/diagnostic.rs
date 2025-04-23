use crate::{args::DiagnosticArg, fmt::FormattedMessage};
use lume_diag::Severity;
use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

#[derive(Debug)]
pub struct Diagnostic {
    pub ident: Ident,
    pub args: Vec<DiagnosticArg>,
}

impl Diagnostic {
    pub fn new(ident: Ident) -> Self {
        Diagnostic {
            ident,
            args: Vec::new(),
        }
    }

    pub fn from(input: syn::DeriveInput) -> syn::Result<Self> {
        let mut diagnostic = Diagnostic::new(input.ident);

        for attr in &input.attrs {
            let mut attr_arg = DiagnosticArg::parse_attribute(attr)?;
            diagnostic.args.append(&mut attr_arg);
        }

        if let syn::Data::Struct(syn::DataStruct { fields, .. }) = &input.data {
            for field in fields {
                let field_attr_arg = match DiagnosticArg::parse_field(field)? {
                    Some(attr) => attr,
                    None => continue,
                };

                diagnostic.args.push(field_attr_arg);
            }
        }

        // Verify that all required attributes are given.
        diagnostic.verify()?;

        Ok(diagnostic)
    }

    /// Verifies the diagnostic attribute.
    pub fn verify(&self) -> syn::Result<()> {
        self.message()?;

        Ok(())
    }

    pub fn tokens(&self) -> syn::Result<TokenStream> {
        let name = &self.ident;
        let message_block = self.message_block()?;
        let code_block = self.code_block()?;
        let help_block = self.help_block()?;
        let span_block = self.span_block()?;
        let labels_block = self.labels_block()?;
        let source_block = self.source_block()?;
        let severity_block = self.severity_block()?;

        let stream = quote! {
            impl lume_diag::Diagnostic for #name {
                #message_block
                #code_block
                #help_block
                #span_block
                #labels_block
                #source_block
                #severity_block
            }
        };

        Ok(stream)
    }

    /// Gets the value of the `message` attribute, if any was given. If not, raises
    /// an error for the user.
    pub fn message(&self) -> syn::Result<String> {
        let arg = self.args.iter().find(|arg| matches!(arg, DiagnosticArg::Message(_)));

        match arg {
            Some(DiagnosticArg::Message(message)) => Ok(message.clone()),
            _ => Err(self.err("No error message provided. Please use `#[diagnostic(message = \"...\")]`")),
        }
    }

    /// Gets the value of the `code` attribute, if any was given. If not, returns `None`.
    pub fn code(&self) -> Option<String> {
        let arg = self.args.iter().find(|arg| matches!(arg, DiagnosticArg::Code(_)));

        match arg {
            Some(DiagnosticArg::Code(code)) => Some(code.clone()),
            _ => None,
        }
    }

    /// Gets the value(s) of the `help` attribute(s), if any was given. If not, returns `None`.
    pub fn help(&self) -> Option<Vec<String>> {
        let args = self
            .args
            .iter()
            .filter_map(|arg| {
                if let DiagnosticArg::Help(help) = arg {
                    Some(help.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<String>>();

        if args.is_empty() { None } else { Some(args) }
    }

    /// Gets the source code of the diagnostic, if any was given. If not, returns `None`.
    pub fn span(&self) -> Option<Ident> {
        let arg = self.args.iter().find(|arg| matches!(arg, DiagnosticArg::Span(_)));

        match arg {
            Some(DiagnosticArg::Span(span)) => Some(span.clone()),
            _ => None,
        }
    }

    /// Gets the severity of the diagnostic, if any was given. If not, returns `None`.
    pub fn severity(&self) -> Option<Severity> {
        let arg = self.args.iter().find(|arg| matches!(arg, DiagnosticArg::Severity(_)));

        match arg {
            Some(DiagnosticArg::Severity(severity)) => Some(*severity),
            _ => None,
        }
    }

    /// Gets the value(s) of the `labels` attribute(s), if any was given. If not, returns `None`.
    pub fn labels(&self) -> Option<Vec<(String, Ident)>> {
        let args = self
            .args
            .iter()
            .filter_map(|arg| {
                if let DiagnosticArg::Label(label, ident) = arg {
                    Some((label.clone(), ident.clone()))
                } else {
                    None
                }
            })
            .collect::<Vec<(String, Ident)>>();

        if args.is_empty() { None } else { Some(args) }
    }

    /// Creates the implementation block for the `message` trait function.
    fn message_block(&self) -> syn::Result<TokenStream> {
        let message = self.message()?;
        let lit = syn::LitStr::new(&message, proc_macro2::Span::call_site());

        let formatted = FormattedMessage::expand(lit);

        let stream = quote! {
            fn message<'a>(&'a self) -> String {
                #formatted
            }
        };

        Ok(stream)
    }

    /// Creates the implementation block for the `code` trait function.
    fn code_block(&self) -> syn::Result<TokenStream> {
        let stream = if let Some(code) = self.code() {
            quote! {
                fn code<'a>(&'a self) -> Option<&'a str> {
                    Some(#code)
                }
            }
        } else {
            TokenStream::new()
        };

        Ok(stream)
    }

    /// Creates the implementation block for the `help` trait function.
    fn help_block(&self) -> syn::Result<TokenStream> {
        let stream = if let Some(help) = self.help() {
            let help_idents = help
                .into_iter()
                .map(|h| {
                    let lit = syn::LitStr::new(&h, proc_macro2::Span::call_site());

                    FormattedMessage::expand(lit)
                })
                .collect::<Vec<TokenStream>>();

            quote! {
                fn help<'a>(&'a self) -> Option<Vec<String>> {
                    Some(vec![ #(#help_idents),* ])
                }
            }
        } else {
            TokenStream::new()
        };

        Ok(stream)
    }

    /// Creates the implementation block for the `span` trait function.
    fn span_block(&self) -> syn::Result<TokenStream> {
        let stream = if let Some(span) = self.span() {
            quote! {
                fn span<'a>(&'a self) -> Option<&'a dyn ::lume_diag::source::Source> {
                    let span = (&self.#span) as &'a dyn ::lume_diag::source::Source;

                    Some(span)
                }
            }
        } else {
            TokenStream::new()
        };

        Ok(stream)
    }

    /// Creates the implementation block for the `labels` trait function.
    fn labels_block(&self) -> syn::Result<TokenStream> {
        let span = if let Some(span) = self.span() {
            span
        } else {
            return Ok(TokenStream::new());
        };

        let stream = if let Some(labels) = self.labels() {
            let label_pairs = labels
                .into_iter()
                .map(|(label, ident)| {
                    let lit_str = syn::LitStr::new(&label, proc_macro2::Span::call_site());
                    let formatted_str = FormattedMessage::expand(lit_str);

                    quote! { (#formatted_str, self.#ident.clone()) }
                })
                .collect::<Vec<TokenStream>>();

            quote! {
                fn labels<'a>(&'a self) -> Option<Vec<Box<::lume_diag::Label<'a>>>> {
                    let span = &self.#span;
                    let labels = vec![ #(#label_pairs),* ];

                    let labels = labels
                        .into_iter()
                        .map(|(label, range)| Box::new(::lume_diag::Label::new(span, range, label.into())))
                        .collect::<Vec<Box<::lume_diag::Label<'a>>>>();

                    Some(labels)
                }
            }
        } else {
            TokenStream::new()
        };

        Ok(stream)
    }

    /// Creates the implementation block for the `source` trait function.
    fn source_block(&self) -> syn::Result<TokenStream> {
        let arg = self.args.iter().find(|arg| matches!(arg, DiagnosticArg::Source(_)));
        let source = match arg {
            Some(DiagnosticArg::Source(source)) => source.clone(),
            _ => return Ok(TokenStream::new()),
        };

        let stream = quote! {
            fn source<'a>(&'a self) -> Option<&'a dyn std::error::Error> {
                Some(&self.#source)
            }
        };

        Ok(stream)
    }

    /// Creates the implementation block for the `severity` trait function.
    fn severity_block(&self) -> syn::Result<TokenStream> {
        let stream = if let Some(severity) = self.severity() {
            let path: TokenStream = match severity {
                Severity::Error => quote!(::lume_diag::Severity::Error),
                Severity::Warning => quote!(::lume_diag::Severity::Warning),
                Severity::Info => quote!(::lume_diag::Severity::Info),
                Severity::Help => quote!(::lume_diag::Severity::Help),
                Severity::Note => quote!(::lume_diag::Severity::Note),
            };

            quote! {
                fn severity<'a>(&'a self) -> ::lume_diag::Severity {
                    #path
                }
            }
        } else {
            TokenStream::new()
        };

        Ok(stream)
    }

    fn err(&self, message: &'static str) -> syn::Error {
        syn::Error::new_spanned(&self.ident, message)
    }
}
