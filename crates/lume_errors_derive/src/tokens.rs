use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

use crate::args::DiagnosticArg;
use crate::diagnostic::{AttrDiagnostic, Severity};
use crate::fmt::FormattedMessage;

struct LabelIdent {
    severity: Option<Ident>,
    label: String,
    ident: Ident,
    has_source: bool,
}

impl AttrDiagnostic {
    pub(crate) fn tokens(&self) -> syn::Result<TokenStream> {
        let (impl_gen, ty_gen, where_clause) = &self.generics.split_for_impl();

        let name = &self.ident;
        let message_block = self.message_block()?;
        let code_block = self.code_block();
        let help_block = self.help_block();
        let labels_block = self.labels_block();
        let related_block = self.related_block();
        let cause_block = self.cause_block();
        let source_block = self.source_block();
        let severity_block = self.severity_block();

        let stream = quote! {
            impl #impl_gen ::lume_errors::Diagnostic for #name #ty_gen #where_clause {
                #message_block
                #code_block
                #help_block
                #labels_block
                #related_block
                #cause_block
                #source_block
                #severity_block
            }

            impl #impl_gen ::std::error::Error for #name #ty_gen #where_clause {}

            impl #impl_gen ::std::fmt::Display for #name #ty_gen #where_clause {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    write!(f, "{}", ::lume_errors::Diagnostic::message(self))
                }
            }
        };

        Ok(stream)
    }

    /// Gets the value of the `message` attribute, if any was given. If not,
    /// raises an error for the user.
    pub(crate) fn message(&self) -> syn::Result<String> {
        let arg = self.args.iter().find(|arg| matches!(arg, DiagnosticArg::Message(_)));

        match arg {
            Some(DiagnosticArg::Message(message)) => Ok(message.clone()),
            _ => Err(self.err("No error message provided. Please use `#[diagnostic(message = \"...\")]`")),
        }
    }

    /// Gets the value of the `code` attribute, if any was given. If not,
    /// returns `None`.
    fn code(&self) -> Option<String> {
        let arg = self.args.iter().find(|arg| matches!(arg, DiagnosticArg::Code(_)));

        match arg {
            Some(DiagnosticArg::Code(code)) => Some(code.clone()),
            _ => None,
        }
    }

    /// Gets the value(s) of the `help` attribute(s), if any was given. If not,
    /// returns `None`.
    fn help(&self) -> Option<Vec<String>> {
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

    /// Gets the source code of the diagnostic, if any was given. If not,
    /// returns `None`.
    fn span(&self) -> Option<Ident> {
        let arg = self.args.iter().find(|arg| matches!(arg, DiagnosticArg::Span(_)));

        match arg {
            Some(DiagnosticArg::Span(span)) => Some(span.clone()),
            _ => None,
        }
    }

    /// Gets the severity of the diagnostic, if any was given. If not, returns
    /// `None`.
    fn severity(&self) -> Option<Severity> {
        let arg = self.args.iter().find(|arg| matches!(arg, DiagnosticArg::Severity(_)));

        match arg {
            Some(DiagnosticArg::Severity(severity)) => Some(severity.clone()),
            _ => None,
        }
    }

    /// Gets the value(s) of the `labels` attribute(s), if any was given. If
    /// not, returns `None`.
    fn labels(&self) -> Option<Vec<LabelIdent>> {
        let args = self
            .args
            .iter()
            .filter_map(|arg| {
                if let DiagnosticArg::Label {
                    severity,
                    label,
                    ident,
                    has_source,
                } = arg
                {
                    Some(LabelIdent {
                        severity: severity.clone().map(|sev| sev.0),
                        label: label.clone(),
                        ident: ident.clone(),
                        has_source: *has_source,
                    })
                } else {
                    None
                }
            })
            .collect::<Vec<LabelIdent>>();

        if args.is_empty() { None } else { Some(args) }
    }

    /// Creates the implementation block for the `message` trait function.
    fn message_block(&self) -> syn::Result<TokenStream> {
        let message = self.message()?;
        let lit = syn::LitStr::new(&message, proc_macro2::Span::call_site());

        let formatted = FormattedMessage::expand(lit);

        let stream = quote! {
            fn message(&self) -> String {
                #formatted
            }
        };

        Ok(stream)
    }

    /// Creates the implementation block for the `code` trait function.
    fn code_block(&self) -> TokenStream {
        if let Some(code) = self.code() {
            quote! {
                fn code(&self) -> Option<Box<dyn std::fmt::Display + '_>> {
                    Some(Box::new(#code) as Box<dyn std::fmt::Display + '_>)
                }
            }
        } else {
            TokenStream::new()
        }
    }

    /// Creates the implementation block for the `help` trait function.
    fn help_block(&self) -> TokenStream {
        if let Some(help) = self.help() {
            let help_idents = help
                .into_iter()
                .map(|h| {
                    let lit = syn::LitStr::new(&h, proc_macro2::Span::call_site());

                    FormattedMessage::expand(lit)
                })
                .collect::<Vec<TokenStream>>();

            quote! {
                fn help(&self) -> Option<Box<dyn Iterator<Item = ::lume_errors::Help> + '_>> {
                    Some(Box::new(
                        vec![ #(#help_idents),* ]
                            .into_iter()
                            .map(|h| Into::<::lume_errors::Help>::into(h))
                    ))
                }
            }
        } else {
            TokenStream::new()
        }
    }

    /// Creates the implementation block for the `labels` trait function.
    fn labels_block(&self) -> TokenStream {
        if let Some(labels) = self.labels() {
            let label_pairs = labels
                .into_iter()
                .map(
                    |LabelIdent {
                         severity,
                         label,
                         ident,
                         has_source,
                     }| {
                        let lit_str = syn::LitStr::new(&label, proc_macro2::Span::call_site());
                        let formatted_str = FormattedMessage::expand(lit_str);

                        let method_name = severity.unwrap_or_else(|| Ident::new("new", proc_macro2::Span::call_site()));

                        if has_source {
                            quote! {
                                ::lume_errors::Label::#method_name(
                                    Into::<::lume_errors::SpanRange>::into(
                                        self.#ident.clone()
                                    ),
                                    #formatted_str
                                ).with_source(Some(
                                    Into::<std::sync::Arc<dyn ::lume_errors::Source>>::into(
                                        self.#ident.clone()
                                    )
                                ))
                            }
                        } else {
                            quote! {
                                ::lume_errors::Label::#method_name(self.#ident.clone(), #formatted_str)
                                    .with_source(::lume_errors::Diagnostic::source_code(self))
                            }
                        }
                    },
                )
                .collect::<Vec<TokenStream>>();

            quote! {
                fn labels(&self) -> Option<Box<dyn Iterator<Item = ::lume_errors::Label> + '_>> {
                    let labels = Box::new(vec![ #(#label_pairs),* ].into_iter());

                    Some(labels)
                }
            }
        } else {
            TokenStream::new()
        }
    }

    /// Creates the implementation block for the `related` trait function.
    fn related_block(&self) -> TokenStream {
        let arg = self.args.iter().find(|arg| matches!(arg, DiagnosticArg::Related(_, _)));

        let (related, collection) = match arg {
            Some(DiagnosticArg::Related(related, collection)) => (related.clone(), *collection),
            _ => return TokenStream::new(),
        };

        if collection {
            quote! {
                fn related(&self) -> Box<dyn Iterator<Item = &(dyn ::lume_errors::Diagnostic + Send + Sync)> + '_> {
                    Box::new(
                        self.#related
                            .iter()
                            .map(|e| e.as_ref() as &(dyn ::lume_errors::Diagnostic + Send + Sync)),
                    )
                }
            }
        } else {
            quote! {
                fn related(&self) -> Box<dyn Iterator<Item = &(dyn ::lume_errors::Diagnostic + Send + Sync)> + '_> {
                    let related: &(dyn lume_errors::Diagnostic + Send + Sync) =
                        (&self.#related as &lume_errors::Error).as_ref();

                    let iter = std::iter::once(related)
                        as std::iter::Once<&(dyn ::lume_errors::Diagnostic + Send + Sync)>;

                    Box::new(iter)
                }
            }
        }
    }

    /// Creates the implementation block for the `cause` trait function.
    fn cause_block(&self) -> TokenStream {
        let arg = self.args.iter().find(|arg| matches!(arg, DiagnosticArg::Cause(_, _)));

        let (cause, collection) = match arg {
            Some(DiagnosticArg::Cause(cause, collection)) => (cause.clone(), *collection),
            _ => return TokenStream::new(),
        };

        if collection {
            quote! {
                fn causes(&self) -> Box<dyn Iterator<Item = &(dyn ::lume_errors::Diagnostic + Send + Sync)> + '_> {
                    Box::new(
                        self.#cause
                            .iter()
                            .map(|e| e.as_ref() as &(dyn ::lume_errors::Diagnostic + Send + Sync)),
                    )
                }
            }
        } else {
            quote! {
                fn causes(&self) -> Box<dyn Iterator<Item = &(dyn ::lume_errors::Diagnostic + Send + Sync)> + '_> {
                    let causes: &(dyn lume_errors::Diagnostic + Send + Sync) =
                        (&self.#cause as &lume_errors::Error).as_ref();

                    let iter = std::iter::once(causes)
                        as std::iter::Once<&(dyn ::lume_errors::Diagnostic + Send + Sync)>;

                    Box::new(iter)
                }
            }
        }
    }

    /// Creates the implementation block for the `source_code` trait function.
    fn source_block(&self) -> TokenStream {
        if let Some(span) = self.span() {
            quote! {
                fn source_code(&self) -> Option<std::sync::Arc<dyn ::lume_errors::Source>> {
                    Some(self.#span.clone())
                }
            }
        } else {
            TokenStream::new()
        }
    }

    /// Creates the implementation block for the `severity` trait function.
    fn severity_block(&self) -> TokenStream {
        if let Some(severity) = self.severity() {
            let path = &severity.0;

            quote! {
                fn severity(&self) -> ::lume_errors::Severity {
                    ::lume_errors::Severity::#path
                }
            }
        } else {
            TokenStream::new()
        }
    }
}
