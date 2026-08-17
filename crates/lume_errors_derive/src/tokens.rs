use proc_macro2::TokenStream;
use quote::{ToTokens, quote};

use crate::DiagnosticArgs;
use crate::fmt::FormattedMessage;
use crate::parse::{FieldArgs, LabelArgs, RelatedArgs, Severity};

impl ToTokens for DiagnosticArgs {
    #[allow(clippy::too_many_lines)]
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let DiagnosticArgs {
            ident,
            message,
            code,
            severity,
            help,
            data,
            ..
        } = self;

        let (impl_gen, ty_gen, where_clause) = &self.generics.split_for_impl();

        let field_args = data.as_ref().take_struct().unwrap();
        let field_names: Vec<_> = field_args.iter().map(|field| field.ident.clone()).collect();

        let primary_span_field = field_args.iter().find(|field| field.primary_span);
        let related_field = field_args.iter().find(|field| field.related.is_some());
        let label_fields: Vec<_> = field_args.iter().filter(|field| field.label.is_some()).collect();

        let expand_field_names = quote! {
            #[allow(unused_variables)]
            let Self { #(#field_names),* } = self;
        };

        let message_block = {
            let lit = syn::LitStr::new(message.as_str(), message.span());
            let formatted = FormattedMessage::expand(lit);

            quote! {
                fn message(&self) -> String {
                    #expand_field_names

                    #formatted
                }
            }
        };

        let code_block = if let Some(code) = &code {
            quote! {
                fn code(&self) -> Option<Box<dyn std::fmt::Display + '_>> {
                    Some(Box::new(#code) as Box<dyn std::fmt::Display + '_>)
                }
            }
        } else {
            TokenStream::new()
        };

        let help_block = if help.is_empty() {
            TokenStream::new()
        } else {
            let forall_help = help
                .iter()
                .map(|s| FormattedMessage::expand(syn::LitStr::new(s.as_str(), proc_macro2::Span::call_site())))
                .collect::<Vec<_>>();

            quote! {
                fn help(&self) -> Option<Box<dyn Iterator<Item = ::lume_errors::Help> + '_>> {
                    #expand_field_names

                    Some(Box::new(
                        vec![ #(#forall_help),* ]
                            .into_iter()
                            .map(Into::<::lume_errors::Help>::into)
                    ))
                }
            }
        };

        let labels_block = if label_fields.is_empty() {
            TokenStream::new()
        } else {
            let label_pairs = label_fields
                .into_iter()
                .map(|FieldArgs { ident, label, .. }| {
                    let LabelArgs {
                        message,
                        severity,
                        source,
                    } = label.as_ref().unwrap();

                    let lit_str = syn::LitStr::new(message, proc_macro2::Span::call_site());
                    let formatted_str = FormattedMessage::expand(lit_str);

                    let label_stream = if *source {
                        quote! {
                            ::lume_errors::Label::new(
                                Into::<::lume_errors::SpanRange>::into(
                                    self.#ident.clone()
                                ),
                                #formatted_str
                            )
                            .with_source(Some(
                                Into::<std::sync::Arc<dyn ::lume_errors::Source>>::into(
                                    self.#ident.clone()
                                )
                            ))
                        }
                    } else {
                        quote! {
                            ::lume_errors::Label::new(self.#ident.clone(), #formatted_str)
                                .with_source(::lume_errors::Diagnostic::source_code(self))
                        }
                    };

                    if let Some(severity) = severity {
                        quote! { #label_stream .with_severity(#severity) }
                    } else {
                        label_stream
                    }
                })
                .collect::<Vec<TokenStream>>();

            quote! {
                fn labels(&self) -> Option<Box<dyn Iterator<Item = ::lume_errors::Label> + '_>> {
                    #expand_field_names

                    let labels = Box::new(vec![ #(#label_pairs),* ].into_iter());

                    Some(labels)
                }
            }
        };

        let related_block = if let Some(FieldArgs {
            ident,
            related: Some(RelatedArgs { collection }),
            ..
        }) = related_field
        {
            if *collection {
                quote! {
                    fn related(&self) -> Box<dyn Iterator<Item = &(dyn ::lume_errors::Diagnostic + Send + Sync)> + '_> {
                        Box::new(
                            self.#ident
                                .iter()
                                .map(|e| e.as_ref() as &(dyn ::lume_errors::Diagnostic + Send + Sync)),
                        )
                    }
                }
            } else {
                quote! {
                    fn related(&self) -> Box<dyn Iterator<Item = &(dyn ::lume_errors::Diagnostic + Send + Sync)> + '_> {
                        let related: &(dyn ::lume_errors::Diagnostic + Send + Sync) =
                            (&self.#ident as &::lume_errors::Error).as_ref();

                        let iter = std::iter::once(related)
                            as std::iter::Once<&(dyn ::lume_errors::Diagnostic + Send + Sync)>;

                        Box::new(iter)
                    }
                }
            }
        } else {
            TokenStream::new()
        };

        let source_block = if let Some(FieldArgs { ident, .. }) = primary_span_field {
            quote! {
                fn source_code(&self) -> Option<std::sync::Arc<dyn ::lume_errors::Source>> {
                    Some(self.#ident.clone())
                }
            }
        } else {
            TokenStream::new()
        };

        let severity_block = quote! {
            #[inline]
            fn severity(&self) -> ::lume_errors::Severity {
                #severity
            }
        };

        tokens.extend(quote! {
            impl #impl_gen ::lume_errors::Diagnostic for #ident #ty_gen #where_clause {
                #message_block
                #code_block
                #help_block
                #labels_block
                #related_block
                #source_block
                #severity_block
            }

            impl #impl_gen ::std::error::Error for #ident #ty_gen #where_clause {}

            impl #impl_gen ::std::fmt::Display for #ident #ty_gen #where_clause {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    write!(f, "{}", ::lume_errors::Diagnostic::message(self))
                }
            }
        });
    }
}

impl ToTokens for Severity {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Severity::Error => tokens.extend(quote! { ::lume_errors::Severity::Error }),
            Severity::Warning => tokens.extend(quote! { ::lume_errors::Severity::Warning }),
            Severity::Info => tokens.extend(quote! { ::lume_errors::Severity::Info }),
            Severity::Help => tokens.extend(quote! { ::lume_errors::Severity::Help }),
            Severity::Note => tokens.extend(quote! { ::lume_errors::Severity::Note }),
        }
    }
}
