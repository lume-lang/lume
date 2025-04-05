use diag::Severity;
use syn::{Attribute, Error, Field, Ident, MetaNameValue, Result};

#[derive(Debug)]
pub enum DiagnosticArg {
    Message(String),
    Code(String),
    Help(String),
    Severity(Severity),
    Source(Ident),
    Span(Ident),
    Label(String, Ident),
}

impl DiagnosticArg {
    pub fn parse_field(field: &Field) -> Result<Option<Self>> {
        let field_ident = field.ident.as_ref();
        let field_ident = field_ident.unwrap();

        for attr in &field.attrs {
            if let syn::Meta::Path(path) = &attr.meta {
                let ident = match path.get_ident() {
                    Some(ident) => ident,
                    None => return Err(Error::new_spanned(&path, "Expected identifier")),
                };

                let arg = match ident.to_string().as_str() {
                    "span" => DiagnosticArg::Span(field_ident.clone()),
                    "source" => DiagnosticArg::Source(field_ident.clone()),
                    _ => return Err(Error::new_spanned(ident, "Invalid property attribute")),
                };

                return Ok(Some(arg));
            } else if let syn::Meta::List(meta) = &attr.meta {
                let ident = match meta.path.get_ident() {
                    Some(ident) => ident,
                    None => return Err(Error::new_spanned(&meta.path, "Expected identifier")),
                };

                let arg = match ident.to_string().as_str() {
                    "label" => Self::parse_label(&field_ident, &meta)?,
                    _ => return Err(Error::new_spanned(ident, "Invalid property attribute")),
                };

                return Ok(Some(arg));
            } else {
                return Err(Error::new_spanned(attr, "Expected path attribute"));
            }
        }

        Ok(None)
    }

    fn parse_label(ident: &syn::Ident, list: &syn::MetaList) -> Result<Self> {
        if let Ok(label) = list.parse_args::<syn::LitStr>() {
            Ok(DiagnosticArg::Label(label.value(), ident.clone()))
        } else {
            Err(Error::new_spanned(list, "Expected attribute argument"))
        }
    }

    pub fn parse_attribute(attr: &Attribute) -> Result<Vec<Self>> {
        if attr.path().is_ident("diagnostic") {
            DiagnosticArg::parse_diagnostic(attr)
        } else {
            Err(Error::new_spanned(attr, "Unknown attribute"))
        }
    }

    fn parse_diagnostic(attr: &Attribute) -> Result<Vec<Self>> {
        if let syn::Meta::List(meta) = &attr.meta {
            let mut args = Vec::new();
            let parser = syn::punctuated::Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated;

            for arg_meta in meta.parse_args_with(parser)? {
                if let syn::Meta::NameValue(name_value) = arg_meta {
                    let arg = Self::parse_diagnostic_argument(&name_value)?;

                    args.push(arg);
                } else {
                    return Err(Error::new_spanned(meta, "Expected name-value attribute"));
                }
            }

            Ok(args)
        } else {
            Err(Error::new_spanned(attr, "Expected list attribute"))
        }
    }

    fn parse_diagnostic_argument(name_value: &MetaNameValue) -> Result<Self> {
        let ident = match name_value.path.get_ident() {
            Some(ident) => ident,
            None => return Err(Error::new_spanned(&name_value.path, "Expected identifier")),
        };

        match ident.to_string().as_str() {
            "code" => Self::parse_code(&name_value),
            "message" => Self::parse_message(&name_value),
            "help" => Self::parse_help(&name_value),
            "severity" => Self::parse_severity(&name_value),
            _ => return Err(Error::new_spanned(ident, "Invalid diagnostic attribute")),
        }
    }

    fn parse_message(meta: &MetaNameValue) -> Result<Self> {
        if let syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Str(lit_str),
            ..
        }) = meta.value.clone()
        {
            Ok(DiagnosticArg::Message(lit_str.value()))
        } else {
            Err(Error::new_spanned(meta, "Expected string literal"))
        }
    }

    fn parse_code(meta: &MetaNameValue) -> Result<Self> {
        if let syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Str(lit_str),
            ..
        }) = meta.value.clone()
        {
            Ok(DiagnosticArg::Code(lit_str.value()))
        } else {
            Err(Error::new_spanned(meta, "Expected string literal"))
        }
    }

    fn parse_help(meta: &MetaNameValue) -> Result<Self> {
        if let syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Str(lit_str),
            ..
        }) = meta.value.clone()
        {
            Ok(DiagnosticArg::Help(lit_str.value()))
        } else {
            Err(Error::new_spanned(meta, "Expected string literal"))
        }
    }

    fn parse_severity(meta: &MetaNameValue) -> Result<Self> {
        if let syn::Expr::Path(syn::ExprPath { path, .. }) = meta.value.clone() {
            let ident = match path.get_ident() {
                Some(ident) => ident,
                None => return Err(Error::new_spanned(path, "Expected ident for path")),
            };

            let severity = match ident.to_string().as_ref() {
                "Error" => Severity::Error,
                "Warning" => Severity::Warning,
                "Info" => Severity::Info,
                "Note" => Severity::Note,
                "Help" => Severity::Help,
                _ => {
                    return Err(Error::new_spanned(
                        path,
                        "Invalid severity: must be either `Error`, `Warning`,` `Info`, `Note` or `Help`",
                    ));
                }
            };

            Ok(DiagnosticArg::Severity(severity))
        } else {
            Err(Error::new_spanned(meta, "Expected path literal"))
        }
    }
}
