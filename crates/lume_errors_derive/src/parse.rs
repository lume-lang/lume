#![allow(clippy::needless_continue, reason = "darling issue, i guess")]

use darling::{FromAttributes, FromMeta};
use quote::ToTokens;
use syn::Ident;
use syn::spanned::Spanned;

#[derive(Debug, darling::FromDeriveInput)]
#[darling(attributes(diagnostic), supports(struct_named))]
pub struct DiagnosticArgs {
    pub ident: Ident,
    pub generics: syn::Generics,
    pub data: darling::ast::Data<(), FieldArgs>,

    /// A formatted message to be printed to the user, once the diagnostic is
    /// rendered.
    pub message: darling::util::SpannedValue<String>,

    /// Diagnostic severity level.
    ///
    /// This may be used by the renderer to determine how to display the
    /// diagnostic or even halt the program, depending on the severity
    /// level.
    #[darling(default)]
    pub severity: Severity,

    /// Unique diagnostic code, which can be used to look up more information
    /// about the error.
    #[darling(default)]
    pub code: Option<String>,

    /// Help messages, which can be used to provide additional information about
    /// the diagnostic.
    #[darling(multiple)]
    pub help: Vec<String>,
}

#[derive(Debug)]
pub struct FieldArgs {
    pub ident: Ident,

    pub primary_span: bool,
    pub label: Option<LabelArgs>,
    pub related: Option<RelatedArgs>,
}

impl darling::FromField for FieldArgs {
    fn from_field(field: &syn::Field) -> darling::Result<Self> {
        let mut primary_span = false;
        let mut label = None;
        let mut related = None;

        for attr in &field.attrs {
            if attr.path().is_ident("primary_span") {
                primary_span = true;
            } else if attr.path().is_ident("related") {
                related = Some(RelatedArgs::from_attributes(std::slice::from_ref(attr))?);
            } else if attr.path().is_ident("label") {
                label = Some(LabelArgs::from_meta(&attr.meta)?);
            } else {
                return Err(darling::Error::custom("unknown attribute"));
            }
        }

        Ok(Self {
            ident: field.ident.clone().unwrap(),
            primary_span,
            label,
            related,
        })
    }
}

#[derive(Debug, darling::FromAttributes)]
#[darling(attributes(related))]
pub struct RelatedArgs {
    /// Defines the severity of the label, which can be independant from the
    /// parent diagnostic.
    #[darling(default)]
    pub collection: bool,
}

#[derive(Debug)]
pub struct LabelArgs {
    /// Defines the actual label to print on the snippet.
    pub message: String,

    /// Defines whether the label field has a source file associated with it.
    pub source: bool,

    /// Defines the severity of the label, which can be independant from the
    /// parent diagnostic.
    pub severity: Option<Severity>,
}

impl darling::FromMeta for LabelArgs {
    fn from_meta(item: &syn::Meta) -> darling::Result<Self> {
        let mut label = Self {
            message: String::new(),
            source: false,
            severity: None,
        };

        let parser = syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated;
        for expr in item.require_list()?.parse_args_with(parser)? {
            match expr {
                syn::Expr::Lit(expr) => {
                    let syn::Lit::Str(name) = &expr.lit else {
                        return Err(darling::Error::unexpected_lit_type(&expr.lit).with_span(&expr.span()));
                    };

                    label.message = name.value();
                }
                syn::Expr::Assign(expr) => {
                    let name = expr.left.to_token_stream().to_string();
                    match name.as_str() {
                        "severity" => label.severity = Some(Severity::from_expr(&expr.right)?),
                        "message" => label.message = String::from_expr(&expr.right)?,
                        field => return Err(darling::Error::unknown_field(field).with_span(&expr.span())),
                    }
                }
                syn::Expr::Path(expr) => {
                    let name = expr.to_token_stream().to_string();
                    match name.as_str() {
                        "source" => label.source = true,
                        field => return Err(darling::Error::unknown_field(field).with_span(&expr.span())),
                    }
                }
                expr => return Err(darling::Error::unexpected_expr_type(&expr)),
            }
        }

        if label.message.is_empty() {
            return Err(darling::Error::missing_field("message").with_span(&item.span()));
        }

        Ok(label)
    }
}

#[derive(Default, Debug, Clone, Copy)]
pub enum Severity {
    #[default]
    Error,
    Warning,
    Info,
    Help,
    Note,
}

impl darling::FromMeta for Severity {
    fn from_expr(item: &syn::Expr) -> darling::Result<Self> {
        let syn::Expr::Path(path) = item else {
            return Err(darling::Error::unexpected_expr_type(item));
        };

        let ident = path.path.require_ident()?.to_string();
        match ident.as_str() {
            "Error" => Ok(Self::Error),
            "Warning" => Ok(Self::Warning),
            "Info" => Ok(Self::Info),
            "Help" => Ok(Self::Help),
            "Note" => Ok(Self::Note),
            _ => Err(darling::Error::custom(format!("invalid severity: {ident}"))),
        }
    }
}
