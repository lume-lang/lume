use syn::Ident;

use crate::args::DiagnosticArg;

#[derive(Clone, Debug)]
pub struct Severity(pub Ident);

pub struct AttrDiagnostic {
    pub ident: Ident,
    pub args: Vec<DiagnosticArg>,
    pub generics: syn::Generics,
    pub fields: syn::Fields,
}

impl AttrDiagnostic {
    pub fn from(input: syn::DeriveInput) -> syn::Result<Self> {
        if let syn::Data::Struct(syn::DataStruct { fields, .. }) = input.data {
            let args = DiagnosticArg::parse_attributes(&input.attrs)?;

            let mut diagnostic = AttrDiagnostic {
                ident: input.ident,
                args,
                generics: input.generics,
                fields,
            };

            for field in &diagnostic.fields {
                let field_attr_arg = match DiagnosticArg::parse_field(field)? {
                    Some(attr) => attr,
                    None => continue,
                };

                diagnostic.args.push(field_attr_arg);
            }

            // Verify that all required attributes are given.
            diagnostic.verify()?;

            return Ok(diagnostic);
        }

        Err(syn::Error::new_spanned(
            &input.ident,
            "can only apply Diagnostic macro to structs",
        ))
    }

    /// Verifies the diagnostic attribute.
    pub(crate) fn verify(&self) -> syn::Result<()> {
        self.message()?;

        Ok(())
    }

    pub(crate) fn err(&self, message: &'static str) -> syn::Error {
        syn::Error::new_spanned(&self.ident, message)
    }
}
