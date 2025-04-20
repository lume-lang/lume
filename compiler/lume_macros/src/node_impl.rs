use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

#[derive(Debug)]
pub enum NodeImpl {
    Struct(Ident),
    Enum(Ident, Vec<Ident>),
}

impl NodeImpl {
    pub fn from(input: syn::DeriveInput) -> syn::Result<Self> {
        let ident = input.ident;

        match &input.data {
            syn::Data::Struct(_) => Self::from_struct(ident),
            syn::Data::Enum(data) => Self::from_enum(ident, data),
            _ => Err(syn::Error::new_spanned(
                ident,
                "`#[derive(Node)]` only supports structs and enums",
            )),
        }
    }

    pub fn tokens(&self) -> syn::Result<TokenStream> {
        match self {
            NodeImpl::Struct(ident) => {
                let tokens = quote! {
                    impl Node for #ident {
                        fn location(&self) -> &Location {
                            &self.location
                        }
                    }
                };

                Ok(tokens)
            }
            NodeImpl::Enum(ident, variants) => {
                let cases = variants
                    .iter()
                    .map(|v| {
                        quote! { #ident::#v(c) => c.location() }
                    })
                    .collect::<Vec<TokenStream>>();

                let tokens = quote! {
                    impl Node for #ident {
                        fn location(&self) -> &Location {
                            match self {
                                #(#cases),*
                            }
                        }
                    }
                };

                Ok(tokens)
            }
        }
    }

    fn from_struct(ident: Ident) -> syn::Result<Self> {
        Ok(NodeImpl::Struct(ident))
    }

    fn from_enum(ident: Ident, data: &syn::DataEnum) -> syn::Result<Self> {
        let variants = data.variants.iter().map(|v| v.ident.clone()).collect::<Vec<Ident>>();

        Ok(NodeImpl::Enum(ident, variants))
    }
}
