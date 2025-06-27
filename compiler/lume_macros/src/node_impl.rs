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
            syn::Data::Struct(_) => Ok(Self::from_struct(ident)),
            syn::Data::Enum(data) => Ok(Self::from_enum(ident, data)),
            syn::Data::Union(_) => Err(syn::Error::new_spanned(
                ident,
                "`#[derive(Node)]` only supports structs and enums",
            )),
        }
    }

    pub fn tokens(&self) -> TokenStream {
        match self {
            NodeImpl::Struct(ident) => {
                quote! {
                    impl Node for #ident {
                        fn location(&self) -> Location {
                            self.location
                        }
                    }
                }
            }
            NodeImpl::Enum(ident, variants) => {
                let cases = variants
                    .iter()
                    .map(|v| {
                        quote! { #ident::#v(c) => c.location() }
                    })
                    .collect::<Vec<TokenStream>>();

                quote! {
                    impl Node for #ident {
                        fn location(&self) -> Location {
                            match self {
                                #(#cases),*
                            }
                        }
                    }
                }
            }
        }
    }

    fn from_struct(ident: Ident) -> Self {
        NodeImpl::Struct(ident)
    }

    fn from_enum(ident: Ident, data: &syn::DataEnum) -> Self {
        let variants = data.variants.iter().map(|v| v.ident.clone()).collect::<Vec<Ident>>();

        NodeImpl::Enum(ident, variants)
    }
}
