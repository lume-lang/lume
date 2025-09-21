use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

#[derive(Debug)]
pub enum LocationImpl {
    Struct(Ident),
    Enum(Ident, Vec<Ident>),
}

impl LocationImpl {
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
            Self::Struct(ident) => {
                quote! {
                    impl WithLocation for #ident {
                        fn location(&self) -> Location {
                            self.location
                        }
                    }
                }
            }
            Self::Enum(ident, variants) => {
                let cases = variants
                    .iter()
                    .map(|v| {
                        quote! { #ident::#v(c) => c.location() }
                    })
                    .collect::<Vec<TokenStream>>();

                quote! {
                    impl WithLocation for #ident {
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
        Self::Struct(ident)
    }

    fn from_enum(ident: Ident, data: &syn::DataEnum) -> Self {
        let variants = data.variants.iter().map(|v| v.ident.clone()).collect::<Vec<Ident>>();

        Self::Enum(ident, variants)
    }
}
