use darling::FromMeta;
use darling::ast::NestedMeta;
use proc_macro::TokenStream;
use quote::{ToTokens, format_ident, quote};
use syn::{ItemFn, ReturnType, parse_macro_input, punctuated::Punctuated};

#[derive(Debug, FromMeta)]
#[allow(dead_code)]
struct CacheMacroArgs {
    #[darling(default)]
    key: Option<syn::Meta>,

    #[darling(default)]
    result: bool,
}

pub(crate) fn cached_query(args: TokenStream, input: TokenStream) -> TokenStream {
    let attr_args = match NestedMeta::parse_meta_list(args.into()) {
        Ok(v) => v,
        Err(e) => return TokenStream::from(darling::Error::from(e).write_errors()),
    };

    let args = match CacheMacroArgs::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => return TokenStream::from(e.write_errors()),
    };

    let input = parse_macro_input!(input as ItemFn);

    let attributes = input.attrs;
    let visibility = input.vis;
    let signature = input.sig;
    let body = input.block;

    let fn_ident = signature.ident.clone();
    let inputs = signature.inputs.clone();
    let output = signature.output.clone();

    let cache_ident = format_ident!("{}", fn_ident.to_string().to_uppercase());
    let fn_cache_ident = format_ident!("{}_cache", fn_ident);
    let cache_value_ty = determine_cache_value_type(&args, &output);

    let keys = if let Some(keys) = args.key {
        parse_hash_keys(&keys)
    } else {
        get_default_keys(&inputs)
    };

    let calculate_hash_expr = quote! { {
        use std::hash::Hash;
        use std::hash::Hasher;

        let mut s = std::hash::DefaultHasher::new();

        let fn_name = &stringify!(#fn_ident);
        fn_name.hash(&mut s);
        &#keys.hash(&mut s);

        s.finish()
    } };

    let execute_block = if args.result {
        quote! { #body? }
    } else {
        quote! { #body }
    };

    let return_value = if args.result {
        quote! { Ok(value) }
    } else {
        quote! { value }
    };

    let cache_ident_doc = format!(
        "Retrieves the underlying [`lume_query::CacheStore`] instance, which memoizes the results of the [`{fn_ident}`] method."
    );

    let expanded = quote! {
        #[doc = #cache_ident_doc]
        #visibility fn #fn_cache_ident () -> &'static ::lume_query::CacheStore<#cache_value_ty> {
            static #cache_ident: ::lume_query::CacheStore<#cache_value_ty>
                = ::lume_query::CacheStore::<#cache_value_ty>::new();

            &#cache_ident
        }

        #(#attributes)*
        #[allow(unused_must_use, reason = "auto-generated")]
        #visibility fn #fn_ident (#inputs) #output {
            let hash = #calculate_hash_expr;

            if let Some(value) = Self::#fn_cache_ident().get(hash) {
                return #return_value;
            }

            let value = #execute_block;
            Self::#fn_cache_ident().insert(hash, value.clone());

            #return_value
        }
    };

    expanded.into()
}

fn determine_cache_value_type(args: &CacheMacroArgs, ty: &ReturnType) -> proc_macro2::TokenStream {
    let output_ty = match ty {
        ReturnType::Default => return quote! { () },
        ReturnType::Type(_, ty) => ty,
    };

    if args.result {
        if let syn::Type::Path(type_path) = *output_ty.clone() {
            let segments = type_path.path.segments;

            if let syn::PathArguments::AngleBracketed(brackets) = &segments.last().unwrap().arguments {
                let inner_ty = brackets.args.first().unwrap();

                quote! { #inner_ty }
            } else {
                panic!("method return type has no inner type")
            }
        } else {
            panic!("method return type is too complex")
        }
    } else {
        quote! { #output_ty }
    }
}

fn parse_hash_keys(expr: &syn::Meta) -> proc_macro2::TokenStream {
    let mut keys = match expr {
        syn::Meta::NameValue(name_value) => match &name_value.value {
            syn::Expr::Lit(lit) => lit.lit.to_token_stream().to_string(),
            syn::Expr::Path(path) => path.path.get_ident().unwrap().to_string(),
            _ => panic!("only literal- and path- expressions are supported"),
        },
        syn::Meta::Path(path) => path.get_ident().unwrap().to_string(),
        syn::Meta::List(_) => panic!("list expressions are not supported"),
    };

    keys.remove_matches("\"");

    let ident = syn::parse_str::<syn::Expr>(&keys).expect("unable to parse \"key\" expression");

    quote! { #ident }
}

fn get_default_keys(inputs: &Punctuated<syn::FnArg, syn::Token![,]>) -> proc_macro2::TokenStream {
    let keys = inputs
        .iter()
        .filter_map(|input| match input {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(pat_type) => match *pat_type.pat {
                syn::Pat::Ident(ref pat_ident) => Some(pat_ident.ident.to_string()),
                _ => None,
            },
        })
        .collect::<Vec<_>>();

    let tuple = format!("({})", keys.join(", "));
    let ident = syn::parse_str::<syn::Expr>(&tuple).expect("unable to parse \"key\" expression");

    quote! { #ident }
}
