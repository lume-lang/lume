//! # Manifold test macro.
//!
//! This is meant to be consumed within `manifold`.

use quote::{quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{Expr, FnArg, ItemFn, Signature, parse_macro_input};

#[derive(Debug, darling::FromMeta)]
#[darling(derive_syn_parse)]
struct BenchmarkOptions {
    /// Arguments to pass to the setup function.
    args: Option<Expr>,

    #[darling(multiple)]
    counter: Vec<Expr>,
}

#[proc_macro_attribute]
pub fn benchmark(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let BenchmarkOptions { args, counter } = match syn::parse(attr) {
        Ok(args) => args,
        Err(e) => return e.into_compile_error().into(),
    };

    let input = parse_macro_input!(item as ItemFn);
    let ItemFn { vis, sig, block, .. } = input;
    let Signature {
        ident, inputs, output, ..
    } = sig;

    for parameter in &inputs {
        if let FnArg::Receiver(_) = parameter {
            return syn::Error::new_spanned(parameter, "benchmark functions cannot have `self`")
                .to_compile_error()
                .into();
        }
    }

    let return_type = match &output {
        syn::ReturnType::Default => quote_spanned! { output.span() => () },
        syn::ReturnType::Type(_, ty) => quote_spanned! { output.span() => #ty },
    };

    let mut bencher_tt = quote! { bencher };

    for counter in counter {
        bencher_tt = quote! {
            #bencher_tt
                .counter(#counter)
        };
    }

    quote! {
        #[::lume_benchmark::divan::bench_group(crate = ::lume_benchmark::divan)]
        mod benchmarks {
            use super::*;

            #[::lume_benchmark::divan::bench(crate = ::lume_benchmark::divan, args = #args)]
            #vis fn #ident (bencher: ::lume_benchmark::divan::Bencher, #inputs) {
                let source_code_gen = move || -> #return_type {
                    #block
                };

                let source_code = source_code_gen().to_string();

                let binary_path = ::lume_benchmark::__private::prepare_benchmark(
                    std::env!("CARGO_TARGET_TMPDIR"),
                    ::std::stringify!(#ident),
                    source_code
                );

                #bencher_tt
                    .bench(|| {
                        ::lume_benchmark::run(&binary_path).unwrap()
                    });
            }
        }

        fn main() {
            ::lume_benchmark::divan::main();
        }
    }
    .into()
}
