//! # Manifold test macro.
//!
//! This is meant to be consumed within `manifold`.

use manifold::ManifoldCollectedTest;
use quote::{format_ident, quote};
use syn::{ItemMod, parse_macro_input};

/// Create a set of test functions for each Manifold test case which exists
/// within `tests/`. It only serves to remove the boilerplate of creating a
/// test function for each test case.
///
/// This macro has no options.
///
/// # Test naming
///
/// Manifold tests will be named after the relative path of the test file, with
/// some additional formatting:
/// - file extensions are replaced with `_`
/// - directory separators are replaced with `__`
///
/// So a test such as `bin/gc/non_root_array.lm` will have it's corresponding
/// test function named `bin__gc__non_root_array_lm`.
#[proc_macro_attribute]
pub fn manifold_tests(_attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let test_root = match manifold::find_test_root() {
        Ok(v) => v,
        Err(e) => {
            return syn::Error::new_spanned(Into::<proc_macro2::TokenStream>::into(item), e.message())
                .to_compile_error()
                .into();
        }
    };

    let config = manifold::Config::default();

    let collected_tests = match manifold::collect_tests(&test_root, &config) {
        Ok(v) => v,
        Err(e) => {
            return syn::Error::new_spanned(Into::<proc_macro2::TokenStream>::into(item), e.message())
                .to_compile_error()
                .into();
        }
    };

    let input = parse_macro_input!(item as ItemMod);
    let ItemMod { vis, ident, .. } = input;

    let test_blocks = collected_tests.iter().map(build_test_block).collect::<Vec<_>>();

    quote! {
        #[cfg(test)]
        #vis mod #ident {
            #(#test_blocks)*
        }
    }
    .into()
}

fn build_test_block(test: &ManifoldCollectedTest) -> proc_macro2::TokenStream {
    let test_relative_path = test.path.relative.display().to_string();
    let test_name = test_relative_path
        .replace(std::path::MAIN_SEPARATOR_STR, "__")
        .replace('.', "_");

    let test_name_ident = format_ident!("{test_name}");

    let type_type = format_ident!("{}", format!("{:?}", test.test_type));
    let test_type_tt = quote! { manifold::ManifoldTestType::#type_type };

    let test_root = test.path.root.display().to_string();
    let rel_test_path = test.path.relative.display().to_string();
    let abs_test_path = test.path.absolute.display().to_string();

    let test_path_tt = quote! { {
        manifold::TestPath {
            root: manifold::AbsolutePath::from(std::path::PathBuf::from(#test_root)),
            relative: manifold::RelativePath::from(std::path::PathBuf::from(#rel_test_path)),
            absolute: manifold::AbsolutePath::from(std::path::PathBuf::from(#abs_test_path)),
        }
    } };

    quote! {
        #[test]
        fn #test_name_ident() -> std::result::Result<(), String> {
            let dcx = lume_errors::DiagCtx::new();

            match manifold::run_single_test(#test_type_tt, #test_path_tt, dcx.clone()) {
                Ok(manifold::TestResult::Success) => Ok(()),
                Ok(manifold::TestResult::Failure { write_failure_report }) => Err(write_failure_report()),
                Err(err) => {
                    dcx.emit(err);

                    let mut renderer = lume_errors::GraphicalRenderer::new();
                    renderer.use_colors = true;
                    renderer.highlight_source = true;

                    Err(dcx.render_buffer(&mut renderer).unwrap_or_default())
                },
            }
        }
    }
}
