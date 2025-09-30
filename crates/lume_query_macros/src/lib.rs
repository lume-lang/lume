mod cached_query;

use proc_macro::TokenStream;

/// Defines a memoized type-checker query method, which stores
/// results of the method in a global cache store. Cached results are
/// keyed from the method arguments.
///
/// # Attributes
/// - `key`: (optional, string expr) specify the value(s) which should be
///   used to compute the hash key for the method.
///
///   NOTE: the resulting expression **must** implement [`std::hash::Hash`].
///
///   Example:
///   ```rs
///   #[cached_query(key = "(def_id)")]
///   ```
///
/// - `result`: (optional, boolean) specifies that the return type of the method
///   is a [`Result`], which should only be cached if handled successfully.
///
///   Example:
///   ```rs
///   #[cached_query(result)]
///   ```
#[proc_macro_attribute]
pub fn cached_query(args: TokenStream, input: TokenStream) -> TokenStream {
    cached_query::cached_query(args, input)
}
