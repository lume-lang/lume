/// Creates a new HIR path to a named HIR type.
#[macro_export]
macro_rules! hir_type_path {
    ($($segment:ident)::+) => {
        $crate::hir_type_path!(@munch [] $($segment)::+)
    };

    // Base case, no namespaces accumulated
    (@munch [] $name:ident) => {
        $crate::Path::from_parts(
            None,
            $crate::PathSegment::ty(stringify!($name)),
        )
    };

    // Base case, some namespaces accumulated
    (@munch [$($ns:expr),+] $name:ident) => {
        $crate::Path::new(
            vec![$($ns),+],
            $crate::PathSegment::ty(stringify!($name)),
        )
    };

    // Recursive case
    (@munch [$($ns:expr),*] $head:ident :: $($rest:ident)::+) => {
        $crate::hir_type_path!(
            @munch
            [$($ns,)* $crate::PathSegment::namespace(stringify!($head))]
            $($rest)::+
        )
    };
}

/// Creates a new HIR path to a named HIR type inside the standard library
/// namespace.
#[macro_export]
macro_rules! hir_std_type_path {
    ($name:ident) => {
        $crate::hir_type_path!(std :: $name)
    };
    ($($namespace:ident)::+, $name:ident) => {
        $crate::Path::from_parts(
            Some(vec![
                $crate::PathSegment::namespace("std"),
                $(
                    $crate::PathSegment::namespace(stringify!($namespace)),
                ),*
            ]),
            $crate::PathSegment::ty(stringify!($name)),
        )
    };
}

/// Creates a new HIR path to a HIR method, on a named HIR type.
#[macro_export]
macro_rules! hir_method_path {
    ($name:ident) => {
        $crate::Path::rooted(
            $crate::PathSegment::ty(stringify!($name)),
        )
    };
    ($($namespace:ident)::+, $name:ident) => {
        $crate::Path::from_parts(
            Some(vec![
                $(
                    $crate::PathSegment::namespace(stringify!($namespace)),
                ),*
            ]),
            $crate::PathSegment::ty(stringify!($name)),
        )
    };
}
