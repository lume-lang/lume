#[macro_export]
macro_rules! ast_type_path {
    ($($segment:ident)::+) => {
        $crate::ast_type_path!(@munch [] $($segment)::+)
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
        $crate::ast_type_path!(
            @munch
            [$($ns,)* $crate::PathSegment::namespace(stringify!($head))]
            $($rest)::+
        )
    };
}
