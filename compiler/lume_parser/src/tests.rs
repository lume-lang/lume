use super::*;
use error_snippet::Error;

#[track_caller]
fn parse(input: &str) -> Vec<TopLevelExpression> {
    let parser = Parser::parse_str(input);

    parser.unwrap()
}

#[track_caller]
fn parse_err(input: &str) -> Error {
    let parser = Parser::parse_str(input);

    parser.unwrap_err()
}

#[track_caller]
fn parse_expr(input: &str) -> Vec<Statement> {
    let mut parser = Parser::new_with_str(input);

    parser.prepare().unwrap();
    parser.parse_statements().unwrap()
}

#[track_caller]
fn parse_expr_err(input: &str) -> Error {
    let mut parser = Parser::new_with_str(input);

    parser.prepare().unwrap();
    parser.parse_statements().unwrap_err()
}

macro_rules! assert_module_eq {
    (
        $input: expr,
        $expression: expr
    ) => {
        let parsed = parse($input);

        assert_eq!(parsed, $expression)
    };
}

macro_rules! assert_err_eq {
    (
        $input: expr,
        $message: expr
    ) => {
        let error = parse_err($input);

        assert_eq!(error.message(), $message)
    };
}

macro_rules! set_snapshot_suffix {
    ($($expr:expr),*) => {
        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_suffix(format!($($expr,)*));
        let _guard = settings.bind_to_scope();
    }
}

macro_rules! assert_snap_eq {
    (
        $input: expr,
        $($expr:expr),+
    ) => {
        set_snapshot_suffix!( $($expr),+ );

        insta::assert_debug_snapshot!(parse($input));
    };
}

macro_rules! assert_err_snap_eq {
    (
        $input: expr,
        $($expr:expr),+
    ) => {
        set_snapshot_suffix!( $($expr),+ );

        insta::assert_debug_snapshot!(parse_err($input));
    };
}

macro_rules! assert_expr_snap_eq {
    (
        $input: expr,
        $($expr:expr),+
    ) => {
        set_snapshot_suffix!( $($expr),+ );

        insta::assert_debug_snapshot!(parse_expr($input));
    };
}

macro_rules! assert_expr_err_snap_eq {
    (
        $input: expr,
        $($expr:expr),+
    ) => {
        set_snapshot_suffix!( $($expr),+ );

        insta::assert_debug_snapshot!(parse_expr_err($input));
    };
}

#[test]
fn test_empty_module() {
    assert_module_eq!("", vec![]);
}

#[test]
fn test_whitespace_module() {
    assert_module_eq!("    ", vec![]);
}

#[test]
fn test_newline_module() {
    assert_module_eq!("\n\n    \n", vec![]);
}

#[test]
fn test_imports() {
    assert_eq!(
        Parser::parse_str("import std (Int)").unwrap(),
        vec![TopLevelExpression::Import(Box::new(Import {
            path: ImportPath {
                path: vec![Identifier {
                    name: "std".into(),
                    location: Location(7..10)
                }],
                location: Location(7..10)
            },
            names: vec![Identifier {
                name: "Int".into(),
                location: Location(12..15)
            }],
            location: Location(0..16)
        }))]
    );

    assert_eq!(
        Parser::parse_str("import std::io (File)").unwrap(),
        vec![TopLevelExpression::Import(Box::new(Import {
            path: ImportPath {
                path: vec![
                    Identifier {
                        name: "std".into(),
                        location: Location(7..10)
                    },
                    Identifier {
                        name: "io".into(),
                        location: Location(12..14)
                    }
                ],
                location: Location(8..14)
            },
            names: vec![Identifier {
                name: "File".into(),
                location: Location(16..20)
            }],
            location: Location(0..21)
        }))]
    );

    assert_eq!(
        Parser::parse_str("import std::io (File, Buffer)").unwrap(),
        vec![TopLevelExpression::Import(Box::new(Import {
            path: ImportPath {
                path: vec![
                    Identifier {
                        name: "std".into(),
                        location: Location(7..10)
                    },
                    Identifier {
                        name: "io".into(),
                        location: Location(12..14)
                    }
                ],
                location: Location(7..14)
            },
            names: vec![
                Identifier {
                    name: "File".into(),
                    location: Location(16..20)
                },
                Identifier {
                    name: "Buffer".into(),
                    location: Location(22..28)
                }
            ],
            location: Location(0..29)
        }))]
    );

    assert_eq!(
        Parser::parse_str("import std::io ()").unwrap(),
        vec![TopLevelExpression::Import(Box::new(Import {
            path: ImportPath {
                path: vec![
                    Identifier {
                        name: "std".into(),
                        location: Location(7..10)
                    },
                    Identifier {
                        name: "io".into(),
                        location: Location(12..14)
                    }
                ],
                location: Location(7..14)
            },
            names: vec![],
            location: Location(0..17)
        }))]
    );

    assert_err_eq!("import std::io", "invalid import path");
    assert_err_eq!("import std::io::", "expected identifier");
    assert_err_eq!("import ::std::io", "expected identifier");
}

#[test]
fn test_namespace_snapshots() {
    assert_snap_eq!("namespace std", "path_1");
    assert_snap_eq!("namespace std::io", "path_2");
    assert_snap_eq!("namespace std::io::path", "path_3");
    assert_snap_eq!("namespace System::IO", "path_casing");
    assert_err_eq!("namespace", "expected identifier");
    assert_err_eq!("namespace ::std", "expected identifier");
    assert_err_eq!("namespace std::io::", "expected identifier");
}

#[test]
fn test_function_definition_snapshots() {
    assert_snap_eq!("fn main() -> void {}", "empty");
    assert_snap_eq!("fn main() -> void { let a = 0; }", "statement");
    assert_snap_eq!("fn main() -> void { let a = 0; let b = 1; }", "statements");
    assert_snap_eq!("fn main() {}", "no_return_type");
    assert_snap_eq!("fn main(argc: UInt8) -> void { }", "parameter");
    assert_snap_eq!("fn main(argc: UInt8, arcv: [String]) -> void { }", "parameters");
    assert_snap_eq!("fn external main() -> void", "external");
    assert_err_snap_eq!("fn external main() -> void {}", "external_body");
    assert_snap_eq!("pub fn main() -> void {}", "pub_modifier");
    assert_snap_eq!("fn loop() -> void {}", "reserved_keyword");
    assert_snap_eq!("fn main() -> std::Int32 {}", "namespaced_type");
    assert_snap_eq!("fn empty?() {}", "boolean_function");
    assert_snap_eq!("fn foo(...args: Int32) {}", "varargs");
}

#[test]
fn test_cast_snapshots() {
    assert_expr_snap_eq!("0 as u64;", "literal_single");
    assert_expr_snap_eq!("let _ = 0 as u64;", "precedence_assign");
    assert_expr_snap_eq!("0 as u64 == 0;", "precedence_equal");
    assert_expr_snap_eq!("a() as u64;", "invocation");
}

#[test]
fn test_binary_snapshots() {
    assert_expr_snap_eq!("0 & 0;", "and");
    assert_expr_snap_eq!("0 | 0;", "or");
    assert_expr_snap_eq!("0 ^ 0;", "xor");
}

#[test]
fn test_boolean_snapshots() {
    assert_expr_snap_eq!("0 && 0;", "and");
    assert_expr_snap_eq!("0 || 0;", "or");
}

#[test]
fn test_literal_snapshots() {
    assert_expr_snap_eq!("\"\";", "string_empty");
    assert_expr_snap_eq!("\"string\";", "string_content");
    assert_expr_snap_eq!("true;", "bool_true");
    assert_expr_snap_eq!("false;", "bool_false");
    assert_expr_snap_eq!("ident;", "ident");
    assert_expr_snap_eq!("IDENT;", "ident_case");
    assert_expr_snap_eq!("__IDENT__;", "ident_underscore");
    assert_expr_snap_eq!("0;", "int");
    assert_expr_snap_eq!("55;", "int_positive");
    assert_expr_snap_eq!("-55;", "int_negative");
    assert_expr_snap_eq!("0x55;", "int_hex_positive");
    assert_expr_snap_eq!("-0x55;", "int_hex_negative");
    assert_expr_snap_eq!("0b01010101;", "int_bin_positive");
    assert_expr_snap_eq!("-0b01010101;", "int_bin_negative");
    assert_expr_snap_eq!("0o125;", "int_oct_positive");
    assert_expr_snap_eq!("-0o125;", "int_oct_negative");
}

#[test]
fn test_self_snapshots() {
    assert_expr_snap_eq!("self;", "self");
    assert_expr_snap_eq!("self + self;", "self_binary_op");
    assert_expr_snap_eq!("self.invoke();", "self_call");
    assert_expr_snap_eq!("self.invoke<T>();", "self_generic_call");
    assert_expr_snap_eq!("self::invoke();", "self_static_call");
    assert_expr_snap_eq!("self::invoke<T>();", "self_static_generic_call");
}

#[test]
fn test_construction_snapshots() {
    assert_expr_snap_eq!("Foo { };", "empty");
    assert_expr_snap_eq!("Self { };", "empty_self");
    assert_expr_snap_eq!("Foo { bar: 0 };", "field");
    assert_expr_snap_eq!("Foo { bar: 0, baz: 1 };", "fields");
    assert_expr_snap_eq!("Foo { bar };", "shorthand");
    assert_expr_snap_eq!("Foo { bar: 0, };", "trailing_comma");
    assert_expr_err_snap_eq!("Foo { bar: 0 baz };", "missing_comma");
}

#[test]
fn test_conditional_snapshots() {
    assert_expr_snap_eq!("if true { }", "if_empty");
    assert_expr_snap_eq!("if true { let a = 1; }", "if_statement");
    assert_expr_snap_eq!("if true { } else if false { }", "if_else_if_empty");
    assert_expr_snap_eq!("if true { } else { }", "if_else_empty");
    assert_expr_snap_eq!("if true { } else if false { } else { }", "if_else_if_else_empty");
    assert_expr_snap_eq!("if a == 1 { }", "equality_empty");
    assert_expr_snap_eq!("if a != 1 { }", "inequality_empty");
    assert_expr_snap_eq!("if true { let a = 0; }", "if_statement");
    assert_expr_snap_eq!("if true { let a = 0; let b = 0; }", "if_statements");
    assert_expr_snap_eq!(
        "if true { let a = 0; } else if false { let a = 0; }",
        "else_if_statements"
    );
}

#[test]
fn test_loop_snapshots() {
    assert_expr_snap_eq!("loop { }", "inf_loop_empty");
    assert_expr_snap_eq!("loop { let a = 0; }", "inf_loop_statement");
    assert_expr_snap_eq!("loop { break; }", "inf_loop_break");
    assert_expr_snap_eq!("loop { continue; }", "inf_loop_continue");

    assert_expr_snap_eq!("while true { }", "pred_loop_empty");
    assert_expr_snap_eq!("while true { let a = 0; }", "pred_loop_statement");
    assert_expr_snap_eq!("while true { break; }", "pred_loop_break");
    assert_expr_snap_eq!("while true { continue; }", "pred_loop_continue");

    assert_expr_snap_eq!("for pattern in collection { }", "iter_loop_empty");
    assert_expr_snap_eq!("for pattern in collection { let a = 0; }", "iter_loop_statement");
    assert_expr_snap_eq!("for pattern in collection { break; }", "iter_loop_break");
    assert_expr_snap_eq!("for pattern in collection { continue; }", "iter_loop_continue");
    assert_expr_snap_eq!("for pattern in [1, 2, 3] { }", "iter_expr");
}

#[test]
fn test_array_snapshots() {
    assert_expr_snap_eq!("let _ = [];", "empty");
    assert_expr_snap_eq!("let _ = [a];", "single");
    assert_expr_snap_eq!("let _ = [a, b];", "multiple");
    assert_expr_snap_eq!("let _ = [a, [a, b]];", "nested");
    assert_expr_err_snap_eq!("let _ = [a b];", "missing_comma");
    assert_expr_snap_eq!("let _ = [a, ];", "trailing_comma");
}

#[test]
fn test_range_snapshots() {
    assert_expr_snap_eq!("let _ = (0..1);", "literal_exclusive");
    assert_expr_snap_eq!("let _ = (0..=1);", "literal_inclusive");
    assert_expr_snap_eq!("let _ = (a..b);", "expr_exclusive");
    assert_expr_snap_eq!("let _ = (a..=b);", "expr_inclusive");
    assert_expr_snap_eq!("let _ = ((a + b)..(a + b + 1));", "expr_nested_exclusive");
    assert_expr_snap_eq!("let _ = ((a + b)..=(a + b + 1));", "expr_nested_inclusive");
}

#[test]
fn test_call_snapshots() {
    assert_expr_snap_eq!("let _ = call();", "function_empty");
    assert_expr_snap_eq!("let _ = call(a);", "function_param_1");
    assert_expr_snap_eq!("let _ = call(a, b);", "function_param_2");
    assert_expr_snap_eq!("let _ = call?();", "function_boolean");
    assert_expr_snap_eq!("let _ = call<T>(a, b);", "function_generic");

    assert_expr_snap_eq!("let _ = a.call();", "method_empty");
    assert_expr_snap_eq!("let _ = a.call(a);", "method_param_1");
    assert_expr_snap_eq!("let _ = a.call(a, b);", "method_param_2");
    assert_expr_snap_eq!("let _ = a.call?();", "method_boolean");
    assert_expr_snap_eq!("let _ = a.call<T>(a, b);", "method_generic");
    assert_expr_snap_eq!("let _ = Foo::call(a, b);", "static_method");
    assert_expr_snap_eq!("let _ = Foo::call<T>(a, b);", "static_generic_method");
    assert_expr_snap_eq!("let _ = Foo<T>::call(a, b);", "generic_static_method");
    assert_expr_snap_eq!("let _ = std::io::Buffer::call(a, b);", "namespaced_static_method");
    assert_expr_snap_eq!("let _ = std::Buffer<T>::call(a, b);", "namespaced_generic_ty_method");
    assert_expr_snap_eq!("let _ = std::Buffer::call<T>(a, b);", "namespaced_generic_method");
}

#[test]
fn test_return_snapshots() {
    assert_expr_snap_eq!("return;", "empty");
    assert_expr_snap_eq!("return 1;", "scalar");
    assert_expr_snap_eq!("return a.b(c);", "call");
    assert_expr_err_snap_eq!("return;;", "extra_semi");
}

#[test]
fn test_operator_snapshots() {
    assert_expr_snap_eq!("a + b;", "plus");
    assert_expr_snap_eq!("a - b;", "minus");
    assert_expr_snap_eq!("a * b;", "multiply");
    assert_expr_snap_eq!("a / b;", "divide");
    assert_expr_snap_eq!("a += b;", "plus_assign");
    assert_expr_snap_eq!("a -= b;", "minus_assign");
    assert_expr_snap_eq!("a *= b;", "multiply_assign");
    assert_expr_snap_eq!("a /= b;", "divide_assign");
    assert_expr_snap_eq!("a < b;", "less");
    assert_expr_snap_eq!("a <= b;", "less_equal");
    assert_expr_snap_eq!("a > b;", "greater");
    assert_expr_snap_eq!("a >= b;", "greater_equal");
    assert_expr_snap_eq!("a++;", "increment");
    assert_expr_snap_eq!("a--;", "decrement");
    assert_expr_snap_eq!("a == b;", "equal");
    assert_expr_snap_eq!("a != b;", "not_equal");
    assert_expr_snap_eq!("!a;", "negation");
}

#[test]
fn test_generic_function_snapshots() {
    assert_snap_eq!("fn test() -> void {}", "no_generics");
    assert_snap_eq!("fn test<>() -> void {}", "empty_generics");
    assert_snap_eq!("fn test<T>() -> void {}", "single_generic");
    assert_snap_eq!("fn test<T1, T2>() -> void {}", "multiple_generics");
    assert_snap_eq!("fn test<T: Numeric>() -> void {}", "constrained_generic");
    assert_snap_eq!("fn test<T1: Numeric, T2: Numeric>() -> void {}", "constrained_generics");
    assert_snap_eq!(
        "fn test<T: Numeric + Floating>() -> void {}",
        "multiple_constrained_types"
    );
    assert_snap_eq!(
        "fn test<T1: Numeric + Floating, T2: Numeric + Floating>() -> void {}",
        "multiple_types_with_multiple_constraints"
    );
    assert_snap_eq!("fn test<T1,>() -> void {}", "trailing_generic_comma");
    assert_err_snap_eq!("fn test<T1 T2>() -> void {}", "missing_comma");
}

#[test]
fn test_struct_snapshots() {
    assert_snap_eq!("struct Int32 {}", "empty_decl");
    assert_snap_eq!("impl Int32 {}", "empty_impl");

    assert_err_snap_eq!("struct 1A {}", "invalid_decl_name");
    assert_err_snap_eq!("impl 1A {}", "invalid_impl_name");

    assert_err_snap_eq!("impl Foo { pub x: Bar; }", "property_in_impl");
    assert_err_snap_eq!("struct Foo { pub fn bar() -> void { } }", "method_in_struct");

    assert_snap_eq!(
        "
        impl Foo {
            fn bar() -> Int32 {
                return 0;
            }
        }",
        "method"
    );

    assert_snap_eq!(
        "
        impl Foo {
            fn bar() { }
        }",
        "method_no_ret"
    );

    assert_snap_eq!(
        "
        impl Foo {
            pub fn bar() -> Int32 {
                return 0;
            }
        }",
        "pub_method"
    );

    assert_snap_eq!(
        "
        impl Foo {
            fn external bar() -> Int32
        }",
        "ext_method"
    );

    assert_snap_eq!(
        "
        impl Foo {
            pub fn ==() -> Boolean {
                return true;
            }
        }",
        "operator_method"
    );

    assert_snap_eq!(
        "
        impl Foo {
            fn bar<T>() -> Int32 { }
        }",
        "generic_method"
    );

    assert_snap_eq!(
        "
        impl Foo {
            fn empty?() -> Boolean { }
        }",
        "method_boolean"
    );

    assert_snap_eq!(
        "
        struct Foo {
            x: Int32 = 0;
        }",
        "property"
    );

    assert_err_snap_eq!(
        "
        struct Foo {
            x?: Int32 = 0;
        }",
        "property_invalid_boolean"
    );

    assert_snap_eq!(
        "
        struct Foo {
            x: Int32;
        }",
        "property_no_default"
    );

    assert_snap_eq!(
        "
        struct Foo {
            pub x: Int32 = 1;
        }",
        "pub_property"
    );
}

#[test]
fn test_generic_struct_snapshots() {
    assert_snap_eq!("struct Test {}", "no_generics");
    assert_snap_eq!("struct Test<> {}", "empty_generics");
    assert_snap_eq!("struct Test<T> {}", "single_generic");
    assert_snap_eq!("struct Test<T1, T2> {}", "multiple_generics");
    assert_snap_eq!("struct Test<T: Numeric> {}", "constrained_generic");
    assert_snap_eq!("struct Test<T1: Numeric, T2: Numeric> {}", "constrained_generics");
    assert_snap_eq!("struct Test<T1,> {}", "trailing_generic_comma");
    assert_err_snap_eq!("struct Test<T1 T2> {}", "missing_comma");
}

#[test]
fn test_generic_method_snapshots() {
    assert_snap_eq!("impl Test { fn test() -> void {} }", "no_generics");
    assert_snap_eq!("impl Test { fn test<>() -> void {} }", "empty_generics");
    assert_snap_eq!("impl Test { fn test<T>() -> void {} }", "single_generic");
    assert_snap_eq!("impl Test { fn test<T1, T2>() -> void {} }", "multiple_generics");
    assert_snap_eq!("impl Test { fn test<T: Numeric>() -> void {} }", "constrained_generic");
    assert_snap_eq!(
        "impl Test { fn test<T1: Numeric, T2: Numeric>() -> void {} }",
        "constrained_generics"
    );
    assert_snap_eq!("impl Test { fn test<T1,>() -> void {} }", "trailing_generic_comma");
    assert_err_snap_eq!("impl Test { fn test<T1 T2>() -> void {} }", "missing_comma");
}

#[test]
fn test_enum_snapshots() {
    assert_snap_eq!("enum Foo {}", "empty");
    assert_snap_eq!("enum Foo { Bar }", "single_variant");
    assert_snap_eq!("enum Foo { Bar, Baz }", "multiple_variants");
    assert_err_snap_eq!("enum Foo { Bar Baz }", "missing_comma");
    assert_snap_eq!("enum Foo { Bar, Baz, }", "trailing_comma");

    assert_snap_eq!(
        "
        enum Foo {
            Bar()
        }",
        "variant_param_empty"
    );

    assert_snap_eq!(
        "
        enum Foo {
            Bar(Int32)
        }",
        "variant_param_single"
    );

    assert_snap_eq!(
        "
        enum Foo {
            Bar(Int32, Int32)
        }",
        "variant_param_multiple"
    );

    assert_snap_eq!(
        "
        enum Foo {
            Bar(Int32, Int32),
            Baz(Int32, Int32)
        }",
        "multiple_variants_multiple_params"
    );
}

#[test]
fn test_visibility_snapshots() {
    assert_snap_eq!("fn foo {}", "default");
    assert_snap_eq!("priv fn foo {}", "priv");
    assert_snap_eq!("pub fn foo {}", "pub");
}

#[test]
fn test_trait_snapshots() {
    assert_snap_eq!("trait Add { }", "empty");
    assert_snap_eq!("trait Add { pub fn add(other: Int32) -> Int32; }", "method");
    assert_snap_eq!(
        "trait Add { pub fn add(other: Int32) -> Int32 { return self + other; } }",
        "method_impl"
    );
    assert_snap_eq!("trait Add<T> { }", "generic");
    assert_snap_eq!("trait Add<T1, T2> { }", "generics");
    assert_snap_eq!("trait Add { fn add(other: Int32) -> Int32; }", "private_method");
    assert_snap_eq!("trait Add<T: Numeric> {}", "constrained_generic");
    assert_snap_eq!("trait Add<T1: Numeric, T2: Numeric> {}", "constrained_generics");
    assert_snap_eq!("trait Add { pub fn add(other: Int32) { } }", "method_no_ret");
}

#[test]
fn test_use_trait_snapshots() {
    assert_snap_eq!("use Add in Int32 {}", "empty");

    assert_snap_eq!(
        "
        use Add in Int32 {
            fn add(other: Int32) -> Int32 {
                return self + other;
            }
        }",
        "priv_method"
    );

    assert_snap_eq!(
        "
        use Add in Int32 {
            pub fn add(other: Int32) -> Int32 {
                return self + other;
            }
        }",
        "pub_method"
    );

    assert_snap_eq!(
        "
        use Add in Int32 {
            fn add(other: Int32) {}
        }",
        "method_no_ret"
    );

    assert_snap_eq!(
        "
        use Cast in Int32 {
            pub fn to_string() -> String {
                return self;
            }

            pub fn to_int() -> Int32 {
                return self;
            }
        }",
        "methods"
    );

    assert_snap_eq!(
        "
        use Zero in Int32 {
            pub fn zero?() -> Boolean { }
        }",
        "boolean_method"
    );

    assert_snap_eq!(
        "
        use Add<Int32> in Int32 {
            pub fn add(other: Int32) -> Int32 {
                return self + other;
            }
        }",
        "generic"
    );

    assert_snap_eq!(
        "
        use Add<Int32, Int64> in Int32 {
            pub fn add(other: Int32) -> Int64 {
                return self + other;
            }
        }",
        "generics"
    );

    assert_snap_eq!(
        "
        use Enumerable<T> in Vector<T> {
            pub fn next() -> T { }
        }",
        "generic_type"
    );

    assert_snap_eq!(
        "
        use<T> Enumerable<T> in Vector<T> {
            pub fn next() -> T { }
        }",
        "generic_trait"
    );
}

#[test]
fn test_doc_comments_snapshots() {
    assert_snap_eq!(
        "/// This is a doc comment
        fn foo() -> void { }",
        "function"
    );

    assert_snap_eq!(
        "/// This is a doc comment
        ///
        /// Another line in the doc comment
        fn foo() -> void { }",
        "multiline"
    );

    assert_snap_eq!(
        "/// This is a doc comment
        struct Foo { }",
        "struct"
    );

    assert_snap_eq!(
        "struct Foo {
            /// This is a doc comment
            pub bar: Int32 = 0;
        }",
        "property"
    );

    assert_snap_eq!(
        "impl Foo {
            /// This is a doc comment
            pub fn bar() -> void { }
        }",
        "method"
    );

    assert_snap_eq!(
        "/// This is a doc comment
        trait Foo { }",
        "trait"
    );

    assert_snap_eq!(
        "trait Foo {
            /// This is a doc comment
            pub fn bar() -> void { }
        }",
        "trait_method"
    );

    assert_snap_eq!(
        "/// This is a doc comment
        enum Foo {
            Bar
        }",
        "enum"
    );

    assert_snap_eq!(
        "enum Foo {
            /// This is a doc comment
            Bar
        }",
        "enum_case"
    );
}
