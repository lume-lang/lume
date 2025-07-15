use lume_errors::{DiagCtx, DiagOutputFormat};
use lume_parser::Parser;
use lume_span::PackageId;

use super::*;

#[track_caller]
fn lower(input: &str) -> Result<Map> {
    let dcx = DiagCtx::new(DiagOutputFormat::Stubbed);
    let source = Arc::new(SourceFile::internal(input));

    let expressions = dcx
        .with_res(|handle| Parser::new(source.clone(), handle))?
        .parse()
        .unwrap();

    let module_id = PackageId::empty();
    let mut map = Map::empty(module_id);
    let mut item_id = ItemId::empty();

    dcx.with(|handle| LowerModule::lower(&mut map, &mut item_id, source, handle, expressions, true))?;

    Ok(map)
}

#[track_caller]
fn lower_expr(input: &str) -> Result<Vec<lume_hir::Statement>> {
    let dcx = DiagCtx::new(DiagOutputFormat::Stubbed);
    let source = Arc::new(SourceFile::internal(input));

    let mut parser = Parser::new_with_str(input);
    parser.prepare()?;

    let statements = parser.parse_statements()?;

    let module_id = PackageId::empty();
    let item_id = ItemId::empty();
    let mut map = Map::empty(module_id);

    let mut lower = dcx.with_res(|handle| LowerModule::new(&mut map, item_id, source, handle))?;

    Ok(lower.statements(statements))
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

        insta::with_settings!({
            description => $input,
            omit_expression => true
        }, {
            insta::assert_debug_snapshot!(lower($input).unwrap());
        });
    };
}

macro_rules! assert_err_snap_eq {
    (
        $input: expr,
        $($expr:expr),+
    ) => {
        set_snapshot_suffix!( $($expr),+ );

        insta::with_settings!({
            description => $input,
            omit_expression => true
        }, {
            insta::assert_debug_snapshot!(lower($input).unwrap_err());
        });
    };
}

macro_rules! assert_expr_snap_eq {
    (
        $input: expr,
        $($expr:expr),+
    ) => {
        set_snapshot_suffix!( $($expr),+ );

        insta::with_settings!({
            description => $input,
            omit_expression => true
        }, {
            insta::assert_debug_snapshot!(lower_expr($input).unwrap());
        });
    };
}

#[test]
fn test_implicit_imports() {
    for ty in &[
        "Boolean",
        "String",
        "Int8",
        "UInt8",
        "Int16",
        "UInt16",
        "Int32",
        "UInt32",
        "Int64",
        "UInt64",
        "Float",
        "Double",
        "Array<Int32>",
        "Pointer",
        "Range<Int32>",
        "RangeInclusive<Int32>",
    ] {
        assert_snap_eq!(&format!("fn foo() -> {ty} {{ }}"), "{}", ty);
    }
}

#[test]
fn test_function_definition_snapshots() {
    assert_snap_eq!("fn main() -> void {}", "empty");
    assert_snap_eq!("fn main() -> void { let a = 0; }", "statement");
    assert_snap_eq!("fn main() -> void { let a = 0; let b = 1; }", "statements");
    assert_snap_eq!("fn main() {}", "no_return_type");
    assert_snap_eq!("fn main(argc: u8) -> void { }", "parameter");
    assert_snap_eq!("fn main(argc: u8, arcv: [String]) -> void { }", "parameters");
    assert_snap_eq!("fn external main() -> void", "external");
    assert_snap_eq!("pub fn main() -> void {}", "pub_modifier");
    assert_snap_eq!("fn loop() -> void {}", "reserved_keyword");
    assert_snap_eq!("fn main() -> std::Int32 {}", "namespaced_type");
    assert_snap_eq!("fn empty?() {}", "boolean_function");
    assert_snap_eq!("fn foo(...args: Int32) {}", "varargs");
    assert_err_snap_eq!("fn foo(...args: Int32, a: Int32) {}", "varargs_nonlast");
}

#[test]
fn test_cast_snapshots() {
    assert_expr_snap_eq!("0 as u64;", "literal_single");
    assert_expr_snap_eq!("let _ = 0 as u64;", "precedence_assign");
    assert_expr_snap_eq!("0 as u64 == 0;", "precedence_equal");
    assert_expr_snap_eq!("call() as u64;", "invocation");
}

#[test]
fn test_construction_snapshots() {
    assert_expr_snap_eq!("Foo { };", "empty");
    assert_expr_snap_eq!("Self { };", "empty_self");
    assert_expr_snap_eq!("Foo { bar: 0 };", "field");
    assert_expr_snap_eq!("Foo { bar: 0, baz: 1 };", "fields");
    assert_expr_snap_eq!("Foo { bar: 0, };", "trailing_comma");
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
    assert_expr_snap_eq!("let ident = 0;", "ident");
    assert_expr_snap_eq!("let IDENT = 0;", "ident_case");
    assert_expr_snap_eq!("let __IDENT__ = 0;", "ident_underscore");
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
fn test_conditional_snapshots() {
    assert_expr_snap_eq!("if true { }", "if_empty");
    assert_expr_snap_eq!("if true { let a = 1; }", "if_statement");
    assert_expr_snap_eq!("if true { } else if false { }", "if_else_if_empty");
    assert_expr_snap_eq!("if true { } else { }", "if_else_empty");
    assert_expr_snap_eq!("if true { } else if false { } else { }", "if_else_if_else_empty");
    assert_expr_snap_eq!("let a = 0; if a == 1 { }", "equality_empty");
    assert_expr_snap_eq!("let a = 0; if a != 1 { }", "inequality_empty");
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

    assert_expr_snap_eq!("for pattern in [1, 2, 3] { }", "iter_loop_empty");
    assert_expr_snap_eq!("for pattern in [1, 2, 3] { let a = 0; }", "iter_loop_statement");
    assert_expr_snap_eq!("for pattern in [1, 2, 3] { break; }", "iter_loop_break");
    assert_expr_snap_eq!("for pattern in [1, 2, 3] { continue; }", "iter_loop_continue");

    assert_expr_snap_eq!(
        "
        let collection = [1, 2, 3];
        for pattern in collection { }",
        "iter_loop_empty_var"
    );

    assert_expr_snap_eq!(
        "
        let collection = [1, 2, 3];
        for pattern in collection { let a = 0; }",
        "iter_loop_statement_var"
    );

    assert_expr_snap_eq!(
        "
        let collection = [1, 2, 3];
        for pattern in collection { break; }",
        "iter_loop_break_var"
    );

    assert_expr_snap_eq!(
        "
        let collection = [1, 2, 3];
        for pattern in collection { continue; }",
        "iter_loop_continue_var"
    );
}

#[test]
fn test_range_snapshots() {
    assert_expr_snap_eq!("let _ = (0..1);", "literal_exclusive");
    assert_expr_snap_eq!("let _ = (0..=1);", "literal_inclusive");
    assert_expr_snap_eq!(
        "
        let a = 0;
        let b = 1;
        let _ = (a..b);",
        "expr_exclusive"
    );

    assert_expr_snap_eq!(
        "
        let a = 0;
        let b = 1;
        let _ = (a..=b);",
        "expr_inclusive"
    );

    assert_expr_snap_eq!(
        "
        let a = 0;
        let b = 1;
        let _ = ((a + b)..(a + b + 1));",
        "expr_nested_exclusive"
    );

    assert_expr_snap_eq!(
        "
        let a = 0;
        let b = 1;
        let _ = ((a + b)..=(a + b + 1));",
        "expr_nested_inclusive"
    );
}

#[test]
fn test_call_snapshots() {
    assert_expr_snap_eq!("let _ = call();", "function_empty");
    assert_expr_snap_eq!("let _ = call(0);", "function_param_1");
    assert_expr_snap_eq!("let _ = call(0, 1);", "function_param_2");
    assert_expr_snap_eq!("let _ = call<T>(0, 1);", "function_generic");

    assert_expr_snap_eq!("let _ = 1.call();", "method_empty");
    assert_expr_snap_eq!("let _ = 1.call(0);", "method_param_1");
    assert_expr_snap_eq!("let _ = 1.call(0, 1);", "method_param_2");
    assert_expr_snap_eq!("let _ = 1.call<T>(0, 1);", "method_generic");
    assert_expr_snap_eq!("let _ = Int32<T>::call(0, 1);", "generic_static_method");
}

#[test]
fn test_return_snapshots() {
    assert_expr_snap_eq!("return;", "empty");
    assert_expr_snap_eq!("return 1;", "scalar");
    assert_expr_snap_eq!(
        "
        let a = 1;
        let b = 1;
        let c = 1;

        return a.b(c);",
        "call"
    );
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
}

#[test]
fn test_struct_snapshots() {
    assert_snap_eq!("struct Int32 {}", "empty_decl");
    assert_snap_eq!("impl Int32 {}", "empty_impl");

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
                pub fn ==() -> bool {
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
            struct Foo {
                x: Int32 = 0;
            }",
        "property"
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
}

#[test]
fn test_enum_snapshots() {
    assert_snap_eq!("enum Foo {}", "empty");
    assert_snap_eq!("enum Foo { Bar }", "single_variant");
    assert_snap_eq!("enum Foo { Bar, Baz }", "multiple_variants");

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
                Bar(int)
            }",
        "variant_param_single"
    );

    assert_snap_eq!(
        "
            enum Foo {
                Bar(int, int)
            }",
        "variant_param_multiple"
    );

    assert_snap_eq!(
        "
            enum Foo {
                Bar(int, int),
                Baz(int, int)
            }",
        "multiple_variants_multiple_params"
    );
}

#[test]
fn test_trait_snapshots() {
    assert_snap_eq!("trait Add { }", "empty");
    assert_snap_eq!("trait Add { pub fn add(other: int) -> int; }", "method");
    assert_snap_eq!("trait Add { pub fn add(other: int) -> int { } }", "method_impl");
    assert_snap_eq!("trait Add<T> { }", "generic");
    assert_snap_eq!("trait Add<T1, T2> { }", "generics");
    assert_snap_eq!("trait Add { fn add(other: int) -> int; }", "private_method");
    assert_snap_eq!("trait Add<T: Numeric> {}", "constrained_generic");
    assert_snap_eq!("trait Add<T1: Numeric, T2: Numeric> {}", "constrained_generics");
    assert_snap_eq!("trait Add { pub fn add(other: int) { } }", "method_no_ret");
}

#[test]
fn test_use_trait_snapshots() {
    assert_snap_eq!("use Add in Int32 {}", "empty");

    assert_snap_eq!(
        "
            use Add in Int32 {
                fn add(other: Int32) -> Int32 {}
            }",
        "priv_method"
    );

    assert_snap_eq!(
        "
            use Add in Int32 {
                pub fn add(other: Int32) -> Int32 {}
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
                pub fn to_string() -> String {}

                pub fn to_int() -> Int32 {}
            }",
        "methods"
    );

    assert_snap_eq!(
        "
            use Add<Int32> in Int32 {
                pub fn add(other: Int32) -> Int32 {}
            }",
        "generic"
    );

    assert_snap_eq!(
        "
            use Add<Int32, Int64> in Int32 {
                pub fn add(other: Int32) -> Int64 {}
            }",
        "generics"
    );

    assert_snap_eq!(
        "
            use Enumerable<T> in Vector<T> {
                pub fn next() -> T {}
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

#[test]
fn test_using_self_as_parameter() {
    assert_snap_eq!(
        "impl Foo {
        pub fn bar(self, a: Int32) -> void { }
    }",
        "valid"
    );

    assert_err_snap_eq!(
        "impl Foo {
        pub fn bar(a: Int32, self) -> void { }
    }",
        "invalid"
    );
}

#[test]
fn test_using_self_in_function() {
    assert_snap_eq!("fn foo() -> void { }", "valid");

    assert_err_snap_eq!("fn foo(self) -> void { }", "invalid");
}

#[test]
fn test_duplicate_item() {
    assert_snap_eq!("fn foo() { } fn bar() { }", "valid");
    assert_err_snap_eq!("fn foo() { } fn foo() { }", "duplicate_func");
    assert_err_snap_eq!("struct Foo {} struct Foo { }", "duplicate_struct");
    assert_err_snap_eq!("struct Foo {} trait Foo { }", "duplicate_type");
    assert_snap_eq!("fn Foo() {} trait Foo { }", "valid_func_type");
}

#[test]
fn test_duplicate_type_param() {
    assert_snap_eq!("fn foo<T1, T2>() { }", "valid");
    assert_err_snap_eq!("fn foo<T, T>() { }", "duplicate");
}
