use crate::assert_typech_snapshot;

#[test]
fn typech_create_enum() {
    assert_typech_snapshot!(
        "enum Foo { A, B }

        fn foo() {
            let a = Foo::A;
        }"
    );
}

#[test]
fn typech_create_enum_param_empty() {
    assert_typech_snapshot!(
        "enum Foo { A, B }

        fn foo() {
            let a = Foo::A();
        }"
    );
}

#[test]
fn typech_create_enum_param() {
    assert_typech_snapshot!(
        "enum Foo { A, B(Int32) }

        fn foo() {
            let a = Foo::B(42);
        }"
    );
}

#[test]
fn typech_create_enum_param_incomp() {
    assert_typech_snapshot!(
        "enum Foo { A, B(Int32) }

        fn foo() {
            let a = Foo::B(false);
        }"
    );
}

#[test]
fn typech_create_enum_params_incomp() {
    assert_typech_snapshot!(
        "enum Foo { A, B(Int32, Boolean) }

        fn foo() {
            let a = Foo::B(false, 0);
        }"
    );
}

#[test]
fn typech_create_enum_param_missing() {
    assert_typech_snapshot!(
        "enum Foo { A, B(Int32) }

        fn foo() {
            let a = Foo::B;
        }"
    );
}

#[test]
fn typech_create_enum_param_extra() {
    assert_typech_snapshot!(
        "enum Foo { A, B }

        fn foo() {
            let a = Foo::B(false);
        }"
    );
}
