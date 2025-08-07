use crate::assert_typech_snapshot;

#[test]
fn typech_trait_simple() {
    assert_typech_snapshot!(
        "struct A { }

        trait Foo {}

        use Foo in A { }"
    );
}

#[test]
fn typech_trait_single_method() {
    assert_typech_snapshot!(
        "struct A { }

        trait Foo {
            fn foo();
        }

        use Foo in A {
            fn foo() { }
        }"
    );
}

#[test]
fn typech_trait_missing_method() {
    assert_typech_snapshot!(
        "struct A { }

        trait Foo {
            fn foo();
        }

        use Foo in A { }"
    );
}

#[test]
fn typech_trait_extraneous_method() {
    assert_typech_snapshot!(
        "struct A { }

        trait Foo { }

        use Foo in A {
            fn foo() { }
        }"
    );
}

#[test]
fn typech_trait_self_type() {
    assert_typech_snapshot!(
        "struct A { }

        trait Foo {
            fn foo(self);
        }

        use Foo in A {
            fn foo(self) { }
        }"
    );
}
