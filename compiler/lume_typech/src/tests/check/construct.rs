use crate::assert_typech_snapshot;

#[test]
fn typech_construct_empty_struct() {
    assert_typech_snapshot!(
        "struct A { }

        fn foo() {
            let a = A { };
        }"
    );
}

#[test]
fn typech_construct_struct_prop_single() {
    assert_typech_snapshot!(
        "struct A {
            foo: Boolean;
        }

        fn foo() {
            let a = A { foo: false };
        }"
    );
}

#[test]
fn typech_construct_struct_prop_multiple() {
    assert_typech_snapshot!(
        "struct A {
            foo: Boolean;
            bar: Boolean;
        }

        fn foo() {
            let a = A { foo: false, bar: true };
        }"
    );
}

#[test]
fn typech_construct_struct_type_comp() {
    assert_typech_snapshot!(
        "struct A {
            foo: Boolean;
        }

        fn foo() {
            let a = A { foo: false };
        }"
    );
}

#[test]
fn typech_construct_struct_type_incomp() {
    assert_typech_snapshot!(
        "struct A {
            foo: Boolean;
        }

        fn foo() {
            let a = A { foo: 0 };
        }"
    );
}

#[test]
fn typech_construct_struct_type_missing_field() {
    assert_typech_snapshot!(
        "struct A {
            foo: Boolean;
        }

        fn foo() {
            let a = A { };
        }"
    );
}

#[test]
fn typech_construct_struct_type_extra_field() {
    assert_typech_snapshot!(
        "struct A {
            foo: Boolean;
        }

        fn foo() {
            let a = A { foo: false, bar: 0 };
        }"
    );
}

#[test]
fn typech_construct_struct_type_defalt_value() {
    assert_typech_snapshot!(
        "struct A {
            foo: Boolean = false;
        }

        fn foo() {
            let a = A { };
        }"
    );
}

#[test]
fn typech_construct_struct_type_defalt_value_prov() {
    assert_typech_snapshot!(
        "struct A {
            foo: Boolean = false;
        }

        fn foo() {
            let a = A { foo: true };
        }"
    );
}

#[test]
fn typech_construct_struct_type_defalt_incomp() {
    assert_typech_snapshot!(
        "struct A {
            foo: Boolean = 0;
        }"
    );
}
