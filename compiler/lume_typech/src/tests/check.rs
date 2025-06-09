use crate::assert_typech_snapshot;

#[test]
fn typech_variable_decl_compatible() {
    assert_typech_snapshot!(
        "fn foo() {
        let _: Int32 = 1_i32;
    }"
    );
}

#[test]
fn typech_variable_decl_incompatible() {
    assert_typech_snapshot!(
        "fn foo() {
        let _: Int32 = false;
    }"
    );
}
