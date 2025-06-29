use crate::assert_typech_snapshot;

#[test]
fn typech_variable_decl_comp() {
    assert_typech_snapshot!(
        "fn foo() {
        let _: Int32 = 1_i32;
    }"
    );
}

#[test]
fn typech_variable_decl_incomp() {
    assert_typech_snapshot!(
        "fn foo() {
        let _: Int32 = false;
    }"
    );
}

#[test]
fn typech_return_value_inline_comp() {
    assert_typech_snapshot!(
        "fn foo() -> Int32 {
        return 0_i32;
    }"
    );
}

#[test]
fn typech_return_value_inline_incomp() {
    assert_typech_snapshot!(
        "fn foo() -> Int32 {
        return true;
    }"
    );
}

#[test]
fn typech_return_value_condition_if() {
    assert_typech_snapshot!(
        "fn foo() -> Int32 {
        if false {
            return true;
        }
        return 0_i32;
    }"
    );
}

#[test]
fn typech_return_value_condition_unless() {
    assert_typech_snapshot!(
        "fn foo() -> Int32 {
        unless false {
            return true;
        }
        return 0_i32;
    }"
    );
}

#[test]
fn typech_return_value_condition_inf_loop() {
    assert_typech_snapshot!(
        "fn foo() -> Int32 {
        loop {
            return true;
        }
        return 0_i32;
    }"
    );
}

#[test]
fn typech_return_value_condition_pred_loop() {
    assert_typech_snapshot!(
        "fn foo() -> Int32 {
        while true {
            return true;
        }
        return 0_i32;
    }"
    );
}

#[test]
fn typech_return_value_condition_iter_loop() {
    assert_typech_snapshot!(
        "fn foo() -> Int32 {
        for x in [] {
            return true;
        }
        return 0_i32;
    }"
    );
}

#[test]
fn typech_return_value_call_expr() {
    assert_typech_snapshot!(
        "fn foo() -> Boolean {
            return false;
        }

        fn bar() -> Int32 {
            return foo();
        }"
    );
}
