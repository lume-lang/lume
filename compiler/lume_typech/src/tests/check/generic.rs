use crate::assert_typech_snapshot;

#[test]
fn typech_generic_function_return() {
    assert_typech_snapshot!(
        "fn foo<T>(val: T) -> T {
            return val;
        }"
    );
}

#[test]
fn typech_generic_function_call_param() {
    assert_typech_snapshot!(
        "fn foo<T>(val: T) { }

        fn bar() {
            foo<Boolean>(false);
        }"
    );
}

#[test]
fn typech_generic_function_call_incorrect_type() {
    assert_typech_snapshot!(
        "fn foo<T>(val: T) { }

        fn bar() {
            foo<Boolean>(0);
        }"
    );
}

#[test]
fn typech_generic_function_call_constraint() {
    assert_typech_snapshot!(
        "trait A {}

        use A in Boolean { }

        fn foo<T: A>() { }

        fn bar() {
            foo<Boolean>();
        }"
    );
}

#[test]
fn typech_generic_function_call_unsatisfied_constraint() {
    assert_typech_snapshot!(
        "trait A {}

        fn foo<T: A>() { }

        fn bar() {
            foo<Boolean>();
        }"
    );
}

#[test]
fn typech_generic_function_call_unsatisfied_constraints() {
    assert_typech_snapshot!(
        "trait A {}

        trait B {}

        fn foo<T: A + B>() { }

        fn bar() {
            foo<Boolean>();
        }"
    );
}

#[test]
fn typech_generic_function_call_returned_type() {
    assert_typech_snapshot!(
        "fn foo<T>(val: T) -> T {
            return val;
        }

        fn bar() {
            let _: Boolean = foo<Boolean>(false);
        }"
    );
}

#[test]
fn typech_generic_function_call_returned_type_incomp() {
    assert_typech_snapshot!(
        "fn foo<T>(val: T) -> T {
            return val;
        }

        fn bar() {
            let _: Int32 = foo<Boolean>(false);
        }"
    );
}

#[test]
fn typech_generic_method_static_return() {
    assert_typech_snapshot!(
        "struct A {}

        impl A {
            pub fn bar<T>(val:  T) -> T {
                return val;
            }
        }"
    );
}

#[test]
fn typech_generic_method_static_call_param() {
    assert_typech_snapshot!(
        "struct A {}

        impl A {
            pub fn bar<T>(val:  T) { }
        }

        fn bar() {
            A::bar<Boolean>(false);
        }"
    );
}

#[test]
fn typech_generic_method_static_call_incorrect_type() {
    assert_typech_snapshot!(
        "struct A {}

        impl A {
            pub fn bar<T>(val:  T) { }
        }

        fn bar() {
            A::bar<Boolean>(0);
        }"
    );
}

#[test]
fn typech_generic_method_static_call_returned_type() {
    assert_typech_snapshot!(
        "struct A {}

        impl A {
            pub fn bar<T>(val: T) -> T {
                return val;
            }
        }

        fn bar() {
            let _: Boolean = A::bar<Boolean>(false);
        }"
    );
}

#[test]
fn typech_generic_method_static_call_returned_type_incomp() {
    assert_typech_snapshot!(
        "struct A {}

        impl A {
            pub fn bar<T>(val: T) -> T {
                return val;
            }
        }

        fn bar() {
            let _: Int32 = A::bar<Boolean>(false);
        }"
    );
}

#[test]
fn typech_generic_method_static_call_constraint() {
    assert_typech_snapshot!(
        "trait A {}

        use A in Boolean { }

        struct Foo { }

        impl Foo {
            pub fn bar<T: A>(val: T) -> T {
                return val;
            }
        }

        fn bar() {
            Foo::bar<Boolean>(false);
        }"
    );
}

#[test]
fn typech_generic_method_static_call_unsatisfied_constraint() {
    assert_typech_snapshot!(
        "trait A {}

        struct Foo { }

        impl Foo {
            pub fn bar<T: A>(val: T) -> T {
                return val;
            }
        }

        fn bar() {
            Foo::bar<Boolean>(false);
        }"
    );
}

#[test]
fn typech_generic_method_static_call_unsatisfied_constraints() {
    assert_typech_snapshot!(
        "trait A {}

        trait B {}

        struct Foo { }

        impl Foo {
            pub fn bar<T: A + B>(val: T) -> T {
                return val;
            }
        }

        fn bar() {
            Foo::bar<Boolean>(false);
        }"
    );
}

#[test]
fn typech_generic_method_instance_return() {
    assert_typech_snapshot!(
        "struct A {}

        impl A {
            pub fn bar<T>(self, val:  T) -> T {
                return val;
            }
        }"
    );
}

#[test]
fn typech_generic_method_instance_call_param() {
    assert_typech_snapshot!(
        "struct A {}

        impl A {
            pub fn bar<T>(self, val:  T) { }
        }

        fn bar() {
            let a: A = A { };

            a.bar<Boolean>(false);
        }"
    );
}

#[test]
fn typech_generic_method_instance_call_incorrect_type() {
    assert_typech_snapshot!(
        "struct A {}

        impl A {
            pub fn bar<T>(self, val:  T) { }
        }

        fn bar() {
            let a: A = A { };

            a.bar<Boolean>(0);
        }"
    );
}

#[test]
fn typech_generic_method_instance_call_returned_type() {
    assert_typech_snapshot!(
        "struct A {}

        impl A {
            pub fn bar<T>(self, val: T) -> T {
                return val;
            }
        }

        fn bar() {
            let a: A = A { };

            let _: Boolean = a.bar<Boolean>(false);
        }"
    );
}

#[test]
fn typech_generic_method_instance_call_returned_type_incomp() {
    assert_typech_snapshot!(
        "struct A {}

        impl A {
            pub fn bar<T>(self, val: T) -> T {
                return val;
            }
        }

        fn bar() {
            let a: A = A { };

            let _: Int32 = a.bar<Boolean>(false);
        }"
    );
}

#[test]
fn typech_generic_type_method_static_return() {
    assert_typech_snapshot!(
        "struct A<T> {}

        impl<T> A<T> {
            pub fn bar(val:  T) -> T {
                return val;
            }
        }"
    );
}

#[test]
fn typech_generic_type_method_static_call_param() {
    assert_typech_snapshot!(
        "struct A<T> {}

        impl<T> A<T> {
            pub fn bar(val:  T) { }
        }

        fn bar() {
            A<Boolean>::bar(false);
        }"
    );
}

#[test]
fn typech_generic_type_method_static_call_incorrect_type() {
    assert_typech_snapshot!(
        "struct A<T> {}

        impl<T> A<T> {
            pub fn bar(val:  T) { }
        }

        fn bar() {
            A<Boolean>::bar(0);
        }"
    );
}

#[test]
fn typech_generic_type_method_static_call_returned_type() {
    assert_typech_snapshot!(
        "struct A<T> {}

        impl<T> A<T> {
            pub fn bar(val: T) -> T {
                return val;
            }
        }

        fn bar() {
            let _: Boolean = A<Boolean>::bar(false);
        }"
    );
}

#[test]
fn typech_generic_type_method_static_call_returned_type_incomp() {
    assert_typech_snapshot!(
        "struct A<T> {}

        impl<T> A<T> {
            pub fn bar(val: T) -> T {
                return val;
            }
        }

        fn bar() {
            let _: Int32 = A<Boolean>::bar(false);
        }"
    );
}

#[test]
fn typech_generic_type_method_instance_return() {
    assert_typech_snapshot!(
        "struct A<T> {}

        impl<T> A<T> {
            pub fn bar(self, val:  T) -> T {
                return val;
            }
        }"
    );
}

#[test]
fn typech_generic_type_method_instance_call_param() {
    assert_typech_snapshot!(
        "struct A<T> {}

        impl<T> A<T> {
            pub fn bar(self, val:  T) { }
        }

        fn bar() {
            let a: A<Boolean> = A<Boolean> { };

            a.bar(false);
        }"
    );
}

#[test]
fn typech_generic_type_method_instance_call_incorrect_type() {
    assert_typech_snapshot!(
        "struct A<T> {}

        impl<T> A<T> {
            pub fn bar(self, val:  T) { }
        }

        fn bar() {
            let a: A<Boolean> = A<Boolean> { };

            a.bar(0);
        }"
    );
}

#[test]
fn typech_generic_type_method_instance_call_returned_type() {
    assert_typech_snapshot!(
        "struct A<T> {}

        impl<T> A<T> {
            pub fn bar(self, val: T) -> T {
                return val;
            }
        }

        fn bar() {
            let a: A<Boolean> = A<Boolean> { };

            let _: Boolean = a.bar(false);
        }"
    );
}

#[test]
fn typech_generic_type_method_instance_call_returned_type_incomp() {
    assert_typech_snapshot!(
        "struct A<T> {}

        impl<T> A<T> {
            pub fn bar(self, val: T) -> T {
                return val;
            }
        }

        fn bar() {
            let a: A<Boolean> = A<Boolean> { };

            let _: Int32 = a.bar(false);
        }"
    );
}
