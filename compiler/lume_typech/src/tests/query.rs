use lume_hir::SymbolName;
use lume_types::TypeRef;

use crate::query::lookup::{CallableCheckError, CallableCheckResult};

use super::*;

#[test]
fn query_function_name_rooted() -> Result<()> {
    let (tcx, _) = type_infer("fn foo() { }")?;
    let funcs = tcx.lookup_functions_unchecked(&SymbolName::rooted("foo"));

    assert_eq!(funcs.len(), 1);

    let func = funcs.first().unwrap();

    assert_eq!(func.name, SymbolName::rooted("foo"));
    assert_eq!(func.parameters.len(), 0);
    assert_eq!(func.return_type, TypeRef::void());

    Ok(())
}

#[test]
fn query_function_name_rooted_name_mismatch() -> Result<()> {
    let (tcx, _) = type_infer("fn fooo() { }")?;
    let funcs = tcx.lookup_functions_unchecked(&SymbolName::rooted("foo"));

    assert_eq!(funcs.len(), 0);

    Ok(())
}

#[test]
fn query_function_check_empty() -> Result<()> {
    let (tcx, hir) = type_infer("fn foo() { }")?;
    let funcs = tcx.lookup_functions_unchecked(&SymbolName::rooted("foo"));
    let func = funcs.first().unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::rooted("foo"),
        arguments: Vec::new(),
        type_arguments: Vec::new(),
    };

    assert_eq!(tcx.check_function(&hir, func, &expr)?, CallableCheckResult::Success);

    Ok(())
}

#[test]
fn query_function_check_arg_count() -> Result<()> {
    let (tcx, hir) = type_infer("fn foo(x: Int32) { }")?;
    let funcs = tcx.lookup_functions_unchecked(&SymbolName::rooted("foo"));
    let func = funcs.first().unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::rooted("foo"),
        arguments: Vec::new(),
        type_arguments: Vec::new(),
    };

    assert_eq!(
        tcx.check_function(&hir, func, &expr)?,
        CallableCheckResult::Failure(vec![CallableCheckError::ArgumentCountMismatch])
    );

    Ok(())
}

#[test]
fn query_function_check_arg_type() -> Result<()> {
    let (tcx, hir) = type_infer("fn foo(x: Int32) { }")?;
    let funcs = tcx.lookup_functions_unchecked(&SymbolName::rooted("foo"));
    let func = funcs.first().unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::rooted("foo"),
        arguments: vec![lume_hir::Expression::lit_bool(false)],
        type_arguments: Vec::new(),
    };

    assert_eq!(
        tcx.check_function(&hir, func, &expr)?,
        CallableCheckResult::Failure(vec![CallableCheckError::ArgumentTypeMismatch(0)])
    );

    Ok(())
}

#[test]
fn query_function_check_arg_type_second() -> Result<()> {
    let (tcx, hir) = type_infer("fn foo(x: Boolean, y: Int32) { }")?;
    let funcs = tcx.lookup_functions_unchecked(&SymbolName::rooted("foo"));
    let func = funcs.first().unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::rooted("foo"),
        arguments: vec![
            lume_hir::Expression::lit_bool(false),
            lume_hir::Expression::lit_bool(false),
        ],
        type_arguments: Vec::new(),
    };

    assert_eq!(
        tcx.check_function(&hir, func, &expr)?,
        CallableCheckResult::Failure(vec![CallableCheckError::ArgumentTypeMismatch(1)])
    );

    Ok(())
}

#[test]
fn query_function_check_arg_type_match() -> Result<()> {
    let (tcx, hir) = type_infer("fn foo(x: Boolean, y: Int32) { }")?;
    let funcs = tcx.lookup_functions_unchecked(&SymbolName::rooted("foo"));
    let func = funcs.first().unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::rooted("foo"),
        arguments: vec![lume_hir::Expression::lit_bool(false), lume_hir::Expression::lit_i32(1)],
        type_arguments: Vec::new(),
    };

    assert_eq!(tcx.check_function(&hir, func, &expr)?, CallableCheckResult::Success);

    Ok(())
}

#[test]
fn query_function_check_type_arg_type_count() -> Result<()> {
    let (tcx, hir) = type_infer("fn foo<T>() { }")?;
    let funcs = tcx.lookup_functions_unchecked(&SymbolName::rooted("foo"));
    let func = funcs.first().unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::rooted("foo"),
        arguments: Vec::new(),
        type_arguments: Vec::new(),
    };

    assert_eq!(
        tcx.check_function(&hir, func, &expr)?,
        CallableCheckResult::Failure(vec![CallableCheckError::TypeParameterCountMismatch])
    );

    Ok(())
}

#[test]
fn query_function_check_type_arg_type_match() -> Result<()> {
    let (tcx, hir) = type_infer("fn foo<T>() { }")?;
    let funcs = tcx.lookup_functions_unchecked(&SymbolName::rooted("foo"));
    let func = funcs.first().unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::rooted("foo"),
        arguments: Vec::new(),
        type_arguments: vec![lume_hir::TypeArgument::Implicit {
            location: lume_span::Location::empty(),
        }],
    };

    assert_eq!(tcx.check_function(&hir, func, &expr)?, CallableCheckResult::Success);

    Ok(())
}

#[test]
fn query_methods_on_type_empty() -> Result<()> {
    let (tcx, _) = type_infer("struct A {} impl A {}")?;

    let ty = tcx.tcx.find_type(&SymbolName::rooted("A")).unwrap();
    let methods = tcx.methods_defined_on(&lume_types::TypeRef::new(ty.id));

    assert_eq!(methods.len(), 0);

    Ok(())
}

#[test]
fn query_methods_on_type_single() -> Result<()> {
    let (tcx, _) = type_infer("struct A {} impl A { pub fn foo() { } }")?;

    let ty = tcx.tcx.find_type(&SymbolName::rooted("A")).unwrap();
    let methods = tcx.methods_defined_on(&lume_types::TypeRef::new(ty.id));

    assert_eq!(methods.len(), 1);

    let method = methods.first().unwrap();

    assert_eq!(method.name, SymbolName::from_parts(Some(["A"]), "foo"));
    assert_eq!(method.parameters.len(), 0);
    assert_eq!(method.return_type, TypeRef::void());

    Ok(())
}

#[test]
fn query_methods_on_type_single_impl() -> Result<()> {
    let (tcx, _) = type_infer("struct A {} impl A { pub fn foo() { } pub fn bar() { } }")?;

    let ty = tcx.tcx.find_type(&SymbolName::rooted("A")).unwrap();
    let methods = tcx.methods_defined_on(&lume_types::TypeRef::new(ty.id));

    assert_eq!(methods.len(), 2);

    assert_eq!(methods[0].name, SymbolName::from_parts(Some(["A"]), "foo"));
    assert_eq!(methods[0].parameters.len(), 0);
    assert_eq!(methods[0].return_type, TypeRef::void());

    assert_eq!(methods[1].name, SymbolName::from_parts(Some(["A"]), "bar"));
    assert_eq!(methods[1].parameters.len(), 0);
    assert_eq!(methods[1].return_type, TypeRef::void());

    Ok(())
}

#[test]
fn query_methods_on_type_multiple_impl() -> Result<()> {
    let (tcx, _) = type_infer("struct A {} impl A { pub fn foo() { } } impl A { pub fn bar() { } }")?;

    let ty = tcx.tcx.find_type(&SymbolName::rooted("A")).unwrap();
    let methods = tcx.methods_defined_on(&lume_types::TypeRef::new(ty.id));

    assert_eq!(methods.len(), 2);

    assert_eq!(methods[0].name, SymbolName::from_parts(Some(["A"]), "foo"));
    assert_eq!(methods[0].parameters.len(), 0);
    assert_eq!(methods[0].return_type, TypeRef::void());

    assert_eq!(methods[1].name, SymbolName::from_parts(Some(["A"]), "bar"));
    assert_eq!(methods[1].parameters.len(), 0);
    assert_eq!(methods[1].return_type, TypeRef::void());

    Ok(())
}

#[test]
fn query_check_method_empty() -> Result<()> {
    let (tcx, hir) = type_infer(
        "struct A {}

        impl A {
            pub fn foo() { }
        }",
    )?;

    let method = tcx
        .tcx
        .find_method(&SymbolName::from_parts(Some(["A"]), "foo"))
        .unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::from_parts(Some(["A"]), "foo"),
        arguments: Vec::new(),
        type_arguments: Vec::new(),
    };

    assert_eq!(
        tcx.check_method(&hir, method, &lume_hir::CallExpression::Static(&expr)),
        Ok(CallableCheckResult::Success)
    );

    Ok(())
}

#[test]
fn query_check_method_arg_count() -> Result<()> {
    let (tcx, hir) = type_infer(
        "struct A {}

        impl A {
            pub fn foo(x: Int32) { }
        }",
    )?;

    let method = tcx
        .tcx
        .find_method(&SymbolName::from_parts(Some(["A"]), "foo"))
        .unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::from_parts(Some(["A"]), "foo"),
        arguments: Vec::new(),
        type_arguments: Vec::new(),
    };

    assert_eq!(
        tcx.check_method(&hir, method, &lume_hir::CallExpression::Static(&expr)),
        Ok(CallableCheckResult::Failure(vec![
            CallableCheckError::ArgumentCountMismatch
        ]))
    );

    Ok(())
}

#[test]
fn query_check_method_arg_type() -> Result<()> {
    let (tcx, hir) = type_infer(
        "struct A {}

        impl A {
            pub fn foo(x: Int32) { }
        }",
    )?;

    let method = tcx
        .tcx
        .find_method(&SymbolName::from_parts(Some(["A"]), "foo"))
        .unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::from_parts(Some(["A"]), "foo"),
        arguments: vec![lume_hir::Expression::lit_bool(false)],
        type_arguments: Vec::new(),
    };

    assert_eq!(
        tcx.check_method(&hir, method, &lume_hir::CallExpression::Static(&expr)),
        Ok(CallableCheckResult::Failure(vec![
            CallableCheckError::ArgumentTypeMismatch(0)
        ]))
    );

    Ok(())
}

#[test]
fn query_check_method_type_arg_count() -> Result<()> {
    let (tcx, hir) = type_infer(
        "struct A {}

        impl A {
            pub fn foo<T>() { }
        }",
    )?;

    let method = tcx
        .tcx
        .find_method(&SymbolName::from_parts(Some(["A"]), "foo"))
        .unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::from_parts(Some(["A"]), "foo"),
        arguments: Vec::new(),
        type_arguments: Vec::new(),
    };

    assert_eq!(
        tcx.check_method(&hir, method, &lume_hir::CallExpression::Static(&expr)),
        Ok(CallableCheckResult::Failure(vec![
            CallableCheckError::TypeParameterCountMismatch
        ]))
    );

    Ok(())
}

#[test]
fn query_check_method_type_arg_valid() -> Result<()> {
    let (tcx, hir) = type_infer(
        "struct A {}

        impl A {
            pub fn foo<T>() { }
        }",
    )?;

    let method = tcx
        .tcx
        .find_method(&SymbolName::from_parts(Some(["A"]), "foo"))
        .unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::from_parts(Some(["A"]), "foo"),
        arguments: Vec::new(),
        type_arguments: vec![lume_hir::TypeArgument::Implicit {
            location: lume_span::Location::empty(),
        }],
    };

    assert_eq!(
        tcx.check_method(&hir, method, &lume_hir::CallExpression::Static(&expr)),
        Ok(CallableCheckResult::Success)
    );

    Ok(())
}

#[test]
fn query_lookup_functions_single() -> Result<()> {
    let (tcx, hir) = type_infer("fn foo() { }")?;

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::rooted("foo"),
        arguments: Vec::new(),
        type_arguments: Vec::new(),
    };

    let func = tcx.lookup_functions(&hir, &expr);
    assert!(func.is_ok());

    let func = func.unwrap();
    assert_eq!(func.name, SymbolName::rooted("foo"));
    assert_eq!(func.parameters.len(), 0);
    assert_eq!(func.type_parameters.len(), 0);
    assert_eq!(func.return_type, TypeRef::void());

    Ok(())
}

#[test]
fn query_lookup_functions_missing() -> Result<()> {
    let (tcx, hir) = type_infer("fn bar() { }")?;

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::rooted("foo"),
        arguments: Vec::new(),
        type_arguments: Vec::new(),
    };

    let func = tcx.lookup_functions(&hir, &expr);
    assert!(func.is_err());

    let err = func.unwrap_err();
    insta::assert_debug_snapshot!(err);

    Ok(())
}

#[test]
fn query_lookup_functions_suggestion_name_mismatch() -> Result<()> {
    let (tcx, hir) = type_infer("fn foo_() { }")?;

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::rooted("foo"),
        arguments: Vec::new(),
        type_arguments: Vec::new(),
    };

    let func = tcx.lookup_functions(&hir, &expr);
    assert!(func.is_err());

    let err = func.unwrap_err();
    insta::assert_debug_snapshot!(err);

    Ok(())
}

#[test]
fn query_lookup_functions_suggestion_arg_count() -> Result<()> {
    let (tcx, hir) = type_infer("fn foo(x: Int32) { }")?;

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::rooted("foo"),
        arguments: Vec::new(),
        type_arguments: Vec::new(),
    };

    let func = tcx.lookup_functions(&hir, &expr);
    assert!(func.is_err());

    let err = func.unwrap_err();
    insta::assert_debug_snapshot!(err);

    Ok(())
}

#[test]
fn query_lookup_functions_suggestion_arg_mismatch() -> Result<()> {
    let (tcx, hir) = type_infer("fn foo(x: Int32) { }")?;

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::rooted("foo"),
        arguments: vec![lume_hir::Expression::lit_bool(false)],
        type_arguments: Vec::new(),
    };

    let func = tcx.lookup_functions(&hir, &expr);
    assert!(func.is_err());

    let err = func.unwrap_err();
    insta::assert_debug_snapshot!(err);

    Ok(())
}

#[test]
fn query_lookup_functions_suggestion_type_arg_count() -> Result<()> {
    let (tcx, hir) = type_infer("fn foo<T>() { }")?;

    let expr = lume_hir::StaticCall {
        id: lume_span::ExpressionId::default(),
        name: SymbolName::rooted("foo"),
        arguments: Vec::new(),
        type_arguments: Vec::new(),
    };

    let func = tcx.lookup_functions(&hir, &expr);
    assert!(func.is_err());

    let err = func.unwrap_err();
    insta::assert_debug_snapshot!(err);

    Ok(())
}
