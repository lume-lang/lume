use lume_hir::{Path, PathSegment};
use lume_span::{Internable, Location};
use lume_types::TypeRef;

use super::*;

#[test]
fn query_function_name_rooted() -> Result<()> {
    let tcx = type_infer("fn foo() { }")?;
    let funcs = tcx.probe_functions(&Path::rooted(PathSegment::callable("foo")));

    assert_eq!(funcs.len(), 1);

    let func = funcs.first().unwrap();

    assert_eq!(func.name, Path::rooted(PathSegment::callable("foo")));
    assert_eq!(func.parameters.len(), 0);
    assert_eq!(func.return_type, TypeRef::void());

    Ok(())
}

#[test]
fn query_function_name_rooted_name_mismatch() -> Result<()> {
    let tcx = type_infer("fn fooo() { }")?;
    let funcs = tcx.probe_functions(&Path::rooted(PathSegment::callable("foo")));

    assert_eq!(funcs.len(), 0);

    Ok(())
}

#[test]
fn query_function_check_empty() -> Result<()> {
    let tcx = type_infer("fn foo() { }")?;
    let funcs = tcx.probe_functions(&Path::rooted(PathSegment::callable("foo")));
    let func = funcs.first().unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::NodeId::default(),
        name: Path::rooted(PathSegment::callable("foo")),
        arguments: Vec::new(),
        location: lume_span::source::Location {
            file: Arc::new(SourceFile::internal("foo();")),
            index: 0..6,
        }
        .intern(),
    };

    tcx.check_function(func, &expr)?;

    crate::tests::assert_dcx_snapshot!(tcx.dcx());

    Ok(())
}

#[test]
fn query_function_check_arg_count() -> Result<()> {
    let tcx = type_infer("fn foo(x: Int32) { }")?;
    let funcs = tcx.probe_functions(&Path::rooted(PathSegment::callable("foo")));
    let func = funcs.first().unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::NodeId::default(),
        name: Path::rooted(PathSegment::callable("foo")),
        arguments: Vec::new(),
        location: lume_span::source::Location {
            file: Arc::new(SourceFile::internal("foo();")),
            index: 0..6,
        }
        .intern(),
    };

    tcx.check_function(func, &expr)?;

    crate::tests::assert_dcx_snapshot!(tcx.dcx());

    Ok(())
}

#[test]
fn query_function_check_type_arg_type_count() -> Result<()> {
    let tcx = type_infer("fn foo<T>() { }")?;
    let funcs = tcx.probe_functions(&Path::rooted(PathSegment::callable("foo")));
    let func = funcs.first().unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::NodeId::default(),
        name: Path::rooted(PathSegment::callable("foo")),
        arguments: Vec::new(),
        location: lume_span::source::Location {
            file: Arc::new(SourceFile::internal("foo();")),
            index: 0..6,
        }
        .intern(),
    };

    tcx.check_function(func, &expr)?;

    crate::tests::assert_dcx_snapshot!(tcx.dcx());

    Ok(())
}

#[test]
fn query_function_check_type_arg_type_match() -> Result<()> {
    let tcx = type_infer("fn foo<T>() { }")?;
    let funcs = tcx.probe_functions(&Path::rooted(PathSegment::callable("foo")));
    let func = funcs.first().unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::NodeId::default(),
        name: Path::rooted(PathSegment::Callable {
            name: "foo".into(),
            type_arguments: vec![lume_hir::Type::void()],
            location: Location::empty(),
        }),
        arguments: Vec::new(),
        location: Location::empty(),
    };

    tcx.check_function(func, &expr)?;

    crate::tests::assert_dcx_snapshot!(tcx.dcx());

    Ok(())
}

#[test]
fn query_methods_on_type_empty() -> Result<()> {
    let tcx = type_infer("struct A {} impl A {}")?;

    let ty = tcx.tdb().find_type(&Path::rooted(PathSegment::ty("A"))).unwrap();
    let methods = tcx
        .methods_defined_on(&lume_types::TypeRef::new(ty.id, Location::empty()))
        .collect::<Vec<_>>();

    assert_eq!(methods.len(), 0);

    Ok(())
}

#[test]
fn query_methods_on_type_single() -> Result<()> {
    let tcx = type_infer("struct A {} impl A { pub fn foo() { } }")?;

    let ty = tcx.tdb().find_type(&Path::rooted(PathSegment::ty("A"))).unwrap();
    let methods = tcx
        .methods_defined_on(&lume_types::TypeRef::new(ty.id, Location::empty()))
        .collect::<Vec<_>>();

    assert_eq!(methods.len(), 1);

    let method = methods.first().unwrap();

    assert_eq!(
        method.name,
        Path::from_parts(Some([PathSegment::ty("A")]), PathSegment::callable("foo"))
    );
    assert_eq!(method.parameters.len(), 0);
    assert_eq!(method.return_type, TypeRef::void());

    Ok(())
}

#[test]
fn query_methods_on_type_single_impl() -> Result<()> {
    let tcx = type_infer("struct A {} impl A { pub fn foo() { } pub fn bar() { } }")?;

    let ty = tcx.tdb().find_type(&Path::rooted(PathSegment::ty("A"))).unwrap();
    let methods = tcx
        .methods_defined_on(&lume_types::TypeRef::new(ty.id, Location::empty()))
        .collect::<Vec<_>>();

    assert_eq!(methods.len(), 2);

    assert_eq!(
        methods[0].name,
        Path::from_parts(Some([PathSegment::ty("A")]), PathSegment::callable("foo"))
    );
    assert_eq!(methods[0].parameters.len(), 0);
    assert_eq!(methods[0].return_type, TypeRef::void());

    assert_eq!(
        methods[1].name,
        Path::from_parts(Some([PathSegment::ty("A")]), PathSegment::callable("bar"))
    );
    assert_eq!(methods[1].parameters.len(), 0);
    assert_eq!(methods[1].return_type, TypeRef::void());

    Ok(())
}

#[test]
fn query_methods_on_type_multiple_impl() -> Result<()> {
    let tcx = type_infer("struct A {} impl A { pub fn foo() { } } impl A { pub fn bar() { } }")?;

    let ty = tcx.tdb().find_type(&Path::rooted(PathSegment::ty("A"))).unwrap();
    let methods = tcx
        .methods_defined_on(&lume_types::TypeRef::new(ty.id, Location::empty()))
        .collect::<Vec<_>>();

    assert_eq!(methods.len(), 2);

    assert_eq!(
        methods[0].name,
        Path::from_parts(Some([PathSegment::ty("A")]), PathSegment::callable("foo"))
    );
    assert_eq!(methods[0].parameters.len(), 0);
    assert_eq!(methods[0].return_type, TypeRef::void());

    assert_eq!(
        methods[1].name,
        Path::from_parts(Some([PathSegment::ty("A")]), PathSegment::callable("bar"))
    );
    assert_eq!(methods[1].parameters.len(), 0);
    assert_eq!(methods[1].return_type, TypeRef::void());

    Ok(())
}

#[test]
fn query_check_method_empty() -> Result<()> {
    let tcx = type_infer(
        "struct A {}

        impl A {
            pub fn foo() { }
        }",
    )?;

    let method = tcx
        .tdb()
        .find_method(&Path::from_parts(
            Some([PathSegment::ty("A")]),
            PathSegment::callable("foo"),
        ))
        .unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::NodeId::default(),
        name: Path::from_parts(Some([PathSegment::ty("A")]), PathSegment::callable("foo")),
        arguments: Vec::new(),
        location: lume_span::source::Location {
            file: Arc::new(SourceFile::internal("A::foo();")),
            index: 0..9,
        }
        .intern(),
    };

    tcx.check_method(method, lume_hir::CallExpression::Static(&expr))?;

    crate::tests::assert_dcx_snapshot!(tcx.dcx());

    Ok(())
}

#[test]
fn query_check_method_arg_count() -> Result<()> {
    let tcx = type_infer(
        "struct A {}

        impl A {
            pub fn foo(x: Int32) { }
        }",
    )?;

    let method = tcx
        .tdb()
        .find_method(&Path::from_parts(
            Some([PathSegment::ty("A")]),
            PathSegment::callable("foo"),
        ))
        .unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::NodeId::default(),
        name: Path::from_parts(Some([PathSegment::ty("A")]), PathSegment::callable("foo")),
        arguments: Vec::new(),
        location: lume_span::source::Location {
            file: Arc::new(SourceFile::internal("A::foo();")),
            index: 0..9,
        }
        .intern(),
    };

    tcx.check_method(method, lume_hir::CallExpression::Static(&expr))?;

    crate::tests::assert_dcx_snapshot!(tcx.dcx());

    Ok(())
}

#[test]
fn query_check_method_type_arg_count() -> Result<()> {
    let tcx = type_infer(
        "struct A {}

        impl A {
            pub fn foo<T>() { }
        }",
    )?;

    let method = tcx
        .tdb()
        .find_method(&Path::from_parts(
            Some([PathSegment::ty("A")]),
            PathSegment::callable("foo"),
        ))
        .unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::NodeId::default(),
        name: Path::from_parts(Some([PathSegment::ty("A")]), PathSegment::callable("foo")),
        arguments: Vec::new(),
        location: lume_span::source::Location {
            file: Arc::new(SourceFile::internal("A::foo();")),
            index: 0..9,
        }
        .intern(),
    };

    tcx.check_method(method, lume_hir::CallExpression::Static(&expr))?;

    crate::tests::assert_dcx_snapshot!(tcx.dcx());

    Ok(())
}

#[test]
fn query_check_method_type_arg_valid() -> Result<()> {
    let tcx = type_infer(
        "struct A {}

        impl A {
            pub fn foo<T>() { }
        }",
    )?;

    let method = tcx
        .tdb()
        .find_method(&Path::from_parts(
            Some([PathSegment::ty("A")]),
            PathSegment::callable("foo"),
        ))
        .unwrap();

    let expr = lume_hir::StaticCall {
        id: lume_span::NodeId::default(),
        name: Path::from_parts(
            Some([PathSegment::ty("A")]),
            PathSegment::Callable {
                name: "foo".into(),
                type_arguments: vec![lume_hir::Type::void()],
                location: Location::empty(),
            },
        ),
        arguments: Vec::new(),
        location: Location::empty(),
    };

    assert!(tcx.check_method(method, lume_hir::CallExpression::Static(&expr))?);

    Ok(())
}

#[test]
fn query_lookup_functions_single() -> Result<()> {
    let tcx = type_infer("fn foo() { }")?;

    let expr = lume_hir::StaticCall {
        id: lume_span::NodeId::default(),
        name: Path::rooted(PathSegment::callable("foo")),
        arguments: Vec::new(),
        location: Location::empty(),
    };

    let func = tcx.lookup_functions(&expr);
    assert!(func.is_ok());

    let func = func.unwrap();
    assert_eq!(func.name, Path::rooted(PathSegment::callable("foo")));
    assert_eq!(func.parameters.len(), 0);
    assert_eq!(func.type_parameters.len(), 0);
    assert_eq!(func.return_type, TypeRef::void());

    Ok(())
}

#[test]
fn query_lookup_functions_missing() -> Result<()> {
    let tcx = type_infer("fn bar() { }")?;

    let expr = lume_hir::StaticCall {
        id: lume_span::NodeId::default(),
        name: Path::rooted(PathSegment::callable("foo")),
        arguments: Vec::new(),
        location: Location::empty(),
    };

    let func = tcx.lookup_functions(&expr);
    assert!(func.is_err());

    let err = func.unwrap_err();
    insta::assert_debug_snapshot!(err);

    Ok(())
}

#[test]
fn query_lookup_functions_suggestion_name_mismatch() -> Result<()> {
    let tcx = type_infer("fn foo_() { }")?;

    let expr = lume_hir::StaticCall {
        id: lume_span::NodeId::default(),
        name: Path::rooted(PathSegment::callable("foo")),
        arguments: Vec::new(),
        location: Location::empty(),
    };

    let func = tcx.lookup_functions(&expr);
    assert!(func.is_err());

    let err = func.unwrap_err();
    insta::assert_debug_snapshot!(err);

    Ok(())
}

#[test]
fn query_lookup_functions_suggestion_arg_count() -> Result<()> {
    let tcx = type_infer("fn foo(x: Int32) { }")?;

    let expr = lume_hir::StaticCall {
        id: lume_span::NodeId::default(),
        name: Path::rooted(PathSegment::callable("foo")),
        arguments: Vec::new(),
        location: Location::empty(),
    };

    tcx.lookup_functions(&expr)?;

    crate::tests::assert_dcx_snapshot!(tcx.dcx());

    Ok(())
}

#[test]
fn query_lookup_functions_suggestion_type_arg_count() -> Result<()> {
    let tcx = type_infer("fn foo<T>() { }")?;

    let expr = lume_hir::StaticCall {
        id: lume_span::NodeId::default(),
        name: Path::rooted(PathSegment::callable("foo")),
        arguments: Vec::new(),
        location: Location::empty(),
    };

    tcx.lookup_functions(&expr)?;

    crate::tests::assert_dcx_snapshot!(tcx.dcx());

    Ok(())
}
