use lume_errors::{Result, SimpleDiagnostic};
use lume_span::{NodeId, PackageId};
use lume_typech::TyCheckCtx;

/// Prefix for all mangled names, indicating that the name is mangled, along
/// with the current version being used for the mangling.
pub const MANGLED_PREFIX: &str = "_L1";

/// Indicator for the start of the package segment, defining which package the
/// symbol comes from.
pub const PACKAGE_INDICATOR: &str = "P";

/// Indicator for the start of the path segment, defining the full path of the
/// symbol (save for any type parameters).
pub const PATH_INDICATOR: &str = "N";

/// Indicator for the start of the implementor segment, defining the type on
/// which a method is implemented on. This is only used for methods and trait
/// implementation methods.
pub const IMPL_INDICATOR: &str = "I";

/// Indicator for the start of the type segment, defining a type name within an
/// implementor segment.
pub const TYPE_INDICATOR: &str = "Y";

/// Indicator for the start of the type constraint sub-segment, defining the
/// name of a constraint within a type segment.
pub const CONSTRAINT_INDICATOR: &str = "c";

/// Indicator for the start of the name segment, defining the name of a method
/// without it's implementing type. This is only used for methods and trait
/// implementation methods.
pub const NAME_INDICATOR: &str = "M";

/// Key for when the referenced symbol is a callable function definition.
pub const FUNCTION_SYM: &str = "_Cf";

/// Key for when the referenced symbol is a callable method definition, coming
/// from an `impl` block.
pub const METHOD_SYM: &str = "_Ci";

/// Key for when the referenced symbol is a callable method definition, coming
/// from a trait definition.
pub const TRAIT_METHOD_DEF_SYM: &str = "_Ct";

/// Key for when the referenced symbol is a callable method definition, coming
/// from a trait implementation.
pub const TRAIT_METHOD_IMPL_SYM: &str = "_Cs";

/// Key for when the referenced symbol is a `struct` type definition.
pub const STRUCT_TYPE_SYM: &str = "_S";

/// Key for when the referenced symbol is a `trait` type definition.
pub const TRAIT_TYPE_SYM: &str = "_Tt";

/// Key for when the referenced symbol is a type parameter definition.
pub const PARAM_TYPE_SYM: &str = "_Tp";

/// Key for when the referenced symbol is a `enum` type definition.
pub const ENUM_TYPE_SYM: &str = "_E";

/// Gets the mangled name of the node with the given ID, following the L1
/// mangling scheme.
pub fn mangled_name_of(tcx: &TyCheckCtx, id: NodeId) -> Result<String> {
    match tcx.hir_expect_node(id) {
        lume_hir::Node::Function(func) => Ok(mangled_name_of_function(tcx, func)),
        lume_hir::Node::Type(lume_hir::TypeDefinition::Struct(struct_def)) => {
            Ok(mangled_name_of_struct_definition(tcx, struct_def))
        }
        lume_hir::Node::Type(lume_hir::TypeDefinition::Trait(trait_def)) => {
            Ok(mangled_name_of_trait_definition(tcx, trait_def))
        }
        lume_hir::Node::Type(lume_hir::TypeDefinition::Enum(enum_def)) => {
            Ok(mangled_name_of_enum_definition(tcx, enum_def))
        }
        lume_hir::Node::Type(lume_hir::TypeDefinition::TypeParameter(type_param)) => {
            Ok(mangled_name_of_type_parameter(tcx, type_param))
        }
        lume_hir::Node::Method(method) => Ok(mangled_name_of_method(tcx, method)),
        lume_hir::Node::TraitMethodDef(method_def) => Ok(mangled_name_of_trait_method_def(tcx, method_def)),
        lume_hir::Node::TraitMethodImpl(method_impl) => mangled_name_of_trait_method_impl(tcx, method_impl),
        lume_hir::Node::Impl(_) => Err(SimpleDiagnostic::new("cannot get mangled name of implementation").into()),
        lume_hir::Node::TraitImpl(_) => {
            Err(SimpleDiagnostic::new("cannot get mangled name of trait implementation").into())
        }
        lume_hir::Node::Field(_) => Err(SimpleDiagnostic::new("cannot get mangled name of field").into()),
        lume_hir::Node::Pattern(_) => Err(SimpleDiagnostic::new("cannot get mangled name of pattern").into()),
        lume_hir::Node::Statement(_) => Err(SimpleDiagnostic::new("cannot get mangled name of statement").into()),
        lume_hir::Node::Expression(_) => Err(SimpleDiagnostic::new("cannot get mangled name of expression").into()),
    }
}

fn mangled_name_of_function(tcx: &TyCheckCtx, func: &lume_hir::FunctionDefinition) -> String {
    if func.block.is_none() {
        return format!("{:+}", func.name);
    }

    // If the function is the package entrypoint, give it a specific name so we can
    // reference it in the runtime.
    if tcx.is_entrypoint(func.id) {
        return String::from("__lume_entry");
    }

    let package_segment = mangled_package_segment(tcx, func.id.package);
    let path_segment = mangled_path_segment(&func.name);

    format!("{MANGLED_PREFIX}{FUNCTION_SYM}{package_segment}{path_segment}")
}

fn mangled_name_of_struct_definition(tcx: &TyCheckCtx, struct_def: &lume_hir::StructDefinition) -> String {
    let package_segment = mangled_package_segment(tcx, struct_def.id.package);
    let path_segment = mangled_path_segment(&struct_def.name);

    format!("{MANGLED_PREFIX}{STRUCT_TYPE_SYM}{package_segment}{path_segment}")
}

fn mangled_name_of_trait_definition(tcx: &TyCheckCtx, trait_def: &lume_hir::TraitDefinition) -> String {
    let package_segment = mangled_package_segment(tcx, trait_def.id.package);
    let path_segment = mangled_path_segment(&trait_def.name);

    format!("{MANGLED_PREFIX}{TRAIT_TYPE_SYM}{package_segment}{path_segment}")
}

fn mangled_name_of_enum_definition(tcx: &TyCheckCtx, enum_def: &lume_hir::EnumDefinition) -> String {
    let package_segment = mangled_package_segment(tcx, enum_def.id.package);
    let path_segment = mangled_path_segment(&enum_def.name);

    format!("{MANGLED_PREFIX}{ENUM_TYPE_SYM}{package_segment}{path_segment}")
}

fn mangled_name_of_type_parameter(tcx: &TyCheckCtx, type_param: &lume_hir::TypeParameter) -> String {
    let package_segment = mangled_package_segment(tcx, type_param.id.package);

    let full_name = tcx.hir_path_of_node(type_param.id);
    let path_segment = mangled_path_segment(&full_name);

    let mut constraints = String::new();
    for constraint in &type_param.constraints {
        constraints.push_str(CONSTRAINT_INDICATOR);
        constraints.push_str(&mangled_path_name(&constraint.name));
    }

    format!(
        "{MANGLED_PREFIX}{PARAM_TYPE_SYM}{package_segment}{path_segment}{constraints}_{}",
        type_param.id
    )
}

fn mangled_name_of_method(tcx: &TyCheckCtx, method: &lume_hir::MethodDefinition) -> String {
    if method.block.is_none() {
        return format!("{:+}", tcx.hir_path_of_node(method.id));
    }

    let parent = tcx.hir_parent_of(method.id).unwrap();
    let lume_hir::Node::Impl(implementation) = tcx.hir_expect_node(parent) else {
        unreachable!();
    };

    let package_segment = mangled_package_segment(tcx, method.id.package);
    let impl_segment = mangled_impl_segment(tcx, &implementation.target);
    let path_segment = mangled_name_segment(method.name.as_str());

    format!("{MANGLED_PREFIX}{METHOD_SYM}{package_segment}{impl_segment}{path_segment}")
}

fn mangled_name_of_trait_method_def(tcx: &TyCheckCtx, method_def: &lume_hir::TraitMethodDefinition) -> String {
    let method_name = tcx.hir_path_of_node(method_def.id);

    let package_segment = mangled_package_segment(tcx, method_def.id.package);
    let path_segment = mangled_path_segment(&method_name);

    format!("{MANGLED_PREFIX}{TRAIT_METHOD_DEF_SYM}{package_segment}{path_segment}")
}

fn mangled_name_of_trait_method_impl(
    tcx: &TyCheckCtx,
    method_impl: &lume_hir::TraitMethodImplementation,
) -> Result<String> {
    if method_impl.block.is_none() {
        return Ok(format!("{:+}", tcx.hir_path_of_node(method_impl.id)));
    }

    let parent = tcx.hir_parent_of(method_impl.id).unwrap();
    let lume_hir::Node::TraitImpl(trait_impl) = tcx.hir_expect_node(parent) else {
        unreachable!();
    };

    let trait_def = tcx.trait_definition_of_method_impl(method_impl)?;

    let package_segment = mangled_package_segment(tcx, method_impl.id.package);
    let path_segment = mangled_path_segment(&trait_def.name);
    let name_segment = mangled_name_segment(method_impl.name.as_str());
    let impl_segment = mangled_impl_segment(tcx, &trait_impl.target);

    Ok(format!(
        "{MANGLED_PREFIX}{TRAIT_METHOD_IMPL_SYM}{package_segment}{path_segment}{name_segment}{impl_segment}"
    ))
}

fn mangled_package_segment(tcx: &TyCheckCtx, id: PackageId) -> String {
    let package_name = &tcx.gcx().package_name(id).expect("failed to find package");
    let name_len = package_name.len();

    format!("{PACKAGE_INDICATOR}{name_len}{package_name}")
}

fn mangled_path_segment(name: &lume_hir::Path) -> String {
    format!("{PATH_INDICATOR}{}", mangled_path_name(name))
}

fn mangled_name_segment(name: &str) -> String {
    let len = name.len();

    format!("{NAME_INDICATOR}{len}{name}")
}

fn mangled_impl_segment(tcx: &TyCheckCtx, ty: &lume_hir::Type) -> String {
    let type_name = if let Some(type_param) = tcx.as_type_param(ty.id.as_node_id()) {
        mangled_type_parameter_segment(type_param)
    } else {
        mangled_path_name(&ty.name)
    };

    format!("{IMPL_INDICATOR}{type_name}")
}

fn mangled_type_parameter_segment(type_param: &lume_hir::TypeParameter) -> String {
    let type_param_name = type_param.name.as_str();
    let mut name = format!("{TYPE_INDICATOR}{}{type_param_name}", type_param_name.len());

    for constraint in &type_param.constraints {
        name.push_str(CONSTRAINT_INDICATOR);
        name.push_str(&mangled_path_name(&constraint.name));
    }

    name
}

fn mangled_path_name(name: &lume_hir::Path) -> String {
    let segments = name.segments();
    let mut name_str = Vec::with_capacity(segments.len());

    for segment in segments {
        match segment {
            lume_hir::PathSegment::Namespace { name }
            | lume_hir::PathSegment::Type { name, .. }
            | lume_hir::PathSegment::Callable { name, .. }
            | lume_hir::PathSegment::Variant { name, .. } => {
                let str = name.as_str();
                let len = str.len();

                name_str.push(format!("{len}{str}"));
            }
        }
    }

    name_str.join("")
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lume_errors::{DiagCtx, Result};
    use lume_hir::{Path, PathSegment};
    use lume_typech::TyCheckCtx;

    fn mangle_fixture(package_name: &'static str, source: &'static str) -> Result<TyCheckCtx> {
        let source_file = lume_span::SourceFile::new(lume_span::PackageId::empty(), "main.lm", source);
        let stub_package = build_stage::stub_package_with(|pkg: &mut lume_session::Package| {
            pkg.name = package_name.to_string();
            pkg.add_source(Arc::new(source_file));
        });

        let dcx = DiagCtx::new();
        let manifold_driver = build_stage::ManifoldDriver::new(stub_package, dcx.clone());

        manifold_driver.type_check()
    }

    #[test]
    fn test_func_name() -> Result<()> {
        let tcx = mangle_fixture("playground", "fn foo_bar() { }")?;
        let callable = tcx
            .callable_with_name(&Path::rooted(PathSegment::callable("foo_bar")))
            .unwrap();

        let mangled = super::mangled_name_of(&tcx, callable.id())?;
        assert_eq!(mangled.as_str(), "_L1_CfP10playgroundN7foo_bar");

        Ok(())
    }

    #[test]
    fn test_external_func_name() -> Result<()> {
        let tcx = mangle_fixture("playground", "fn external foo_bar()")?;
        let callable = tcx
            .callable_with_name(&Path::rooted(PathSegment::callable("foo_bar")))
            .unwrap();

        let mangled = super::mangled_name_of(&tcx, callable.id())?;
        assert_eq!(mangled.as_str(), "foo_bar");

        Ok(())
    }

    #[test]
    fn test_method_impl_name() -> Result<()> {
        let tcx = mangle_fixture(
            "playground",
            "
                struct Foo { }

                impl Foo {
                    pub fn bar(self) { }
                }
            ",
        )?;

        let callable = tcx
            .callable_with_name(&Path::from_parts(
                Some(vec![PathSegment::ty("Foo")]),
                PathSegment::callable("bar"),
            ))
            .unwrap();

        let mangled = super::mangled_name_of(&tcx, callable.id())?;
        assert_eq!(mangled.as_str(), "_L1_CiP10playgroundI3FooM3bar");

        Ok(())
    }

    #[test]
    fn test_external_method_name() -> Result<()> {
        let tcx = mangle_fixture(
            "playground",
            "
                struct Foo { }

                impl Foo {
                    pub fn external bar(self)
                }
            ",
        )?;

        let callable = tcx
            .callable_with_name(&Path::from_parts(
                Some(vec![PathSegment::ty("Foo")]),
                PathSegment::callable("bar"),
            ))
            .unwrap();

        let mangled = super::mangled_name_of(&tcx, callable.id())?;
        assert_eq!(mangled.as_str(), "Foo::bar");

        Ok(())
    }

    #[test]
    fn test_method_blanket_impl() -> Result<()> {
        let tcx = mangle_fixture(
            "playground",
            "
                impl<T> T {
                    pub fn bar(self) { }
                }
            ",
        )?;

        let callable = tcx
            .callable_with_name(&Path::from_parts(
                Some(vec![PathSegment::ty("T")]),
                PathSegment::callable("bar"),
            ))
            .unwrap();

        let mangled = super::mangled_name_of(&tcx, callable.id())?;
        assert_eq!(mangled.as_str(), "_L1_CiP10playgroundIY1TM3bar");

        Ok(())
    }

    #[test]
    fn test_method_blanket_constrained_impl() -> Result<()> {
        let tcx = mangle_fixture(
            "playground",
            "
                trait Foo { }

                impl<T: Foo> T {
                    pub fn bar(self) { }
                }
            ",
        )?;

        let callable = tcx
            .callable_with_name(&Path::from_parts(
                Some(vec![PathSegment::ty("T")]),
                PathSegment::callable("bar"),
            ))
            .unwrap();

        let mangled = super::mangled_name_of(&tcx, callable.id())?;
        assert_eq!(mangled.as_str(), "_L1_CiP10playgroundIY1Tc3FooM3bar");

        Ok(())
    }

    #[test]
    fn test_method_blanket_more_constrained_impl() -> Result<()> {
        let tcx = mangle_fixture(
            "playground",
            "
                trait Foo { }
                trait Bar { }

                impl<T: Foo + Bar> T {
                    pub fn baz(self) { }
                }
            ",
        )?;

        let callable = tcx
            .callable_with_name(&Path::from_parts(
                Some(vec![PathSegment::ty("T")]),
                PathSegment::callable("baz"),
            ))
            .unwrap();

        let mangled = super::mangled_name_of(&tcx, callable.id())?;
        assert_eq!(mangled.as_str(), "_L1_CiP10playgroundIY1Tc3Fooc3BarM3baz");

        Ok(())
    }

    #[test]
    fn test_trait_method_def() -> Result<()> {
        let tcx = mangle_fixture(
            "playground",
            "
                trait Foo {
                    fn bar(self) { }
                }
            ",
        )?;

        let callable = tcx
            .callable_with_name(&Path::from_parts(
                Some(vec![PathSegment::ty("Foo")]),
                PathSegment::callable("bar"),
            ))
            .unwrap();

        let mangled = super::mangled_name_of(&tcx, callable.id())?;
        assert_eq!(mangled.as_str(), "_L1_CtP10playgroundN3Foo3bar");

        Ok(())
    }

    #[test]
    fn test_trait_method_impl() -> Result<()> {
        let tcx = mangle_fixture(
            "playground",
            "
                trait Foo {
                    fn bar(self) { }
                }

                use Foo: Int32 {
                    fn bar(self) { }
                }
            ",
        )?;

        let callable = tcx
            .callable_with_name(&Path::from_parts(
                Some(vec![PathSegment::namespace("std"), PathSegment::ty("Int32")]),
                PathSegment::callable("bar"),
            ))
            .unwrap();

        let mangled = super::mangled_name_of(&tcx, callable.id())?;
        assert_eq!(mangled.as_str(), "_L1_CsP10playgroundN3FooM3barI3std5Int32");

        Ok(())
    }

    #[test]
    fn test_external_trait_method_impl() -> Result<()> {
        let tcx = mangle_fixture(
            "playground",
            "
                trait Foo {
                    fn bar(self) { }
                }

                use Foo: Int32 {
                    fn external bar(self)
                }
            ",
        )?;

        let callable = tcx
            .callable_with_name(&Path::from_parts(
                Some(vec![PathSegment::namespace("std"), PathSegment::ty("Int32")]),
                PathSegment::callable("bar"),
            ))
            .unwrap();

        let mangled = super::mangled_name_of(&tcx, callable.id())?;
        assert_eq!(mangled.as_str(), "std::Int32::Foo::bar");

        Ok(())
    }

    #[test]
    fn test_struct_def() -> Result<()> {
        let tcx = mangle_fixture("playground", "struct Foo { }")?;
        let ty = tcx.tdb().find_type(&Path::rooted(PathSegment::ty("Foo"))).unwrap();

        let mangled = super::mangled_name_of(&tcx, ty.id)?;
        assert_eq!(mangled.as_str(), "_L1_SP10playgroundN3Foo");

        Ok(())
    }

    #[test]
    fn test_trait_def() -> Result<()> {
        let tcx = mangle_fixture("playground", "trait Foo { }")?;
        let ty = tcx.tdb().find_type(&Path::rooted(PathSegment::ty("Foo"))).unwrap();

        let mangled = super::mangled_name_of(&tcx, ty.id)?;
        assert_eq!(mangled.as_str(), "_L1_TtP10playgroundN3Foo");

        Ok(())
    }

    #[test]
    fn test_enum_def() -> Result<()> {
        let tcx = mangle_fixture("playground", "enum Foo { }")?;
        let ty = tcx.tdb().find_type(&Path::rooted(PathSegment::ty("Foo"))).unwrap();

        let mangled = super::mangled_name_of(&tcx, ty.id)?;
        assert_eq!(mangled.as_str(), "_L1_EP10playgroundN3Foo");

        Ok(())
    }
}
