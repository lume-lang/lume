use error_snippet::Result;
use lume_hir::{self};
use lume_types::Identifier;

use crate::{check::TypeCheckerPass, *};

mod define_method_bodies;
mod define_scope;
mod define_type_constraints;
mod define_type_params;
mod define_types;
mod infer_type_args;

/// Defines a list of types which are often used in other languages,
/// but have a different name in Lume.
const NEWCOMER_TYPE_NAMES: &[(&str, &str)] = &[
    ("int", "Int32"),
    ("i8", "Int8"),
    ("u8", "UInt8"),
    ("i16", "Int16"),
    ("u16", "UInt16"),
    ("i32", "Int32"),
    ("u32", "UInt32"),
    ("i64", "Int64"),
    ("u64", "UInt64"),
    ("isize", "IntPtr"),
    ("usize", "UIntPtr"),
    ("f32", "Float"),
    ("f64", "Double"),
    ("str", "String"),
    ("string", "String"),
    ("bool", "Boolean"),
    ("boolean", "Boolean"),
];

impl ThirBuildCtx<'_> {
    /// Defines all the different types, type parameters and type constraints within
    /// the HIR maps into the type database.
    ///
    /// The defined types are stored within the `ThirBuildCtx` struct, which can be
    /// accessed through the `self.tcx` field, the `self.tcx()` method or the `self.tcx_mut()` method.
    pub fn define_types(&mut self, hir: &mut lume_hir::map::Map) -> Result<()> {
        infer::define_types::DefineTypes::run_all(self, hir)?;
        infer::define_type_params::DefineTypeParameters::run_all(self, hir)?;
        infer::define_type_constraints::DefineTypeConstraints::run_all(self, hir)?;
        infer::define_method_bodies::DefineMethodBodies::run_all(self, hir)?;

        self.infer_calls(hir)?;

        infer::infer_type_args::InferTypeArgs::run_all(self, hir)?;
        infer::define_scope::ScopeVisitor::run(self, hir)?;

        self.infer_exprs(hir)?;

        Ok(())
    }

    /// Attempt to infer the types of all expressions in the current module.
    ///
    /// The resolved types are stored in the `resolved_exprs` field of the `ThirBuildCtx`, which can be
    /// accessed through the `self.tcx` field, the `self.tcx()` method or the `self.tcx_mut()` method.
    fn infer_exprs(&mut self, hir: &lume_hir::map::Map) -> Result<()> {
        for (id, expr) in hir.expressions() {
            let type_ref = self.type_of(hir, expr.id)?;

            self.resolved_exprs.insert(*id, type_ref);
        }

        Ok(())
    }

    /// Attempt to infer the referenced callable of all call expressions in the current module.
    ///
    /// The resolved references are stored in the `resolved_calls` field of the `ThirBuildCtx`, which can be
    /// accessed through the `self.tcx` field, the `self.tcx()` method or the `self.tcx_mut()` method.
    fn infer_calls(&mut self, hir: &mut lume_hir::map::Map) -> Result<()> {
        for (id, expr) in hir.expressions() {
            let location = expr.location.clone();

            let reference = match &expr.kind {
                lume_hir::ExpressionKind::InstanceCall(call) => {
                    let method = self.lookup_instance_method(hir, call, location)?;

                    CallReference::Method(method.id)
                }
                lume_hir::ExpressionKind::StaticCall(call) => self.lookup_static_method(call, location)?,
                _ => continue,
            };

            self.resolved_calls.insert(*id, reference);
        }

        // After all calls have been resolved, we'll update the amount type parameters
        // in the call expressions, so
        for (id, reference) in &mut self.resolved_calls {
            let location = hir.expression(*id).unwrap().location.clone();

            let type_params = match reference {
                CallReference::Method(id) => &id.get(self.state.tcx()).type_parameters,
                CallReference::Function(id) => &id.get(self.state.tcx()).type_parameters,
            };

            let type_args = match &mut hir.expressions_mut().get_mut(id).unwrap().kind {
                lume_hir::ExpressionKind::InstanceCall(call) => &mut call.type_arguments,
                lume_hir::ExpressionKind::StaticCall(call) => &mut call.type_arguments,
                kind => panic!("BUG: unexpected expression kind: {:?}", kind),
            };

            // If no type arguments are provided, we are expected to infer all the of them.
            //
            // However, if at least one is provided, but it doesn't match the number of type parameters,
            // we raise an error, so the user doesn't invoke the wrong function / method.
            if !type_args.is_empty() && type_args.len() != type_params.len() {
                return Err(crate::errors::TypeArgumentMismatch {
                    source: location.file.clone(),
                    range: location.index.clone(),
                    expected: type_params.len(),
                    found: type_args.len(),
                }
                .into());
            }

            // If the type arguments are meant to be inferred, match the number of type arguments
            // with the number of type parameters, using `Implicit` type arguments.
            if type_args.is_empty() {
                for _ in 0..type_params.len() {
                    type_args.push(lume_hir::TypeArgument::Implicit {
                        location: lume_span::Location::empty(),
                    });
                }
            }
        }

        Ok(())
    }

    /// Gets the HIR statement with the given ID and assert that it's a variable declaration statement.
    pub(crate) fn hir_expect_var_stmt<'a>(
        &'a self,
        hir: &'a lume_hir::map::Map,
        id: lume_hir::StatementId,
    ) -> &'a lume_hir::VariableDeclaration {
        let stmt = self.hir_stmt(hir, id);

        match &stmt.kind {
            lume_hir::StatementKind::Variable(decl) => decl,
            t => panic!("invalid variable reference type: {:?}", t),
        }
    }

    /// Returns the *type* of the expression with the given [`ExpressionId`].
    ///
    /// ### Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls this
    /// method makes, in the case of some expressions, such as assignments.
    pub(crate) fn type_of(&self, hir: &lume_hir::map::Map, def: ExpressionId) -> Result<TypeRef> {
        // If the expression has been memorized, return it instead.
        if let Some(existing) = self.resolved_exprs.get(&def) {
            return Ok(existing.clone());
        }

        let expr = self.hir_expr(hir, def);

        match &expr.kind {
            lume_hir::ExpressionKind::Assignment(e) => self.type_of(hir, e.value.id),
            lume_hir::ExpressionKind::StaticCall(call) => {
                match self.lookup_static_method(call, expr.location.clone())? {
                    CallReference::Method(method_id) => {
                        let method = method_id.get(self.tcx());

                        Ok(method.return_type.clone())
                    }
                    CallReference::Function(func_id) => {
                        let func = func_id.get(self.tcx());

                        Ok(func.return_type.clone())
                    }
                }
            }
            lume_hir::ExpressionKind::InstanceCall(call) => {
                let method = self.lookup_instance_method(hir, call, expr.location.clone())?;

                Ok(method.return_type.clone())
            }
            lume_hir::ExpressionKind::Literal(e) => self.type_of_lit(e),
            lume_hir::ExpressionKind::Member(expr) => {
                let callee_type = self.type_of(hir, expr.callee.id)?;
                let property_id = match callee_type.property(self.tcx(), &expr.name) {
                    Some(property_id) => property_id,
                    None => {
                        return Err(errors::MissingProperty {
                            source: expr.location.file.clone(),
                            range: expr.location.index.clone(),
                            type_name: callee_type.name(self.tcx()),
                            property_name: Identifier {
                                name: expr.name.clone(),
                                location: expr.location.clone(),
                            },
                        }
                        .into());
                    }
                };

                Ok(property_id.get(self.tcx()).property_type.clone())
            }
            lume_hir::ExpressionKind::Variable(var) => {
                let decl = self.hir_expect_var_stmt(hir, var.reference);

                Ok(self.type_of(hir, decl.value.id)?)
            }
        }
    }

    /// Attempts to get the type of a literal expression.
    fn type_of_lit(&self, lit: &lume_hir::Literal) -> Result<TypeRef> {
        let type_id = match &lit.kind {
            lume_hir::LiteralKind::Int(k) => match &k.kind {
                lume_hir::IntKind::I8 => TypeId::find_or_err(self.tcx(), &SymbolName::i8()),
                lume_hir::IntKind::U8 => TypeId::find_or_err(self.tcx(), &SymbolName::u8()),
                lume_hir::IntKind::I16 => TypeId::find_or_err(self.tcx(), &SymbolName::i16()),
                lume_hir::IntKind::U16 => TypeId::find_or_err(self.tcx(), &SymbolName::u16()),
                lume_hir::IntKind::I32 => TypeId::find_or_err(self.tcx(), &SymbolName::i32()),
                lume_hir::IntKind::U32 => TypeId::find_or_err(self.tcx(), &SymbolName::u32()),
                lume_hir::IntKind::I64 => TypeId::find_or_err(self.tcx(), &SymbolName::i64()),
                lume_hir::IntKind::U64 => TypeId::find_or_err(self.tcx(), &SymbolName::u64()),
                lume_hir::IntKind::IPtr => TypeId::find_or_err(self.tcx(), &SymbolName::iptr()),
                lume_hir::IntKind::UPtr => TypeId::find_or_err(self.tcx(), &SymbolName::uptr()),
            },
            lume_hir::LiteralKind::Float(k) => match &k.kind {
                lume_hir::FloatKind::F32 => TypeId::find_or_err(self.tcx(), &SymbolName::float()),
                lume_hir::FloatKind::F64 => TypeId::find_or_err(self.tcx(), &SymbolName::double()),
            },
            lume_hir::LiteralKind::String(_) => TypeId::find_or_err(self.tcx(), &SymbolName::string()),
            lume_hir::LiteralKind::Boolean(_) => TypeId::find_or_err(self.tcx(), &SymbolName::boolean()),
        };

        Ok(TypeRef::new(type_id))
    }

    /// Lowers the given HIR type into a type reference.
    pub(crate) fn mk_type_ref(&self, ty: &lume_hir::Type) -> Result<TypeRef> {
        self.mk_type_ref_generic(ty, &[])
    }

    /// Lowers the given HIR type into a type reference, which also looks
    /// up the given type parameters.
    pub(crate) fn mk_type_ref_generic(&self, ty: &lume_hir::Type, type_params: &[TypeParameter]) -> Result<TypeRef> {
        let found_type = match self.find_type_ref_ctx(&ty.name, type_params) {
            Some(id) => id,
            None => return Err(self.missing_type_err(ty)),
        };

        let mut type_ref = TypeRef::new(found_type);

        for type_param in &ty.type_params {
            let type_param_ref = self.mk_type_ref_generic(type_param, type_params)?;
            type_ref.push_type_argument(type_param_ref);
        }

        Ok(type_ref)
    }

    fn find_type_ref_ctx(&self, name: &SymbolName, type_params: &[TypeParameter]) -> Option<TypeId> {
        // First, attempt to find the type name within the given type parameters.
        for type_param in type_params {
            if type_param.name == name.name {
                return Some(type_param.type_id.unwrap());
            }
        }

        // Afterwards, attempt to find the type name within the type context.
        TypeId::find(self.tcx(), name)
    }

    /// Attempts to evaluate the type of the given type argument within a call expression.
    ///
    /// A type can container another type, either directly through a generic parameter or a
    /// nested type, which contains the type. This method does *not* check whether the type
    /// is used as a property type, method parameter, return type or any other member within classes,
    /// traits or otherwise.
    ///
    /// ### Panics
    ///
    /// This method will panic if any of the given types, or any nested types, are invalid
    /// or unregistered within the HIR or the type context.
    pub(crate) fn type_arg_evaluate<'a>(
        &'a self,
        parent: &lume_types::TypeRef,
        child: &lume_types::TypeRef,
        arg: &'a lume_types::TypeRef,
    ) -> Option<&'a lume_types::TypeRef> {
        // Before anything more taxing, just check if the types are equal.
        if parent == child {
            return Some(arg);
        }

        match &child.get(self.tcx()).kind {
            lume_types::TypeKind::TypeParameter(_) | lume_types::TypeKind::Void => None,
            kind => todo!("implement type_contains for {:?}", kind),
        }
    }

    /// Returns an error indicating that the given type was not found.
    fn missing_type_err(&self, ty: &lume_hir::Type) -> error_snippet::Error {
        for (newcomer_name, lume_name) in NEWCOMER_TYPE_NAMES {
            let ty_name = &ty.name.name.name;

            if newcomer_name == ty_name {
                return check::errors::UnavailableScalarType {
                    source: ty.location.file.clone(),
                    range: ty.location.index.clone(),
                    found: ty.name.name.to_string(),
                    suggestion: lume_name,
                }
                .into();
            }
        }

        errors::MissingType {
            source: ty.location.file.clone(),
            range: ty.location.index.clone(),
            name: ty.name.clone(),
        }
        .into()
    }
}
