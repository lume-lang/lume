use lume_diag::Result;
use lume_hir::{self, ScalarType};
use lume_types::Identifier;

use crate::{check::TypeCheckerPass, *};

mod define_method_bodies;
mod define_scope;
mod define_type_constraints;
mod define_type_params;
mod define_types;

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
            lume_hir::ExpressionKind::New(new) => {
                let type_ref = self.mk_type_ref(&new.name)?;
                let type_def = type_ref.get(self.tcx());

                match &type_def.kind {
                    lume_types::TypeKind::Class(_) => Ok(type_ref),
                    kind => Err(crate::errors::AbstractTypeInstantiate {
                        source: expr.location.file.clone(),
                        range: expr.location.index.clone(),
                        name: type_def.name.clone(),
                        kind: match kind {
                            lume_types::TypeKind::Trait(_) => "trait",
                            lume_types::TypeKind::Alias(_) => "alias",
                            lume_types::TypeKind::Enum(_) => "enum",
                            lume_types::TypeKind::TypeParameter(_) => "type parameter",
                            lume_types::TypeKind::Void => "void",
                            lume_types::TypeKind::Class(_) => unreachable!(),
                        },
                    }
                    .into()),
                }
            }
            lume_hir::ExpressionKind::FunctionCall(call) => {
                let definition = match lume_types::Function::find(self.tcx(), &call.name) {
                    Some(def) => def,
                    None => {
                        return Err(crate::errors::MissingFunction {
                            source: expr.location.file.clone(),
                            range: expr.location.index.clone(),
                            name: call.name.clone(),
                        }
                        .into());
                    }
                };

                Ok(definition.return_type.clone())
            }
            lume_hir::ExpressionKind::MethodCall(call) => {
                let callee_type = self.type_of(hir, call.callee.id)?;
                let method =
                    match self.method_lookup(hir, &callee_type, &call.name, &call.arguments, &call.type_parameters)? {
                        method::MethodLookupResult::Success(method) => method,
                        method::MethodLookupResult::Failure(err) => {
                            return Err(err.compound_err(expr.location.clone()));
                        }
                    };

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
        match ty {
            lume_hir::Type::Scalar(t) => {
                let found_type = match self.find_type_ref_ctx(&t.name, type_params) {
                    Some(id) => id,
                    None => return Err(self.missing_type_err(t)),
                };

                let mut type_ref = TypeRef::new(found_type);

                for type_param in &t.type_params {
                    let type_param_ref = self.mk_type_ref(type_param)?;
                    type_ref.push_type_argument(type_param_ref);
                }

                Ok(type_ref)
            }
            _ => todo!(),
        }
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

    /// Returns an error indicating that the given type was not found.
    fn missing_type_err(&self, ty: &ScalarType) -> lume_diag::Error {
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
