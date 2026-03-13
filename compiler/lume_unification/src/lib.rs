pub mod engine;

mod constraints;
mod diagnostics;
mod introduce;
mod subst;
mod verify;

use lume_errors::Result;
use lume_infer::TyInferCtx;
use lume_span::{Location, NodeId};
use lume_types::TypeRef;

use crate::engine::{Context, Engine, Type, TypeKind, TypeVar};

/// Performs unification on the given type inference context.
///
/// # Errors
///
/// If any errors are raised during unification, this method will either:
/// - return early with the error wrapped in a `Result::Err`,
/// - or raise the inside error inside the diagnostics context in `tcx`.
#[tracing::instrument(level = "INFO", skip_all, err)]
pub fn unify(tcx: &mut TyInferCtx) -> Result<()> {
    lume_architect::DatabaseContext::db(tcx).disable_caching();
    lume_architect::DatabaseContext::db(tcx).clear_all();

    verify::verify_type_names(tcx);

    let mut engine = Engine::new(tcx);
    engine.introduce_type_variables()?;
    engine.create_constraints()?;

    if let Err(errors) = engine.substitute_all() {
        for error in errors {
            handle_error(&engine, error);
        }
    }

    engine.ctx.dcx().ensure_untainted()?;
    engine.apply_substitutions()?;

    // We need to invalidate the global cache for method calls, since the
    // unification pass has altered some items in the HIR, making those
    // entries in the cache incorrect and/or invalid.
    //
    // Very few method calls would've been cached at this point in the compile
    // process, so we can safetly clear the entire thing, without having
    // to worry too much about the potential performance loss.
    lume_architect::DatabaseContext::db(tcx).enable_caching();
    lume_architect::DatabaseContext::db(tcx).clear_all();

    tcx.dcx().ensure_untainted()
}

/// Emits the given error to the diagnostics context.
fn handle_error(engine: &Engine<'_, TyInferCtx>, error: crate::engine::Error<TyInferCtx>) {
    match error {
        engine::Error::Mismatch { lhs, rhs } | engine::Error::RigidMismatch { lhs, rhs } => {
            engine.ctx.raise_mismatched_types(&lhs, &rhs);
        }
        engine::Error::InfiniteType { var, .. } => {
            let binding = engine.ctx.hir_tyvar_binding_of(var.0).unwrap();

            engine.ctx.dcx().emit(
                diagnostics::InfiniteType {
                    location: engine.ctx.span_of(var.0),
                    type_parameter_span: engine.ctx.span_of(binding.as_node_id()),
                    type_parameter_name: engine.ctx.name_of(binding.as_node_id()).unwrap(),
                }
                .into(),
            );
        }
        engine::Error::BoundUnsatisfied {
            ty,
            bound,
            type_parameter,
        } => {
            let type_parameter = engine.ctx.hir_expect_type_parameter(type_parameter);

            engine.ctx.dcx().emit(
                diagnostics::BoundUnsatisfied {
                    source: ty.location,
                    constraint_loc: bound.location,
                    param_name: type_parameter.name.to_string(),
                    type_name: engine.ctx.name_of_type(&ty).unwrap(),
                    constraint_name: engine.ctx.name_of_type(&bound).unwrap(),
                }
                .into(),
            );
        }
        engine::Error::Unsolved(type_variable) => {
            let binding = engine.ctx.hir_tyvar_binding_of(type_variable.0).unwrap();

            engine.ctx.dcx().emit(
                diagnostics::UnresolvedTypeVariable {
                    location: engine.ctx.span_of(type_variable.0),
                    type_parameter_name: engine.ctx.name_of(binding.as_node_id()).unwrap(),
                }
                .into(),
            );
        }
    }
}

impl Context for TyInferCtx {
    type ID = NodeId;
    type Ty = TypeRef;

    fn name_of(&self, id: Self::ID) -> Result<String> {
        Ok(self.hir_path_of_node(id).to_wide_string())
    }

    fn name_of_type(&self, ty: &Self::Ty) -> Result<String> {
        self.ty_stringifier(ty).include_namespace(true).stringify()
    }

    fn span_of(&self, id: Self::ID) -> Location {
        self.hir_span_of_node(id)
    }

    fn fresh_var(&mut self, owner: Self::ID, binding: Self::ID, location: Location) -> TypeVar<Self> {
        let id = NodeId::from_usize(self.hir().package, self.hir().nodes.len() + (usize::MAX / 2));
        let canonical = self
            .hir_canonical_type_of(binding.into(), owner)
            .map(|type_id| type_id.map_or(binding, |id| id.as_node_id()))
            .unwrap();

        let type_var = TypeVar(id);

        assert!(
            self.tdb_mut()
                .types
                .insert(id, lume_types::Type {
                    id,
                    kind: lume_types::TypeKind::TypeVariable,
                    name: lume_hir::Path::rooted(lume_hir::PathSegment::Type {
                        name: type_var.to_string().into(),
                        bound_types: Vec::new(),
                        location,
                    }),
                })
                .is_none()
        );

        assert!(
            self.hir_mut()
                .nodes
                .insert(
                    id,
                    lume_hir::Node::TypeVariable(lume_hir::TypeVariable {
                        id,
                        binding: binding.into(),
                        canonical: canonical.into(),
                        location
                    })
                )
                .is_none()
        );

        type_var
    }

    fn as_type(&self, type_var: TypeVar<Self>) -> Self::Ty {
        TypeRef::new(type_var.0, Location::empty())
    }

    fn kind_of_type(&self, ty: &Self::Ty) -> TypeKind<Self> {
        match self.tdb().type_(ty.instance_of).map(|t| t.kind) {
            Some(lume_types::TypeKind::TypeVariable) => TypeKind::Variable(TypeVar(ty.instance_of)),
            Some(lume_types::TypeKind::TypeParameter) => TypeKind::Parameter(ty.instance_of),
            _ => TypeKind::Concrete(ty.instance_of),
        }
    }

    fn as_type_variable(&self, ty: &Self::Ty) -> Option<TypeVar<Self>> {
        TyInferCtx::as_type_variable(self, ty).map(|type_variable| TypeVar(type_variable.id))
    }

    fn implements_subtype(&self, ty: &Self::Ty, subtype: &Self::Ty) -> bool {
        debug_assert!(self.is_trait(subtype) == Ok(true), "expected subtype to be trait");

        self.trait_impl_by(subtype, ty) == Ok(true)
    }
}

impl Type<TyInferCtx> for TypeRef {
    fn id(&self) -> <lume_infer::TyInferCtx as Context>::ID {
        self.instance_of
    }

    fn bound_types(&self) -> &[Self] {
        &self.bound_types
    }

    fn bound_types_mut(&mut self) -> &mut [Self] {
        &mut self.bound_types
    }
}
