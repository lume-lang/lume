use error_snippet::Result;
use lume_hir::{FunctionId, MethodId, TypeId, TypeParameter};
use lume_types::TypeRef;

use crate::ThirBuildCtx;

pub(crate) mod errors;
pub(crate) mod expressions;
mod lookup;

#[allow(dead_code)]
#[derive(Debug, Copy, Clone)]
enum ScopeKind {
    Function(FunctionId),
    Ty(TypeId),
    Method(MethodId),
}

/// Represents some form of scope, which hold variables, type parameters, etc.
///
/// Each scope contains a reference to it's parent scope, so a graph can be
/// constructed from a single Scope instance.
#[allow(dead_code)]
#[derive(Debug)]
struct ItemScope<'a> {
    kind: ScopeKind,

    /// Defines the parent scope, if any.
    parent: Option<&'a ItemScope<'a>>,

    /// Defines the type parameters defined in the scope.
    type_parameters: Vec<TypeParameter>,
}

impl<'a> ItemScope<'a> {
    /// Creates a new [`ItemScope`], of kind [`ScopeKind::Function`] with
    /// the given reference.
    fn function(function: FunctionId) -> Self {
        Self {
            kind: ScopeKind::Function(function),
            parent: None,
            type_parameters: Vec::new(),
        }
    }

    /// Creates a new [`ItemScope`], of kind [`ScopeKind::Ty`] with
    /// the given reference.
    fn ty(ty: TypeId) -> Self {
        Self {
            kind: ScopeKind::Ty(ty),
            parent: None,
            type_parameters: Vec::new(),
        }
    }

    /// Creates a new [`ItemScope`], of kind [`ScopeKind::Method`] with
    /// the given reference and parent scope.
    fn method(parent: &'a ItemScope, method: MethodId) -> Self {
        Self {
            kind: ScopeKind::Method(method),
            parent: Some(parent),
            type_parameters: Vec::new(),
        }
    }

    /// Get a flattened array of all [`TypeParameter`]s defined within
    /// the current scope and all parent scopes.
    fn flat_type_params(&self) -> Vec<TypeParameter> {
        let mut current = self;
        let mut type_params = self.type_parameters.clone();

        while let Some(parent) = current.parent {
            type_params.extend(parent.type_parameters.clone());
            current = parent;
        }

        type_params
    }
}

/// Represents a single pass which will be executed when invoking
/// the type checker on a given [`ThirBuildCtx`]-instance.
pub trait TypeCheckerPass {
    fn run(tcx: &mut ThirBuildCtx) -> Result<()>
    where
        Self: Sized;
}

impl ThirBuildCtx {
    /// Performs type-checking on all the items in the context, after they've
    /// been inferred in a previous stage.
    ///
    /// # Errors
    ///
    /// Returns `Err` when either a language error occured, such as missing variables, missing methods,
    /// etc, or when expected items cannot be found within the context.
    pub fn typecheck(&mut self) -> Result<()> {
        expressions::Expressions::run(self)?;

        Ok(())
    }

    /// Checks whether the given type references are compatible.
    ///
    /// # Errors
    ///
    /// Returns `Err` when expected items cannot be found within the context.
    pub(crate) fn check_type_compatibility(&self, from: &TypeRef, to: &TypeRef) -> Result<bool> {
        // If the two given types are exactly the same, both underlying instance and type arguments,
        // we can be sure they're compatible.
        if from == to {
            return Ok(true);
        }

        // Special case for `void` types, since they are always identical, no matter
        // whether they have different underlying IDs.
        match (self.is_void(from)?, self.is_void(to)?) {
            // void => value OR value => void
            (false, true) | (true, false) => return Ok(false),

            // void == void
            (true, true) => return Ok(true),

            // value => value
            (false, false) => (),
        }

        // If `to` refers to a trait where `from` implements `to`, they can
        // be downcast correctly.
        if self.is_trait(to)? && self.trait_impl_by(to, from)? {
            return Ok(true);
        }

        println!("typech: {from:#?} {to:#?}");

        Ok(false)
    }
}
