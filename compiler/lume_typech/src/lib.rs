#![feature(map_try_insert)]

use std::collections::BTreeMap;

use crate::query::CallReference;
use error_snippet::Result;
use lume_errors::DiagCtxHandle;
use lume_hir::{SymbolName, TypeParameter};
use lume_span::{DefId, ExpressionId};
use lume_types::{NamedTypeRef, TypeDatabaseContext, TypeRef};

mod check;
mod errors;
mod infer;
pub(crate) mod query;
#[cfg(test)]
mod tests;

pub struct ThirBuildCtx {
    /// Defines the type context from the build context.
    tcx: TypeDatabaseContext,

    /// Defines the HIR map which contains the input expressions.
    hir: lume_hir::map::Map,

    /// Defines the diagnostics handler.
    dcx: DiagCtxHandle,

    /// Defines a mapping any single node and their parent node.
    pub ancestry: BTreeMap<DefId, DefId>,
}

#[allow(dead_code)]
impl ThirBuildCtx {
    /// Creates a new empty THIR build context.
    pub fn new(hir: lume_hir::map::Map, dcx: DiagCtxHandle) -> ThirBuildCtx {
        ThirBuildCtx {
            tcx: TypeDatabaseContext::default(),
            hir,
            dcx,
            ancestry: BTreeMap::new(),
        }
    }

    /// Retrieves the High-Level Intermediate Representation (HIR) map from the build context.
    pub fn hir(&self) -> &lume_hir::map::Map {
        &self.hir
    }

    /// Retrieves the High-Level Intermediate Representation (HIR) map from the build context.
    pub fn hir_mut(&mut self) -> &mut lume_hir::map::Map {
        &mut self.hir
    }

    /// Retrieves the type context from the build context.
    pub fn tcx(&self) -> &TypeDatabaseContext {
        &self.tcx
    }

    /// Retrieves the type context from the build context.
    pub fn tcx_mut(&mut self) -> &mut TypeDatabaseContext {
        &mut self.tcx
    }

    /// Retrieves the diagnostics handler from the build context.
    pub fn dcx(&mut self) -> &mut DiagCtxHandle {
        &mut self.dcx
    }

    /// Creates a new [`NamedTypeRef`] from the given [`TypeRef`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if any types referenced by the given [`TypeRef`], or any child
    /// instances, are missing from the type context.
    pub fn new_named_type(&self, type_ref: &TypeRef) -> Result<NamedTypeRef> {
        let name = self.type_ref_name(type_ref)?.as_str().to_string();
        let type_arguments = type_ref
            .type_arguments
            .iter()
            .map(|arg| self.new_named_type(arg))
            .collect::<Result<Vec<_>>>()?;

        Ok(NamedTypeRef { name, type_arguments })
    }
}
