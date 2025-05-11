use std::{ops::Range, sync::Arc};

use error_snippet::Result;
use error_snippet_derive::Diagnostic;
use lume_span::SourceFile;
use lume_types::{FunctionId, FunctionSig, Method, MethodId, SymbolName, TypeDatabaseContext, TypeId};

use crate::ThirBuildCtx;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "ambiguous symbol", code = "LM4129")]
pub struct AmbiguousSymbol {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("reference {name} is ambiguous")]
    pub range: Range<usize>,

    pub name: SymbolName,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CallReference {
    /// The call refers to a function.
    Function(FunctionId),

    /// The call refers to a method.
    Method(MethodId),
}

impl CallReference {
    /// Gets the signature of the inner function / method reference.
    pub fn sig<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> FunctionSig<'a> {
        match self {
            CallReference::Function(id) => id.get(ctx).sig(),
            CallReference::Method(id) => id.get(ctx).sig(),
        }
    }
}

/// Represents the kind of a symbol in the type inference system.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SymbolKind {
    /// Represents a symbol, which points to some type.
    Type(TypeId),

    /// Represents a symbol, which points to some method.
    Method(MethodId),

    /// Represents a symbol, which points to a function definition.
    Function(FunctionId),
}

impl ThirBuildCtx<'_> {
    /// Attempts to look up an instance method, which matches the given call expression.
    pub(crate) fn lookup_instance_method(
        &self,
        hir: &lume_hir::map::Map,
        expr: &lume_hir::InstanceCall,
        loc: lume_span::Location,
    ) -> Result<&Method> {
        let callee_type = self.type_of(hir, expr.callee.id)?;

        match self.method_lookup(hir, &callee_type, &expr.name, &expr.arguments, &expr.type_arguments)? {
            crate::method::MethodLookupResult::Success(method) => Ok(method),
            crate::method::MethodLookupResult::Failure(err) => Err(err.compound_err(loc)),
        }
    }

    /// Attempts to look up a static method, which matches the given call expression.
    pub(crate) fn lookup_static_method(
        &self,
        expr: &lume_hir::StaticCall,
        loc: lume_span::Location,
    ) -> Result<CallReference> {
        let symbol = match self.lookup_symbol(&expr.name) {
            Some(symbol) => symbol,
            None => {
                return Err(crate::errors::MissingSymbol {
                    source: loc.file.clone(),
                    range: loc.index.clone(),
                    name: expr.name.clone(),
                }
                .into());
            }
        };

        match symbol {
            SymbolKind::Type(type_id) => {
                let ty = type_id.get(self.tcx());

                Err(crate::errors::AttemptedTypeInvocation {
                    source: loc.file.clone(),
                    range: loc.index.clone(),
                    name: ty.name.clone(),
                }
                .into())
            }
            SymbolKind::Method(method_id) => Ok(CallReference::Method(method_id)),
            SymbolKind::Function(func_id) => Ok(CallReference::Function(func_id)),
        }
    }

    /// Attempts to look up a symbol by it's path within the current package.
    pub(crate) fn lookup_symbol(&self, name: &SymbolName) -> Option<SymbolKind> {
        if let Some(symbol) = self.lookup_type_symbol(name) {
            return Some(SymbolKind::Type(symbol));
        }

        if let Some(symbol) = self.lookup_method_symbol(name) {
            return Some(SymbolKind::Method(symbol));
        }

        if let Some(symbol) = self.lookup_function_symbol(name) {
            return Some(SymbolKind::Function(symbol));
        }

        None
    }

    /// Attempts to look up a type symbol by it's path within the current package.
    fn lookup_type_symbol(&self, name: &SymbolName) -> Option<TypeId> {
        lume_types::TypeId::find(self.tcx(), name)
    }

    /// Attempts to look up a method symbol by it's path within the current package.
    fn lookup_method_symbol(&self, name: &SymbolName) -> Option<MethodId> {
        lume_types::MethodId::find(self.tcx(), name)
    }

    /// Attempts to look up a function symbol by it's path within the current package.
    fn lookup_function_symbol(&self, name: &SymbolName) -> Option<FunctionId> {
        lume_types::FunctionId::find(self.tcx(), name)
    }
}
