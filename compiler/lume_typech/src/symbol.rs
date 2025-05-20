use std::{ops::Range, sync::Arc};

use error_snippet::Result;
use error_snippet_derive::Diagnostic;
use lume_hir::{FunctionId, MethodId, SymbolName};
use lume_span::SourceFile;
use lume_types::{Function, Method, Type};

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

/// Represents the kind of a symbol in the type inference system.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum SymbolKind<'a> {
    /// Represents a symbol, which points to some type.
    Type(&'a Type),

    /// Represents a symbol, which points to some method.
    Method(&'a Method),

    /// Represents a symbol, which points to a function definition.
    Function(&'a Function),
}

impl ThirBuildCtx {
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
        let Some(symbol) = self.lookup_symbol(&expr.name) else {
            return Err(crate::errors::MissingSymbol {
                source: loc.file,
                range: loc.index,
                name: expr.name.clone(),
            }
            .into());
        };

        match symbol {
            SymbolKind::Type(ty) => Err(crate::errors::AttemptedTypeInvocation {
                source: loc.file,
                range: loc.index,
                name: ty.name.clone(),
            }
            .into()),
            SymbolKind::Method(method) => Ok(CallReference::Method(method.id)),
            SymbolKind::Function(func) => Ok(CallReference::Function(func.id)),
        }
    }

    /// Attempts to look up a symbol by it's path within the current package.
    pub(crate) fn lookup_symbol(&self, name: &SymbolName) -> Option<SymbolKind> {
        if let Some(symbol) = self.tcx().find_type(name) {
            return Some(SymbolKind::Type(symbol));
        }

        if let Some(symbol) = self.tcx().find_method(name) {
            return Some(SymbolKind::Method(symbol));
        }

        if let Some(symbol) = self.tcx().find_function(name) {
            return Some(SymbolKind::Function(symbol));
        }

        None
    }
}
