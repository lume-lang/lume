use std::{ops::Range, sync::Arc};

use lume_diag_macros::Diagnostic;
use lume_span::SourceFile;
use lume_types::{FunctionId, MethodId, SymbolName, TypeId};

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
