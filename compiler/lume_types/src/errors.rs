use error_snippet_derive::Diagnostic;
use lume_hir::{FunctionId, ImplId, MethodId, PropertyId, TypeId, UseId};

use crate::{Item, TypeKind, TypeKindRef};

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find function {id:?} in context")]
pub struct FunctionNotFound {
    pub id: FunctionId,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find property {id:?} in context")]
pub struct PropertyNotFound {
    pub id: PropertyId,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find method {id:?} in context")]
pub struct MethodNotFound {
    pub id: MethodId,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find implementation {id:?} in context")]
pub struct ImplNotFound {
    pub id: ImplId,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find trait implementation {id:?} in context")]
pub struct UseNotFound {
    pub id: UseId,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find item {id:?} in context")]
pub struct TypeNotFound {
    pub id: TypeId,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "attempted to get return type of non-invokable type: {item:?}")]
pub struct ReturnTypeOnNoninvokableItem {
    pub item: Item,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "attempted to get type parameters on non-generic item: {item:?}")]
pub struct TypeParametersOnNonGenericItem {
    pub item: Item,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "attempted to get type parameters on non-generic type: {ty:?}")]
pub struct TypeParametersOnNonGenericType {
    pub ty: TypeKindRef,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "expected type to be kind of {expected:?}, found {found:?}")]
pub struct UnexpectedTypeKind {
    pub expected: TypeKind,
    pub found: TypeKind,
}
