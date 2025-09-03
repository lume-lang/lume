use error_snippet_derive::Diagnostic;
use lume_hir::{FieldId, FunctionId, ImplId, MethodId, Path, TypeId, UseId};
use lume_span::Location;

use crate::{Item, TypeKindRef};

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find function {id:?} in context")]
pub struct FunctionNotFound {
    pub id: FunctionId,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find field {id:?} in context")]
pub struct FieldNotFound {
    pub id: FieldId,
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
#[diagnostic(message = "could not find item")]
pub struct TypeNameNotFound {
    pub name: Path,

    #[label(source, "could not find {name:+} in context")]
    pub location: Location,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "expected path {path:+} to have a parent")]
pub struct PathWithoutParent {
    pub path: Path,
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
    pub expected: TypeKindRef,
    pub found: TypeKindRef,
}
