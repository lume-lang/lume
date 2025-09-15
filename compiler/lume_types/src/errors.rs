use error_snippet_derive::Diagnostic;
use lume_hir::Path;
use lume_span::{DefId, Location};

use crate::{Item, TypeKindRef};

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find definition {id:?} in context")]
pub struct DefNotFound {
    pub id: DefId,
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
