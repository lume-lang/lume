use lume_hir::SymbolName;
use lume_types::TypeRef;

use super::*;

#[test]
fn query_function_name_rooted() -> Result<()> {
    let tcx = type_infer("fn foo() { }")?;
    let funcs = tcx.lookup_functions_unchecked(&SymbolName::rooted("foo"));

    assert_eq!(funcs.len(), 1);

    let func = funcs.first().unwrap();

    assert_eq!(func.name, SymbolName::rooted("foo"));
    assert_eq!(func.parameters.len(), 0);
    assert_eq!(func.return_type, TypeRef::void());

    Ok(())
}
