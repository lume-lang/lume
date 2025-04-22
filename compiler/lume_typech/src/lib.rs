use indexmap::IndexMap;
use lume_diag::Result;
use lume_hir::{ExpressionId, StatementId, TypeParameter, map::SourceMap};
use lume_types::{SymbolName, TypeDatabaseContext, TypeId, TypeRef};

mod check;
mod errors;
mod infer;

#[derive(serde::Serialize, Debug)]
pub struct ThirBuildCtx {
    /// Defines the type database context.
    tcx: TypeDatabaseContext,

    /// Defines the sources currently being processed.
    sources: SourceMap,

    /// Defines a mapping between expressions and their resolved types.
    pub resolved_exprs: IndexMap<ExpressionId, TypeRef>,

    /// Defines a mapping between statements and their resolved types.
    pub resolved_stmts: IndexMap<StatementId, TypeRef>,
}

impl ThirBuildCtx {
    /// Creates a new empty THIR build context.
    pub fn new(sources: SourceMap) -> Self {
        ThirBuildCtx {
            tcx: TypeDatabaseContext::new(),
            sources,
            resolved_exprs: IndexMap::new(),
            resolved_stmts: IndexMap::new(),
        }
    }

    /// Retrieves the type context from the build context.
    pub fn tcx(&self) -> &TypeDatabaseContext {
        &self.tcx
    }

    /// Retrieves the type context from the build context.
    pub fn tcx_mut(&mut self) -> &mut TypeDatabaseContext {
        &mut self.tcx
    }

    pub fn type_of_expr(&self, id: ExpressionId) -> &TypeRef {
        self.resolved_exprs.get(&id).unwrap()
    }

    pub fn type_of_stmt(&self, id: StatementId) -> &TypeRef {
        self.resolved_stmts.get(&id).unwrap()
    }

    fn check_type_compatibility(&self, _from: &TypeRef, _to: &TypeRef) -> Result<()> {
        Ok(())
    }

    /// Attempts to infer the types of all expressions within the HIR maps.
    pub fn infer(&mut self, hir: &mut lume_hir::map::Map) -> Result<()> {
        self.define_types(hir)?;

        self.infer_exprs(hir)?;

        Ok(())
    }

    /// Lowers the given HIR type into a type reference.
    pub fn mk_type_ref(&self, ty: &lume_hir::Type) -> Result<TypeRef> {
        self.mk_type_ref_generic(ty, &[])
    }

    /// Lowers the given HIR type into a type reference, which also looks
    /// up the given type parameters.
    pub fn mk_type_ref_generic(&self, ty: &lume_hir::Type, type_params: &[TypeParameter]) -> Result<TypeRef> {
        match ty {
            lume_hir::Type::Scalar(t) => {
                let found_type = match self.find_type_ref_ctx(&t.name, type_params) {
                    Some(id) => id,
                    None => {
                        return Err(errors::MissingType {
                            source: self.sources.mapping.get(&t.location.file).unwrap().clone(),
                            range: t.location.start()..t.location.end(),
                            name: t.name.clone(),
                        }
                        .into());
                    }
                };

                let mut type_ref = TypeRef::new(found_type);

                for type_param in &t.type_params {
                    let type_param_ref = self.mk_type_ref(type_param)?;
                    type_ref.push_type_argument(type_param_ref);
                }

                Ok(type_ref)
            }
            _ => todo!(),
        }
    }

    fn find_type_ref_ctx(&self, name: &SymbolName, type_params: &[TypeParameter]) -> Option<TypeId> {
        // First, attempt to find the type name within the given type parameters.
        for type_param in type_params {
            if type_param.name == name.name {
                return Some(type_param.type_id.unwrap());
            }
        }

        // Afterwards, attempt to find the type name within the type context.
        TypeId::find(&self.tcx, name)
    }

    /// Gets the HIR expression with the given ID within the source file.
    pub(crate) fn hir_stmt<'a>(
        &'a self,
        hir: &'a lume_hir::map::Map,
        id: lume_hir::StatementId,
    ) -> &'a lume_hir::Statement {
        match hir.statements().get(&id) {
            Some(expr) => expr,
            None => panic!("no statement with given ID found: {:?}", id),
        }
    }

    /// Gets the HIR expression with the given ID within the source file.
    pub(crate) fn hir_expr<'a>(&self, hir: &'a lume_hir::map::Map, id: ExpressionId) -> &'a lume_hir::Expression {
        match hir.expressions().get(&id) {
            Some(expr) => expr,
            None => panic!("no expression with given ID found: {:?}", id),
        }
    }

    /// Gets the HIR statement with the given ID and assert that it's a variable declaration statement.
    pub(crate) fn hir_expect_var_stmt<'a>(
        &'a self,
        hir: &'a lume_hir::map::Map,
        id: lume_hir::StatementId,
    ) -> &'a lume_hir::VariableDeclaration {
        let stmt = self.hir_stmt(hir, id);

        match &stmt.kind {
            lume_hir::StatementKind::Variable(decl) => decl,
            t => panic!("invalid variable reference type: {:?}", t),
        }
    }

    /// Attempt to infer the types of all expressions in the current module.
    ///
    /// The resolved types are stored in the `resolved_exprs` field of the `TypeDatabaseContext`.
    pub fn infer_exprs(&mut self, hir: &lume_hir::map::Map) -> Result<()> {
        for (id, expr) in hir.expressions() {
            let type_ref = self.type_of(hir, expr.id)?;

            self.resolved_exprs.insert(*id, type_ref);
        }

        Ok(())
    }

    /// Returns the *type* of the expression with the given [`ExpressionId`].
    ///
    /// ### Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls this
    /// method makes, in the case of some expressions, such as assignments.
    pub(crate) fn type_of(&self, hir: &lume_hir::map::Map, def: ExpressionId) -> Result<TypeRef> {
        let expr = self.hir_expr(hir, def);

        match &expr.kind {
            lume_hir::ExpressionKind::Assignment(e) => self.type_of(hir, e.value.id),
            lume_hir::ExpressionKind::Literal(e) => self.type_of_lit(&*e),
            lume_hir::ExpressionKind::Variable(var) => {
                let decl = self.hir_expect_var_stmt(hir, var.reference);

                Ok(self.type_of(hir, decl.value.id)?)
            }
            k => todo!("unhandled node type in type_of(): {:?}", k),
        }
    }

    fn type_of_lit(&self, lit: &lume_hir::Literal) -> Result<TypeRef> {
        let type_id = match &lit.kind {
            lume_hir::LiteralKind::Int(k) => match &k.kind {
                lume_hir::IntKind::I8 => TypeId::find_or_err(&self.tcx, &SymbolName::i8()),
                lume_hir::IntKind::U8 => TypeId::find_or_err(&self.tcx, &SymbolName::u8()),
                lume_hir::IntKind::I16 => TypeId::find_or_err(&self.tcx, &SymbolName::i16()),
                lume_hir::IntKind::U16 => TypeId::find_or_err(&self.tcx, &SymbolName::u16()),
                lume_hir::IntKind::I32 => TypeId::find_or_err(&self.tcx, &SymbolName::i32()),
                lume_hir::IntKind::U32 => TypeId::find_or_err(&self.tcx, &SymbolName::u32()),
                lume_hir::IntKind::I64 => TypeId::find_or_err(&self.tcx, &SymbolName::i64()),
                lume_hir::IntKind::U64 => TypeId::find_or_err(&self.tcx, &SymbolName::u64()),
                lume_hir::IntKind::IPtr => TypeId::find_or_err(&self.tcx, &SymbolName::iptr()),
                lume_hir::IntKind::UPtr => TypeId::find_or_err(&self.tcx, &SymbolName::uptr()),
            },
            lume_hir::LiteralKind::Float(k) => match &k.kind {
                lume_hir::FloatKind::F32 => TypeId::find_or_err(&self.tcx, &SymbolName::float()),
                lume_hir::FloatKind::F64 => TypeId::find_or_err(&self.tcx, &SymbolName::double()),
            },
            lume_hir::LiteralKind::String(_) => TypeId::find_or_err(&self.tcx, &SymbolName::string()),
            lume_hir::LiteralKind::Boolean(_) => TypeId::find_or_err(&self.tcx, &SymbolName::boolean()),
        };

        Ok(TypeRef::new(type_id))
    }
}
