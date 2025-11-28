use lume_architect::cached_query;
use lume_errors::Result;
use lume_span::NodeId;
use lume_types::{TypeKind, TypeRef};

use crate::TyInferCtx;

impl TyInferCtx {
    /// Gets a slice of the type parameters defined on the given node.
    ///
    /// # Errors
    ///
    /// If the given node is missing or cannot hold type parameters, [`Err`] is
    /// returned.
    pub fn type_params_of(&self, id: NodeId) -> Result<&[NodeId]> {
        let Some(node) = self.hir_node(id) else {
            return Ok(&[]);
        };

        match node {
            lume_hir::Node::Type(type_def) => match type_def {
                lume_hir::TypeDefinition::Struct(struct_def) => Ok(&struct_def.type_parameters),
                lume_hir::TypeDefinition::Trait(trait_def) => Ok(&trait_def.type_parameters),
                lume_hir::TypeDefinition::Enum(enum_def) => Ok(&enum_def.type_parameters),
                lume_hir::TypeDefinition::TypeParameter(_) => Ok(&[]),
            },
            lume_hir::Node::Function(func) => Ok(&func.type_parameters),
            lume_hir::Node::Impl(implementation) => Ok(&implementation.type_parameters),
            lume_hir::Node::TraitImpl(trait_impl) => Ok(&trait_impl.type_parameters),
            lume_hir::Node::TraitMethodDef(method_def) => Ok(&method_def.type_parameters),
            lume_hir::Node::TraitMethodImpl(method_impl) => Ok(&method_impl.type_parameters),
            lume_hir::Node::Method(method) => Ok(&method.type_parameters),
            _ => Err(crate::query::diagnostics::CannotHoldTypeParams { id }.into()),
        }
    }

    /// Return the [`lume_hir::TypeParameter`], which correspond to the
    /// given ID, if it refers to a type parameter. Otherwise, returns [`None`].
    pub fn as_type_param(&self, id: NodeId) -> Option<&lume_hir::TypeParameter> {
        if let lume_hir::Node::Type(lume_hir::TypeDefinition::TypeParameter(type_param)) = self.hir_node(id)? {
            Some(type_param.as_ref())
        } else {
            None
        }
    }

    /// Returns a list of [`lume_hir::TypeParameter`], which correspond to the
    /// IDs within the given slice.
    pub fn as_type_params(&self, type_param_ids: &[NodeId]) -> Result<Vec<&lume_hir::TypeParameter>> {
        let mut type_params = Vec::with_capacity(type_param_ids.len());

        for type_param_id in type_param_ids {
            let lume_hir::Node::Type(lume_hir::TypeDefinition::TypeParameter(type_param)) =
                self.hir_expect_node(*type_param_id)
            else {
                panic!("bug!: expected type parameter");
            };

            type_params.push(type_param.as_ref());
        }

        Ok(type_params)
    }

    /// Returns a list of [`lume_hir::TypeParameter`], which correspond to the
    /// IDs within the given slice, which have been converted into instances of
    /// [`lume_hir::Type`].
    pub fn type_params_as_types(&self, type_param_ids: &[NodeId]) -> Result<Vec<lume_hir::Type>> {
        Ok(self
            .as_type_params(type_param_ids)?
            .into_iter()
            .map(|type_param| lume_hir::Type {
                id: lume_hir::TypeId::from(type_param.id),
                name: lume_hir::Path::rooted(lume_hir::PathSegment::ty(type_param.name.clone())),
                location: type_param.location,
            })
            .collect::<Vec<_>>())
    }

    /// Determines whether the given [`TypeRef`] is a kind of
    /// [`TypeKindRef::Struct`].
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn is_struct(&self, ty: &TypeRef) -> Result<bool> {
        match self.tdb().expect_type(ty.instance_of).map(|ty| &ty.kind) {
            Ok(TypeKind::Struct) => Ok(true),
            _ => Ok(false),
        }
    }

    /// Determines whether the given [`TypeRef`] is a kind of
    /// [`TypeKindRef::Trait`].
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn is_trait(&self, ty: &TypeRef) -> Result<bool> {
        match self.tdb().expect_type(ty.instance_of).map(|ty| &ty.kind) {
            Ok(TypeKind::Trait) => Ok(true),
            _ => Ok(false),
        }
    }

    /// Determines whether the given [`TypeRef`] is a kind of
    /// [`TypeKindRef::TypeParameter`].
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn is_type_parameter(&self, ty: &TypeRef) -> Result<bool> {
        match self.tdb().type_(ty.instance_of).map(|t| t.kind) {
            Some(TypeKind::TypeParameter) => Ok(true),
            _ => Ok(false),
        }
    }

    /// Determines whether the given [`TypeRef`] is a kind of
    /// [`TypeKindRef::TypeParameter`].
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn as_type_parameter(&self, ty: &TypeRef) -> Result<Option<&lume_hir::TypeParameter>> {
        match self.hir_expect_type(ty.instance_of) {
            lume_hir::TypeDefinition::TypeParameter(type_param) => Ok(Some(type_param.as_ref())),
            _ => Ok(None),
        }
    }

    /// Determines whether the given [`TypeRef`] has any generic components.
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn is_type_generic(&self, ty: &TypeRef) -> Result<bool> {
        if self.is_type_parameter(ty)? {
            return Ok(true);
        }

        for type_arg in &ty.bound_types {
            if self.is_type_parameter(type_arg)? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    /// Determines whether the given [`TypeRef`] is the `Never` type.
    #[cached_query]
    #[libftrace::traced(level = Trace)]
    pub fn is_type_never(&self, ty: &TypeRef) -> bool {
        let never_type = self.lang_item_type("never").expect("expected `Never` lang item");

        never_type.instance_of == ty.instance_of
    }

    /// Gets the `lang_item` with the given name from the HIR map.
    #[cached_query]
    #[libftrace::traced(level = Trace)]
    pub fn lang_item(&self, name: &str) -> Option<NodeId> {
        self.hir
            .lang_items
            .iter()
            .find_map(|(key, id)| (key == name).then_some(*id))
    }

    /// Gets the `lang_item` with the given name from the HIR map, turned into a
    /// [`TypeRef`].
    #[cached_query]
    #[libftrace::traced(level = Trace)]
    pub fn lang_item_type(&self, name: &str) -> Option<TypeRef> {
        let id = self.lang_item(name)?;

        Some(TypeRef::new(id, self.hir_span_of_node(id)))
    }

    /// Gets the name of the `![lang_item]` attribute and corresponding method,
    /// matching the given intrinsic.
    #[libftrace::traced(level = Trace)]
    pub fn lang_item_of_intrinsic(&self, intrinsic: &lume_hir::IntrinsicKind) -> (&'static str, &'static str) {
        match intrinsic {
            lume_hir::IntrinsicKind::Add { .. } => ("add_trait", "add"),
            lume_hir::IntrinsicKind::Sub { .. } => ("sub_trait", "sub"),
            lume_hir::IntrinsicKind::Mul { .. } => ("mul_trait", "mul"),
            lume_hir::IntrinsicKind::Div { .. } => ("div_trait", "div"),
            lume_hir::IntrinsicKind::And { .. } => ("and_trait", "and"),
            lume_hir::IntrinsicKind::Or { .. } => ("or_trait", "or"),
            lume_hir::IntrinsicKind::Negate { .. } => ("negate_trait", "negate"),
            lume_hir::IntrinsicKind::BinaryAnd { .. } => ("band_trait", "band"),
            lume_hir::IntrinsicKind::BinaryOr { .. } => ("bor_trait", "bor"),
            lume_hir::IntrinsicKind::BinaryXor { .. } => ("bxor_trait", "bxor"),
            lume_hir::IntrinsicKind::Not { .. } => ("not_trait", "not"),
            lume_hir::IntrinsicKind::Equal { .. } => ("equal_trait", "eq"),
            lume_hir::IntrinsicKind::NotEqual { .. } => ("equal_trait", "ne"),
            lume_hir::IntrinsicKind::Less { .. } => ("cmp_trait", "lt"),
            lume_hir::IntrinsicKind::LessEqual { .. } => ("cmp_trait", "le"),
            lume_hir::IntrinsicKind::Greater { .. } => ("cmp_trait", "gt"),
            lume_hir::IntrinsicKind::GreaterEqual { .. } => ("cmp_trait", "gt"),
        }
    }

    /// Gets the human-readable name of the operation, which is performed by the
    /// given operation.
    #[libftrace::traced(level = Trace)]
    pub fn operation_name_of_intrinsic(&self, intrinsic: &lume_hir::IntrinsicKind) -> &'static str {
        match intrinsic {
            lume_hir::IntrinsicKind::Add { .. } => "addition",
            lume_hir::IntrinsicKind::Sub { .. } => "subtraction",
            lume_hir::IntrinsicKind::Mul { .. } => "multiplication",
            lume_hir::IntrinsicKind::Div { .. } => "division",
            lume_hir::IntrinsicKind::And { .. } => "logical AND",
            lume_hir::IntrinsicKind::Or { .. } => "logical OR",
            lume_hir::IntrinsicKind::Negate { .. } => "negation",
            lume_hir::IntrinsicKind::BinaryAnd { .. } => "binary AND",
            lume_hir::IntrinsicKind::BinaryOr { .. } => "binary OR",
            lume_hir::IntrinsicKind::BinaryXor { .. } => "binary XOR",
            lume_hir::IntrinsicKind::Not { .. } => "binary NOT",
            lume_hir::IntrinsicKind::Equal { .. } => "equality",
            lume_hir::IntrinsicKind::NotEqual { .. } => "inequality",
            lume_hir::IntrinsicKind::Less { .. }
            | lume_hir::IntrinsicKind::LessEqual { .. }
            | lume_hir::IntrinsicKind::Greater { .. }
            | lume_hir::IntrinsicKind::GreaterEqual { .. } => "comparison",
        }
    }

    /// Gets the name of the type which defines the given intrinsic.
    #[libftrace::traced(level = Trace)]
    pub fn type_name_of_intrinsic(&self, intrinsic: &lume_hir::IntrinsicKind) -> Option<&lume_hir::Path> {
        let (lang_item, _) = self.lang_item_of_intrinsic(intrinsic);
        let item_def = self.lang_item(lang_item)?;

        Some(&self.tdb().type_(item_def)?.name)
    }
}
