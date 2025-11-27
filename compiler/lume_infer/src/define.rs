use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::LazyLock;

use error_snippet::Result;
use lume_hir::{Node, NodeType, Path, PathSegment};
use lume_span::*;
use lume_types::{TypeKind, TypeRef};

use crate::TyInferCtx;
use crate::query::Callable;

static INTRINSIC_METHODS: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
    HashSet::from([
        "std::Boolean::eq",
        "std::Boolean::ne",
        "std::Boolean::and",
        "std::Boolean::or",
        "std::Boolean::not",
        "std::Int8::eq",
        "std::Int8::ne",
        "std::Int8::lt",
        "std::Int8::le",
        "std::Int8::gt",
        "std::Int8::ge",
        "std::Int8::add",
        "std::Int8::sub",
        "std::Int8::mul",
        "std::Int8::div",
        "std::Int8::band",
        "std::Int8::bor",
        "std::Int8::bxor",
        "std::Int8::negate",
        "std::Int16::eq",
        "std::Int16::ne",
        "std::Int16::lt",
        "std::Int16::le",
        "std::Int16::gt",
        "std::Int16::ge",
        "std::Int16::add",
        "std::Int16::sub",
        "std::Int16::mul",
        "std::Int16::div",
        "std::Int16::band",
        "std::Int16::bor",
        "std::Int16::bxor",
        "std::Int16::negate",
        "std::Int32::eq",
        "std::Int32::ne",
        "std::Int32::lt",
        "std::Int32::le",
        "std::Int32::gt",
        "std::Int32::ge",
        "std::Int32::add",
        "std::Int32::sub",
        "std::Int32::mul",
        "std::Int32::div",
        "std::Int32::band",
        "std::Int32::bor",
        "std::Int32::bxor",
        "std::Int32::negate",
        "std::Int64::eq",
        "std::Int64::ne",
        "std::Int64::lt",
        "std::Int64::le",
        "std::Int64::gt",
        "std::Int64::ge",
        "std::Int64::add",
        "std::Int64::sub",
        "std::Int64::mul",
        "std::Int64::div",
        "std::Int64::band",
        "std::Int64::bor",
        "std::Int64::bxor",
        "std::Int64::negate",
        "std::UInt8::eq",
        "std::UInt8::ne",
        "std::UInt8::lt",
        "std::UInt8::le",
        "std::UInt8::gt",
        "std::UInt8::ge",
        "std::UInt8::add",
        "std::UInt8::sub",
        "std::UInt8::mul",
        "std::UInt8::div",
        "std::UInt8::band",
        "std::UInt8::bor",
        "std::UInt8::bxor",
        "std::UInt8::negate",
        "std::UInt16::eq",
        "std::UInt16::ne",
        "std::UInt16::lt",
        "std::UInt16::le",
        "std::UInt16::gt",
        "std::UInt16::ge",
        "std::UInt16::add",
        "std::UInt16::sub",
        "std::UInt16::mul",
        "std::UInt16::div",
        "std::UInt16::band",
        "std::UInt16::bor",
        "std::UInt16::bxor",
        "std::UInt16::negate",
        "std::UInt32::eq",
        "std::UInt32::ne",
        "std::UInt32::lt",
        "std::UInt32::le",
        "std::UInt32::gt",
        "std::UInt32::ge",
        "std::UInt32::add",
        "std::UInt32::sub",
        "std::UInt32::mul",
        "std::UInt32::div",
        "std::UInt32::band",
        "std::UInt32::bor",
        "std::UInt32::bxor",
        "std::UInt32::negate",
        "std::UInt64::eq",
        "std::UInt64::ne",
        "std::UInt64::lt",
        "std::UInt64::le",
        "std::UInt64::gt",
        "std::UInt64::ge",
        "std::UInt64::add",
        "std::UInt64::sub",
        "std::UInt64::mul",
        "std::UInt64::div",
        "std::UInt64::band",
        "std::UInt64::bor",
        "std::UInt64::bxor",
        "std::UInt64::negate",
        "std::Float::eq",
        "std::Float::ne",
        "std::Float::lt",
        "std::Float::le",
        "std::Float::gt",
        "std::Float::ge",
        "std::Float::add",
        "std::Float::sub",
        "std::Float::mul",
        "std::Float::div",
        "std::Float::negate",
        "std::Double::eq",
        "std::Double::ne",
        "std::Double::lt",
        "std::Double::le",
        "std::Double::gt",
        "std::Double::ge",
        "std::Double::add",
        "std::Double::sub",
        "std::Double::mul",
        "std::Double::div",
        "std::Double::negate",
    ])
});

fn std_type_id(name: &Path) -> Option<NodeId> {
    match name {
        n if n.is_name_match(&Path::void()) => Some(lume_types::TYPEREF_VOID_ID),
        n if n.is_name_match(&Path::boolean()) => Some(lume_types::TYPEREF_BOOL_ID),
        n if n.is_name_match(&Path::i8()) => Some(lume_types::TYPEREF_INT8_ID),
        n if n.is_name_match(&Path::i16()) => Some(lume_types::TYPEREF_INT16_ID),
        n if n.is_name_match(&Path::i32()) => Some(lume_types::TYPEREF_INT32_ID),
        n if n.is_name_match(&Path::i64()) => Some(lume_types::TYPEREF_INT64_ID),
        n if n.is_name_match(&Path::u8()) => Some(lume_types::TYPEREF_UINT8_ID),
        n if n.is_name_match(&Path::u16()) => Some(lume_types::TYPEREF_UINT16_ID),
        n if n.is_name_match(&Path::u32()) => Some(lume_types::TYPEREF_UINT32_ID),
        n if n.is_name_match(&Path::u64()) => Some(lume_types::TYPEREF_UINT64_ID),
        n if n.is_name_match(&Path::f32()) => Some(lume_types::TYPEREF_FLOAT32_ID),
        n if n.is_name_match(&Path::f64()) => Some(lume_types::TYPEREF_FLOAT64_ID),
        _ => None,
    }
}

impl TyInferCtx {
    #[libftrace::traced(level = Debug)]
    pub(crate) fn define_items(&mut self) -> Result<()> {
        let type_items = self
            .hir
            .nodes()
            .iter()
            .filter_map(|(&id, node)| match node {
                lume_hir::Node::Type(lume_hir::TypeDefinition::Struct(_)) => Some((id, NodeType::StructDef)),
                lume_hir::Node::Type(lume_hir::TypeDefinition::Trait(_)) => Some((id, NodeType::TraitDef)),
                lume_hir::Node::Type(lume_hir::TypeDefinition::Enum(_)) => Some((id, NodeType::EnumDef)),
                _ => None,
            })
            .collect::<Vec<_>>();

        for (id, kind) in type_items {
            match kind {
                NodeType::StructDef => self.define_struct_type_definition(id)?,
                NodeType::TraitDef => self.define_trait_type_definition(id)?,
                NodeType::EnumDef => self.define_enum_type_definition(id)?,
                _ => unreachable!(),
            }
        }

        Ok(())
    }

    fn define_struct_type_definition(&mut self, id: NodeId) -> Result<()> {
        let Some(lume_hir::Node::Type(lume_hir::TypeDefinition::Struct(struct_def))) = self.hir.node_mut(id) else {
            unreachable!()
        };

        let name = struct_def.name.clone();

        if let Some(std_id) = std_type_id(&name) {
            struct_def.id = std_id;

            let (idx, _, node) = self.hir.nodes.shift_remove_full(&id).unwrap();
            self.hir.nodes.insert_before(idx, std_id, node);
        } else {
            self.tcx.db_mut().type_alloc(struct_def.id, &name, TypeKind::Struct);
        };

        Ok(())
    }

    fn define_trait_type_definition(&mut self, id: NodeId) -> Result<()> {
        let lume_hir::Node::Type(lume_hir::TypeDefinition::Trait(trait_def)) = self.hir.expect_node(id)? else {
            unreachable!()
        };

        self.tcx
            .db_mut()
            .type_alloc(trait_def.id, &trait_def.name, TypeKind::Trait);

        Ok(())
    }

    fn define_enum_type_definition(&mut self, id: NodeId) -> Result<()> {
        let lume_hir::Node::Type(lume_hir::TypeDefinition::Enum(enum_def)) = self.hir.expect_node(id)? else {
            unreachable!()
        };

        self.tcx
            .db_mut()
            .type_alloc(enum_def.id, &enum_def.name, TypeKind::Enum);

        Ok(())
    }
}

impl TyInferCtx {
    #[libftrace::traced(level = Debug)]
    pub(crate) fn define_methods(&mut self) -> Result<()> {
        let trait_items = self
            .hir
            .nodes()
            .iter()
            .filter_map(|(&id, node)| match node {
                lume_hir::Node::Type(lume_hir::TypeDefinition::Trait(_)) => Some((id, NodeType::TraitDef)),
                lume_hir::Node::TraitImpl(_) => Some((id, NodeType::TraitImpl)),
                lume_hir::Node::Impl(_) => Some((id, NodeType::Impl)),
                lume_hir::Node::Function(_) => Some((id, NodeType::Function)),
                _ => None,
            })
            .collect::<Vec<_>>();

        for (id, kind) in trait_items {
            match kind {
                NodeType::TraitDef => self.define_trait_definition_methods(id)?,
                NodeType::TraitImpl => self.define_trait_implementation_methods(id)?,
                NodeType::Impl => self.define_implementation_methods(id)?,
                NodeType::Function => self.define_function(id)?,
                _ => unreachable!(),
            }
        }

        Ok(())
    }

    fn define_trait_definition_methods(&mut self, id: NodeId) -> Result<()> {
        let lume_hir::Node::Type(lume_hir::TypeDefinition::Trait(trait_def)) = self.hir.expect_node(id)? else {
            unreachable!()
        };

        let type_params = self.as_type_params(&trait_def.type_parameters)?;
        let type_ref = self.mk_type_ref_generic(
            &lume_hir::Type {
                id: lume_hir::TypeId::from(trait_def.id),
                name: trait_def.name.clone(),
                location: trait_def.location,
            },
            &type_params,
        )?;

        for method in &trait_def.methods {
            let method_name = method.name.clone();
            let mut qualified_name =
                Path::with_root(trait_def.name.clone(), PathSegment::callable(method_name.clone()));

            qualified_name.location = method_name.location;

            self.tcx.db_mut().method_alloc(
                method.id,
                type_ref.clone(),
                qualified_name,
                lume_types::MethodKind::TraitDefinition,
            );
        }

        Ok(())
    }

    fn define_trait_implementation_methods(&mut self, id: NodeId) -> Result<()> {
        let lume_hir::Node::TraitImpl(trait_impl) = self.hir.expect_node(id)? else {
            unreachable!()
        };

        let type_params = self.as_type_params(&trait_impl.type_parameters)?;
        let trait_type_ref = self.mk_type_ref_generic(trait_impl.name.as_ref(), &type_params)?;
        let target_type_ref = self.mk_type_ref_generic(trait_impl.target.as_ref(), &type_params)?;

        self.tcx.db_mut().traits.add_impl(id, &trait_type_ref, &target_type_ref);

        for method in &trait_impl.methods {
            let method_name = method.name.clone();
            let mut qualified_name = Path::with_root(
                trait_impl.target.name.clone(),
                PathSegment::callable(method_name.clone()),
            );

            qualified_name.location = method_name.location;

            let method_kind = if INTRINSIC_METHODS.contains(format!("{qualified_name:+}").as_str()) {
                lume_types::MethodKind::Intrinsic
            } else {
                lume_types::MethodKind::TraitImplementation
            };

            self.tcx
                .db_mut()
                .method_alloc(method.id, target_type_ref.clone(), qualified_name, method_kind);

            self.tcx
                .db_mut()
                .traits
                .add_impl_method(&trait_type_ref, &target_type_ref, method.id);
        }

        Ok(())
    }

    #[libftrace::traced(level = Trace)]
    fn define_implementation_methods(&mut self, id: NodeId) -> Result<()> {
        let lume_hir::Node::Impl(implementation) = self.hir.expect_node(id)? else {
            unreachable!()
        };

        let type_params = self.as_type_params(&implementation.type_parameters)?;
        let type_ref = self.mk_type_ref_generic(implementation.target.as_ref(), &type_params)?;

        let is_type_intrinsic = type_ref.is_bool() || type_ref.is_integer() || type_ref.is_float();

        for method in &implementation.methods {
            let method_name = method.name.clone();

            let mut qualified_name = Path::with_root(
                implementation.target.name.clone(),
                PathSegment::callable(method_name.clone()),
            );

            let method_kind = if is_type_intrinsic && INTRINSIC_METHODS.contains(format!("{qualified_name:+}").as_str())
            {
                lume_types::MethodKind::Intrinsic
            } else {
                lume_types::MethodKind::Implementation
            };

            qualified_name.location = method_name.location;

            self.tcx
                .db_mut()
                .method_alloc(method.id, type_ref.clone(), qualified_name, method_kind);
        }

        Ok(())
    }

    fn define_function(&mut self, id: NodeId) -> Result<()> {
        let lume_hir::Node::Function(func) = self.hir.expect_node(id)? else {
            unreachable!()
        };

        let name = func.name.clone();

        self.tcx.db_mut().func_alloc(func.id, name);

        Ok(())
    }
}

impl TyInferCtx {
    #[libftrace::traced(level = Debug)]
    pub(crate) fn define_type_parameters(&mut self) -> Result<()> {
        let typed_items = self
            .hir
            .nodes()
            .iter()
            .filter_map(|(&id, node)| match node {
                lume_hir::Node::Type(lume_hir::TypeDefinition::Struct(_)) => Some((id, NodeType::StructDef)),
                lume_hir::Node::Type(lume_hir::TypeDefinition::Enum(_)) => Some((id, NodeType::EnumDef)),
                lume_hir::Node::Type(lume_hir::TypeDefinition::Trait(_)) => Some((id, NodeType::TraitDef)),
                lume_hir::Node::TraitImpl(_) => Some((id, NodeType::TraitImpl)),
                lume_hir::Node::Impl(_) => Some((id, NodeType::Impl)),
                lume_hir::Node::Function(_) => Some((id, NodeType::Function)),
                _ => None,
            })
            .collect::<Vec<_>>();

        for (id, kind) in typed_items {
            match kind {
                NodeType::StructDef => self.define_struct_definition_type_parameters(id)?,
                NodeType::EnumDef => self.define_enum_definition_type_parameters(id)?,
                NodeType::TraitDef => self.define_trait_definition_type_parameters(id)?,
                NodeType::TraitImpl => self.define_trait_implementation_type_parameters(id)?,
                NodeType::Impl => self.define_implementation_type_parameters(id)?,
                NodeType::Function => self.define_function_type_parameters(id)?,
                _ => unreachable!(),
            }
        }

        Ok(())
    }

    #[libftrace::traced(level = Trace)]
    fn define_struct_definition_type_parameters(&mut self, id: NodeId) -> Result<()> {
        let lume_hir::Node::Type(lume_hir::TypeDefinition::Struct(struct_def)) = self.hir.expect_node(id)? else {
            unreachable!()
        };

        for &type_param_id in &struct_def.type_parameters {
            let type_param_name = self.hir_expect_type_parameter(type_param_id).name.clone();

            self.tcx.db_mut().type_alloc(
                type_param_id,
                &Path::rooted(PathSegment::ty(type_param_name)),
                TypeKind::TypeParameter,
            );
        }

        Ok(())
    }

    #[libftrace::traced(level = Trace)]
    fn define_enum_definition_type_parameters(&mut self, id: NodeId) -> Result<()> {
        let lume_hir::Node::Type(lume_hir::TypeDefinition::Enum(enum_def)) = self.hir.expect_node(id)? else {
            unreachable!()
        };

        for &type_param_id in &enum_def.type_parameters {
            let type_param_name = self.hir_expect_type_parameter(type_param_id).name.clone();

            self.tcx.db_mut().type_alloc(
                type_param_id,
                &Path::rooted(PathSegment::ty(type_param_name)),
                TypeKind::TypeParameter,
            );
        }

        Ok(())
    }

    #[libftrace::traced(level = Trace)]
    fn define_trait_definition_type_parameters(&mut self, id: NodeId) -> Result<()> {
        let lume_hir::Node::Type(lume_hir::TypeDefinition::Trait(trait_def)) = self.hir.expect_node(id)? else {
            unreachable!()
        };

        for &type_param_id in &trait_def.type_parameters {
            let type_param_name = self.hir_expect_type_parameter(type_param_id).name.clone();

            self.tcx.db_mut().type_alloc(
                type_param_id,
                &Path::rooted(PathSegment::ty(type_param_name)),
                TypeKind::TypeParameter,
            );
        }

        for method in &trait_def.methods {
            for &type_param_id in &method.type_parameters {
                let type_param_name = self.hir_expect_type_parameter(type_param_id).name.clone();

                self.tcx.db_mut().type_alloc(
                    type_param_id,
                    &Path::rooted(PathSegment::ty(type_param_name)),
                    TypeKind::TypeParameter,
                );
            }
        }

        Ok(())
    }

    #[libftrace::traced(level = Trace)]
    fn define_trait_implementation_type_parameters(&mut self, id: NodeId) -> Result<()> {
        let lume_hir::Node::TraitImpl(trait_impl) = self.hir.expect_node(id)? else {
            unreachable!()
        };

        for &type_param_id in &trait_impl.type_parameters {
            let type_param_name = self.hir_expect_type_parameter(type_param_id).name.clone();

            self.tcx.db_mut().type_alloc(
                type_param_id,
                &Path::rooted(PathSegment::ty(type_param_name)),
                TypeKind::TypeParameter,
            );
        }

        for method in &trait_impl.methods {
            for &type_param_id in &method.type_parameters {
                let type_param_name = self.hir_expect_type_parameter(type_param_id).name.clone();

                self.tcx.db_mut().type_alloc(
                    type_param_id,
                    &Path::rooted(PathSegment::ty(type_param_name)),
                    TypeKind::TypeParameter,
                );
            }
        }

        Ok(())
    }

    #[libftrace::traced(level = Trace)]
    fn define_implementation_type_parameters(&mut self, id: NodeId) -> Result<()> {
        let lume_hir::Node::Impl(implementation) = self.hir.expect_node(id)? else {
            unreachable!()
        };

        for &type_param_id in &implementation.type_parameters {
            let type_param_name = self.hir_expect_type_parameter(type_param_id).name.clone();

            self.tcx.db_mut().type_alloc(
                type_param_id,
                &Path::rooted(PathSegment::ty(type_param_name)),
                TypeKind::TypeParameter,
            );
        }

        for method in &implementation.methods {
            for &type_param_id in &method.type_parameters {
                let type_param_name = self.hir_expect_type_parameter(type_param_id).name.clone();

                self.tcx.db_mut().type_alloc(
                    type_param_id,
                    &Path::rooted(PathSegment::ty(type_param_name)),
                    TypeKind::TypeParameter,
                );
            }
        }

        Ok(())
    }

    #[libftrace::traced(level = Trace)]
    fn define_function_type_parameters(&mut self, id: NodeId) -> Result<()> {
        let lume_hir::Node::Function(function) = self.hir.expect_node(id)? else {
            unreachable!()
        };

        for &type_param_id in &function.type_parameters {
            let type_param_name = self.hir_expect_type_parameter(type_param_id).name.clone();

            self.tcx.db_mut().type_alloc(
                type_param_id,
                &Path::rooted(PathSegment::ty(type_param_name)),
                TypeKind::TypeParameter,
            );
        }

        Ok(())
    }
}

impl TyInferCtx {
    #[libftrace::traced(level = Debug)]
    pub(crate) fn define_scopes(&mut self) -> Result<()> {
        let mut tree = BTreeMap::new();

        for (_, item) in &self.hir.nodes {
            match item {
                lume_hir::Node::Type(ty) => match ty {
                    lume_hir::TypeDefinition::Struct(f) => self.define_struct_scope(&mut tree, f)?,
                    lume_hir::TypeDefinition::Trait(f) => self.define_trait_scope(&mut tree, f)?,
                    lume_hir::TypeDefinition::Enum(_) | lume_hir::TypeDefinition::TypeParameter(_) => {}
                },
                lume_hir::Node::Impl(f) => self.define_impl_scope(&mut tree, f)?,
                lume_hir::Node::TraitImpl(f) => self.define_use_scope(&mut tree, f)?,
                lume_hir::Node::Function(f) => self.define_function_scope(&mut tree, f)?,
                _ => {}
            }
        }

        self.ancestry = tree;

        Ok(())
    }

    fn define_struct_scope(
        &self,
        tree: &mut BTreeMap<NodeId, NodeId>,
        struct_def: &lume_hir::StructDefinition,
    ) -> Result<()> {
        let parent = struct_def.id;

        for &type_param in &struct_def.type_parameters {
            tree.insert(type_param, parent);
        }

        for field in &struct_def.fields {
            if let Some(existing) = tree.insert(field.id, parent) {
                assert_eq!(existing, parent);
            }

            if let Some(default) = field.default_value {
                self.define_expr_scope(tree, default, field.id)?;
            }
        }

        Ok(())
    }

    fn define_trait_scope(
        &self,
        tree: &mut BTreeMap<NodeId, NodeId>,
        trait_def: &lume_hir::TraitDefinition,
    ) -> Result<()> {
        let parent = trait_def.id;

        for &type_param in &trait_def.type_parameters {
            tree.insert(type_param, parent);
        }

        for method in &trait_def.methods {
            if let Some(existing) = tree.insert(method.id, parent) {
                assert_eq!(existing, parent);
            }

            for &type_param in &trait_def.type_parameters {
                tree.insert(type_param, method.id);
            }

            if let Some(block) = &method.block {
                self.define_block_scope(tree, block, method.id)?;
            }
        }

        Ok(())
    }

    fn define_impl_scope(
        &self,
        tree: &mut BTreeMap<NodeId, NodeId>,
        implementation: &lume_hir::Implementation,
    ) -> Result<()> {
        let parent = implementation.id;

        for &type_param in &implementation.type_parameters {
            tree.insert(type_param, parent);
        }

        for method in &implementation.methods {
            if let Some(existing) = tree.insert(method.id, parent) {
                assert_eq!(existing, parent);
            }

            for &type_param in &method.type_parameters {
                tree.insert(type_param, method.id);
            }

            if let Some(block) = &method.block {
                self.define_block_scope(tree, block, method.id)?;
            }
        }

        Ok(())
    }

    fn define_use_scope(
        &self,
        tree: &mut BTreeMap<NodeId, NodeId>,
        trait_impl: &lume_hir::TraitImplementation,
    ) -> Result<()> {
        let parent = trait_impl.id;

        for &type_param in &trait_impl.type_parameters {
            tree.insert(type_param, parent);
        }

        for method in &trait_impl.methods {
            if let Some(existing) = tree.insert(method.id, parent) {
                assert_eq!(existing, parent);
            }

            for &type_param in &method.type_parameters {
                tree.insert(type_param, method.id);
            }

            if let Some(block) = &method.block {
                self.define_block_scope(tree, block, method.id)?;
            }
        }

        Ok(())
    }

    fn define_function_scope(
        &self,
        tree: &mut BTreeMap<NodeId, NodeId>,
        func: &lume_hir::FunctionDefinition,
    ) -> Result<()> {
        let parent = func.id;

        for &type_param in &func.type_parameters {
            tree.insert(type_param, parent);
        }

        if let Some(block) = &func.block {
            self.define_block_scope(tree, block, parent)?;
        }

        Ok(())
    }

    fn define_block_scope(
        &self,
        tree: &mut BTreeMap<NodeId, NodeId>,
        block: &lume_hir::Block,
        parent: NodeId,
    ) -> Result<()> {
        if !self.hir_is_local_node(block.id) {
            return Ok(());
        }

        for stmt in &block.statements {
            self.define_stmt_scope(tree, *stmt, parent)?;
        }

        Ok(())
    }

    fn define_stmt_scope(&self, tree: &mut BTreeMap<NodeId, NodeId>, stmt_id: NodeId, parent: NodeId) -> Result<()> {
        if let Some(existing) = tree.insert(stmt_id, parent) {
            assert_eq!(existing, parent);
        }

        let stmt = self.hir.statement(stmt_id).unwrap();

        match &stmt.kind {
            lume_hir::StatementKind::Variable(s) => self.define_expr_scope(tree, s.value, stmt_id),
            lume_hir::StatementKind::Break(_) | lume_hir::StatementKind::Continue(_) => Ok(()),
            lume_hir::StatementKind::Final(s) => {
                self.define_expr_scope(tree, s.value, stmt_id)?;

                Ok(())
            }
            lume_hir::StatementKind::Return(s) => {
                if let Some(value) = s.value {
                    self.define_expr_scope(tree, value, stmt_id)
                } else {
                    Ok(())
                }
            }
            lume_hir::StatementKind::InfiniteLoop(s) => self.define_block_scope(tree, &s.block, stmt_id),
            lume_hir::StatementKind::IteratorLoop(s) => {
                self.define_block_scope(tree, &s.block, stmt_id)?;
                self.define_expr_scope(tree, s.collection, stmt_id)?;

                Ok(())
            }
            lume_hir::StatementKind::Expression(s) => self.define_expr_scope(tree, *s, stmt_id),
        }
    }

    fn define_condition_scope(
        &self,
        tree: &mut BTreeMap<NodeId, NodeId>,
        cond: &lume_hir::Condition,
        parent: NodeId,
    ) -> Result<()> {
        if let Some(condition) = cond.condition {
            self.define_expr_scope(tree, condition, parent)?;
        }

        self.define_block_scope(tree, &cond.block, parent)?;

        Ok(())
    }

    fn define_expr_scope(&self, tree: &mut BTreeMap<NodeId, NodeId>, expr_id: NodeId, parent: NodeId) -> Result<()> {
        if let Some(existing) = tree.insert(expr_id, parent) {
            assert_eq!(
                existing, parent,
                "expected the parent of {expr_id:?}\n\tto be {existing:?},\n\tfound {parent:?}"
            );
        }

        let expr = self.hir.expression(expr_id).unwrap();

        match &expr.kind {
            lume_hir::ExpressionKind::Assignment(s) => {
                self.define_expr_scope(tree, s.target, expr_id)?;
                self.define_expr_scope(tree, s.value, expr_id)?;

                Ok(())
            }
            lume_hir::ExpressionKind::Cast(s) => self.define_expr_scope(tree, s.source, expr_id),
            lume_hir::ExpressionKind::Construct(s) => {
                for field in &s.fields {
                    self.define_expr_scope(tree, field.value, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::InstanceCall(s) => {
                self.define_expr_scope(tree, s.callee, expr_id)?;

                for arg in &s.arguments {
                    self.define_expr_scope(tree, *arg, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::IntrinsicCall(s) => {
                for arg in s.kind.arguments() {
                    self.define_expr_scope(tree, arg, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::If(s) => {
                for case in &s.cases {
                    self.define_condition_scope(tree, case, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::Is(s) => {
                self.define_expr_scope(tree, s.target, expr_id)?;
                self.define_pat_scope(tree, &s.pattern, expr_id)?;

                Ok(())
            }
            lume_hir::ExpressionKind::Member(s) => self.define_expr_scope(tree, s.callee, expr_id),
            lume_hir::ExpressionKind::StaticCall(s) => {
                for arg in &s.arguments {
                    self.define_expr_scope(tree, *arg, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::Scope(s) => {
                for stmt in &s.body {
                    self.define_stmt_scope(tree, *stmt, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::Switch(s) => {
                self.define_expr_scope(tree, s.operand, expr_id)?;

                for case in &s.cases {
                    self.define_pat_scope(tree, &case.pattern, expr_id)?;
                    self.define_expr_scope(tree, case.branch, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::Variant(variant) => {
                for field in &variant.arguments {
                    self.define_expr_scope(tree, *field, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::Literal(_) | lume_hir::ExpressionKind::Variable(_) => Ok(()),
        }
    }

    #[allow(clippy::self_only_used_in_recursion, reason = "pedantic")]
    fn define_pat_scope(
        &self,
        tree: &mut BTreeMap<NodeId, NodeId>,
        pat: &lume_hir::Pattern,
        parent: NodeId,
    ) -> Result<()> {
        if let Some(existing) = tree.insert(pat.id, parent) {
            assert_eq!(existing, parent);
        }

        match &pat.kind {
            lume_hir::PatternKind::Literal(_)
            | lume_hir::PatternKind::Identifier(_)
            | lume_hir::PatternKind::Wildcard(_) => Ok(()),
            lume_hir::PatternKind::Variant(var) => {
                for field in &var.fields {
                    self.define_pat_scope(tree, field, pat.id)?;
                }

                Ok(())
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
enum TypeArgumentInference {
    /// All type arguments within the expression or type are all
    /// already defined and have no need for inference.
    Fulfilled,

    /// One-or-more type arguments were inferred and have been
    /// defined the given `replacement` field.
    Replace { replacement: Vec<lume_hir::Type> },
}

impl TyInferCtx {
    #[libftrace::traced(level = Debug)]
    pub(crate) fn infer_type_arguments(&mut self) -> Result<()> {
        let mut replacements = HashMap::new();

        for &id in self.hir.nodes.keys().rev() {
            let mut expr = match self.hir.expression(id) {
                Some(expr) => expr.to_owned(),
                None => continue,
            };

            match &mut expr.kind {
                lume_hir::ExpressionKind::InstanceCall(call) => {
                    let callable = self.probe_callable_instance(call)?;

                    let replacement = match self
                        .infer_type_arguments_callable(lume_hir::CallExpression::Instanced(call), callable)
                        .unwrap()
                    {
                        TypeArgumentInference::Fulfilled => continue,
                        TypeArgumentInference::Replace { replacement } => replacement,
                    };

                    call.name.place_bound_types(replacement);
                }
                lume_hir::ExpressionKind::IntrinsicCall(call) => {
                    self.probe_callable_intrinsic(call)?;
                }
                lume_hir::ExpressionKind::StaticCall(call) => {
                    let callable = self.probe_callable_static(call)?;

                    match self.infer_type_arguments_callable(lume_hir::CallExpression::Static(call), callable)? {
                        TypeArgumentInference::Fulfilled => (),
                        TypeArgumentInference::Replace { replacement } => {
                            call.name.place_bound_types(replacement);
                        }
                    }
                }
                _ => continue,
            }

            replacements.insert(id, expr);
        }

        for (expr_id, expr) in replacements {
            self.hir.nodes.insert(expr_id, Node::Expression(expr));
        }

        self.tcx.dcx().ensure_untainted()?;

        Ok(())
    }

    fn infer_type_arguments_callable(
        &self,
        expr: lume_hir::CallExpression<'_>,
        callable: Callable<'_>,
    ) -> Result<TypeArgumentInference> {
        let signature = self.signature_of(callable)?;

        let params = signature.params;
        let args = self.hir().expect_expressions(&expr.arguments())?;

        let mut type_args = expr.type_arguments().to_vec();
        let type_params = signature.type_params;

        // All all the type arguments have already been declared, theres
        // nothing for us to infer.
        if type_args.len() >= type_params.len() {
            return Ok(TypeArgumentInference::Fulfilled);
        }

        for &type_param in type_params.iter().skip(type_args.len()) {
            if let Some(inferred_type_arg) = self.infer_type_arg_param(type_param, &params, &args)? {
                type_args.push(self.hir_lift_type(&inferred_type_arg)?);
            } else {
                let type_param_name = self.hir_expect_type_parameter(type_param).name.to_string();

                self.dcx().emit(
                    crate::errors::TypeArgumentInferenceFailedCallable {
                        source: expr.location(),
                        type_param_name,
                        callable_name: format!("{:+}", callable.name().to_string()),
                    }
                    .into(),
                );
            }
        }

        Ok(TypeArgumentInference::Replace { replacement: type_args })
    }

    /// Attempts to infer the type of the type parameter, given the arguments
    /// and parameter types.
    pub(crate) fn infer_type_arg_param(
        &self,
        type_param: NodeId,
        params: &[lume_types::Parameter],
        args: &[&lume_hir::Expression],
    ) -> Result<Option<TypeRef>> {
        for (param, arg) in params.iter().zip(args.iter()) {
            let expr_ty = self.type_of_expr(arg)?;

            if let Some(inferred_type) = self.infer_type_arg_param_nested(type_param, &param.ty, &expr_ty)? {
                return Ok(Some(inferred_type));
            }
        }

        Ok(None)
    }

    /// Attempts to infer a type argument from within a nested parameter type.
    ///
    /// The method takes three parameters:
    /// - `target_param_id`: The ID of the type parameter which we wish to infer
    ///   the type from.
    /// - `param_ty`: The type of some parameter.
    /// - `arg_ty`: The type of the argument which corresponds to the parameter.
    ///
    /// If the parameter is already a type parameter which corresponds to the
    /// target ID, the type of the parameter is returned. If not, the method
    /// will iterate over type arguments within the parameter- and
    /// argument-types. For example, given the given Lume sample:
    /// ```lm
    /// struct Test<T> {}
    ///
    /// fn foo<T>(val: Test<T>) { }
    ///
    /// fn main() {
    ///     let t = Test<Int32> { };
    ///
    ///     foo(t);
    /// }
    /// ```
    ///
    /// From the given sample, we'd want to resolve `T` to be `Int32`, since
    /// they are both contained within the type `Test`. As such, the method
    /// iterates over the type parameters within the `param_ty` and their
    /// corresponding `arg_ty` type argument. When the type parameter
    /// of `para_ty` is matched against the target parameter ID, the
    /// corresponding type argument is returned.
    fn infer_type_arg_param_nested(
        &self,
        target_param_id: NodeId,
        param_ty: &lume_types::TypeRef,
        arg_ty: &lume_types::TypeRef,
    ) -> Result<Option<TypeRef>> {
        if let Some(param_ty_param) = self.as_type_parameter(param_ty)?
            && param_ty_param.id == target_param_id
        {
            return Ok(Some(arg_ty.to_owned()));
        }

        if param_ty.bound_types.len() != arg_ty.bound_types.len() {
            return Ok(None);
        }

        for (type_param, type_arg) in param_ty.bound_types.iter().zip(arg_ty.bound_types.iter()) {
            if let Some(inferred_type) = self.infer_type_arg_param_nested(target_param_id, type_param, type_arg)? {
                return Ok(Some(inferred_type));
            }
        }

        Ok(None)
    }
}
