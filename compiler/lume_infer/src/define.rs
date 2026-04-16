use std::collections::BTreeMap;

use lume_errors::Result;
use lume_hir::{NodeType, Path, PathSegment};
use lume_span::*;
use lume_types::TypeKind;

use crate::TyInferCtx;

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
    #[tracing::instrument(level = "DEBUG", skip_all)]
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
                NodeType::StructDef => self.define_struct_type_definition(id),
                NodeType::TraitDef => self.define_trait_type_definition(id)?,
                NodeType::EnumDef => self.define_enum_type_definition(id)?,
                _ => unreachable!(),
            }
        }

        Ok(())
    }

    fn define_struct_type_definition(&mut self, id: NodeId) {
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
        }
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
    #[tracing::instrument(level = "DEBUG", skip_all)]
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
                self_type: false,
                location: trait_def.location,
            },
            &type_params,
        )?;

        for method in &trait_def.methods {
            let method_name = method.signature.name.name().clone();
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
            let method_name = method.signature.name.name.clone();
            let mut qualified_name = Path::with_root(trait_impl.target.name.clone(), method_name);

            qualified_name.location = method.signature.name.location();

            let method_kind = if self.hir_is_instrinsic(method.id) {
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

    #[tracing::instrument(level = "Trace", skip_all)]
    fn define_implementation_methods(&mut self, id: NodeId) -> Result<()> {
        let lume_hir::Node::Impl(implementation) = self.hir.expect_node(id)? else {
            unreachable!()
        };

        let type_params = self.as_type_params(&implementation.type_parameters)?;
        let type_ref = self.mk_type_ref_generic(implementation.target.as_ref(), &type_params)?;

        let is_type_intrinsic = type_ref.is_bool() || type_ref.is_integer() || type_ref.is_float();

        for method in &implementation.methods {
            let method_name = method.signature.name.name().clone();

            let mut qualified_name = Path::with_root(
                implementation.target.name.clone(),
                PathSegment::callable(method_name.clone()),
            );

            let method_kind = if is_type_intrinsic && self.hir_is_instrinsic(method.id) {
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

        let name = func.signature.name.clone();

        self.tcx.db_mut().func_alloc(func.id, name);

        Ok(())
    }
}

impl TyInferCtx {
    #[tracing::instrument(level = "DEBUG", skip_all)]
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

    #[tracing::instrument(level = "Trace", skip_all)]
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

    #[tracing::instrument(level = "Trace", skip_all)]
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

    #[tracing::instrument(level = "Trace", skip_all)]
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
            for &type_param_id in &method.signature.type_parameters {
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

    #[tracing::instrument(level = "Trace", skip_all)]
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
            for &type_param_id in &method.signature.type_parameters {
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

    #[tracing::instrument(level = "Trace", skip_all)]
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
            for &type_param_id in &method.signature.type_parameters {
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

    #[tracing::instrument(level = "Trace", skip_all)]
    fn define_function_type_parameters(&mut self, id: NodeId) -> Result<()> {
        let lume_hir::Node::Function(function) = self.hir.expect_node(id)? else {
            unreachable!()
        };

        for &type_param_id in &function.signature.type_parameters {
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
    #[tracing::instrument(level = "DEBUG", skip_all)]
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

            for &type_param in &method.signature.type_parameters {
                tree.insert(type_param, method.id);
            }

            for parameter in &method.signature.parameters {
                tree.insert(parameter.id, method.id);
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

            for &type_param in &method.signature.type_parameters {
                tree.insert(type_param, method.id);
            }

            for parameter in &method.signature.parameters {
                tree.insert(parameter.id, method.id);
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

            for &type_param in &method.signature.type_parameters {
                tree.insert(type_param, method.id);
            }

            for parameter in &method.signature.parameters {
                tree.insert(parameter.id, method.id);
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

        for &type_param in &func.signature.type_parameters {
            tree.insert(type_param, parent);
        }

        for parameter in &func.signature.parameters {
            tree.insert(parameter.id, parent);
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
        debug_assert_ne!(stmt_id, parent);

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
        debug_assert_ne!(expr_id, parent);

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
            lume_hir::ExpressionKind::Deref(s) => self.define_expr_scope(tree, s.target, expr_id),
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
                self.define_pat_scope(tree, s.pattern, expr_id)?;

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
                    self.define_pat_scope(tree, case.pattern, expr_id)?;
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
            lume_hir::ExpressionKind::Literal(_)
            | lume_hir::ExpressionKind::Variable(_)
            | lume_hir::ExpressionKind::Missing => Ok(()),
        }
    }

    #[allow(clippy::self_only_used_in_recursion, reason = "pedantic")]
    fn define_pat_scope(&self, tree: &mut BTreeMap<NodeId, NodeId>, pattern_id: NodeId, parent: NodeId) -> Result<()> {
        if let Some(existing) = tree.insert(pattern_id, parent) {
            assert_eq!(existing, parent);
        }

        let pattern = self.hir.expect_pattern(pattern_id).unwrap();

        match &pattern.kind {
            lume_hir::PatternKind::Literal(_)
            | lume_hir::PatternKind::Identifier(_)
            | lume_hir::PatternKind::Wildcard(_)
            | lume_hir::PatternKind::Missing => Ok(()),
            lume_hir::PatternKind::Variant(var) => {
                for &field in &var.fields {
                    self.define_pat_scope(tree, field, pattern_id)?;
                }

                Ok(())
            }
        }
    }
}
