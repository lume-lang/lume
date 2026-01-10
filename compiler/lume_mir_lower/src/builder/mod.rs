pub mod decl;
pub mod intrinsic;
pub mod lower;
pub mod pattern;
pub mod ty;

use std::collections::HashMap;

use lume_mir::*;
use lume_mir_queries::MirQueryCtx;
use lume_span::{Location, NodeId};
use lume_type_metadata::TypeMetadataId;
use lume_typech::TyCheckCtx;
use lume_types::TypeRef;

use crate::builder::decl::OperandRef;

pub(crate) struct Builder<'mir, 'tcx> {
    /// Parent MIR query context
    pub(crate) mcx: &'mir MirQueryCtx<'tcx>,

    /// Defines the MIR function which is being created.
    pub(crate) func: lume_mir::Function,

    /// Map of all metadata declarations within the current function, mapping
    /// each type to the containing register.
    metadata_registers: HashMap<(BasicBlockId, TypeMetadataId), RegisterId>,
}

impl<'mir, 'tcx> Builder<'mir, 'tcx> {
    pub(crate) fn create_from(mcx: &'mir MirQueryCtx<'tcx>, func: lume_mir::Function) -> Self {
        Self {
            mcx,
            func,
            metadata_registers: HashMap::new(),
        }
    }

    /// Gets the enclosed type-checking context.
    pub(crate) fn tcx(&self) -> &TyCheckCtx {
        self.mcx.tcx()
    }

    /// Gets the MIR function with the given ID.
    pub(crate) fn function(&self, func_id: NodeId) -> &Function {
        self.mcx.mir().function(func_id)
    }

    /// Creates a new block in the function.
    pub(crate) fn new_block(&mut self) -> BasicBlockId {
        self.func.new_block()
    }

    /// Creates a new block in the function and sets it as the current block.
    pub(crate) fn new_active_block(&mut self) -> BasicBlockId {
        self.func.new_active_block()
    }

    /// Executes the given closure with the given block as the current operating
    /// block.
    pub(crate) fn with_block<F, R>(&mut self, block: BasicBlockId, f: F) -> R
    where
        F: FnOnce(&mut Self, BasicBlockId) -> R,
    {
        self.func.set_current_block(block);
        f(self, block)
    }

    /// Executes the given closure with a newly-created block being the current
    /// operating block.
    pub(crate) fn with_new_block<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self, BasicBlockId) -> R,
    {
        let block = self.new_active_block();

        self.with_block(block, f)
    }

    /// Executes the given closure with the current block being the current
    /// operating block.
    pub(crate) fn with_current_block<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self, BasicBlockId) -> R,
    {
        let current_block = self.func.current_block().id;
        self.with_block(current_block, f)
    }

    /// Executes the given closure within a loop scope, setting up the loop
    /// header and merge blocks.
    pub(crate) fn in_loop_scope<F, R>(&mut self, body_block: BasicBlockId, merge_block: Option<BasicBlockId>, f: F) -> R
    where
        F: FnOnce(&mut Self, BasicBlockId) -> R,
    {
        if let Some(merge_block) = merge_block {
            self.func.enter_loop_scope(body_block, merge_block);
        }

        let ret = self.with_current_block(f);

        if merge_block.is_some() {
            self.func.exit_scope();
        }

        ret
    }
}

impl Builder<'_, '_> {
    /// Adds a variable declaration, which binds to the given register.
    pub(crate) fn declare_var(&mut self, block: BasicBlockId, variable: lume_tir::VariableId, register: RegisterId) {
        let variable = lume_mir::VariableId(variable.0);

        libftrace::debug!("[{}] declaring {variable} in {block}.{register}", self.func.name);

        self.func.variables.define(block, register, variable);
    }

    /// Gets the register which is bound to the given TIR variable.
    pub(crate) fn reference_var(&mut self, var: lume_tir::VariableId) -> RegisterId {
        let block = self.func.current_block().id;
        let var = lume_mir::VariableId(var.0);

        libftrace::debug!("[{}] referencing {var} in {block}", self.func.name);

        self.func.variables.reference(block, var)
    }

    /// Lowers the given expression into MIR and returns the ID of the register
    /// containing the resulting value.
    ///
    /// The expression is also bound to the register itself with
    /// [`Self::bind_value`].
    pub(crate) fn use_value(&mut self, expr: &lume_tir::Expression) -> (RegisterId, Operand) {
        let operand_ref = if let lume_tir::ExpressionKind::Variable(_) = &expr.kind {
            OperandRef::Explicit
        } else {
            OperandRef::Implicit
        };

        let value_operand = lower::expression(self, expr);
        let value_reference = self.declare_operand(value_operand.clone(), operand_ref);

        (value_reference, value_operand)
    }

    /// Reassigns the value of the register `reg` with the given value.
    ///
    /// Any following uses of the register will be implicitly replaced with a
    /// newly-created register, which will hold the new value.
    pub(crate) fn replace_register(&mut self, reg: RegisterId, new_value: Operand) -> RegisterId {
        let value_type = self.type_of_value(&new_value);
        let reassigned_id = self.declare_operand_as(value_type, new_value, OperandRef::Implicit);

        self.func.reassign_register(reg, reassigned_id);

        reassigned_id
    }

    /// References the given register as an operand.
    pub(crate) fn use_register(&self, id: RegisterId, location: Location) -> Operand {
        let id = self.func.moved_register(id);

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id },
            location,
        }
    }

    /// Stores the given value in a stack-allocated space.
    pub(crate) fn store_on_stack(&mut self, value: Operand, location: Location) -> Operand {
        let operand_type = self.type_of_value(&value);

        let slot = self.alloc_slot(operand_type, location);
        self.func.current_block_mut().store_slot(slot, 0, value, location);

        lume_mir::Operand {
            kind: lume_mir::OperandKind::SlotAddress { id: slot, offset: 0 },
            location,
        }
    }

    /// Stores the given value in a heap-allocated space.
    pub(crate) fn store_on_heap(&mut self, value: Operand, ty: &TypeRef, location: Location) -> Operand {
        let operand_type = self.type_of_value(&value);

        let alloc = self.alloca(operand_type, ty, location);
        self.func.current_block_mut().store(alloc, value, location);

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id: alloc },
            location,
        }
    }
}

impl Builder<'_, '_> {
    /// Adds an edge from the current block to the given block, adding each
    /// other as successor- and predecessor on both.
    fn add_edge_from_current(&mut self, to: BasicBlockId) {
        let current = self.func.current_block().id;

        self.func.block_mut(current).push_successor(to);
        self.func.block_mut(to).push_predecessor(current);
    }

    /// Creates an unconditional branch terminator on the current block,
    /// branching to the given block.
    ///
    /// If the current block already has a terminator, the existing terminator
    /// is left untouched.
    pub(crate) fn branch_to(&mut self, block: BasicBlockId, location: Location) {
        if !self.func.current_block().has_terminator() {
            self.add_edge_from_current(block);
        }

        self.func.current_block_mut().branch(block, location);
    }

    /// Creates an unconditional branch terminator on the current block,
    /// branching to the given block.
    ///
    /// If the current block already has a terminator, the existing terminator
    /// is left untouched.
    pub(crate) fn branch_with(&mut self, block: BasicBlockId, args: &[RegisterId], location: Location) {
        if !self.func.current_block().has_terminator() {
            self.add_edge_from_current(block);
        }

        self.func.current_block_mut().branch_with(block, args, location);
    }

    /// Creates a conditional branch terminator on the current block,
    /// branching to one of the two blocks, depending on the condition.
    ///
    /// If the current block already has a terminator, the existing terminator
    /// is left untouched.
    pub(crate) fn conditional_branch(
        &mut self,
        condition: RegisterId,
        then_block: BasicBlockId,
        else_block: BasicBlockId,
        location: Location,
    ) {
        if !self.func.current_block().has_terminator() {
            self.add_edge_from_current(then_block);
            self.add_edge_from_current(else_block);
        }

        self.func
            .current_block_mut()
            .conditional_branch(condition, then_block, else_block, location);
    }

    /// Creates a conditional branch terminator on the current block,
    /// branching to one of the two blocks, depending on the condition.
    ///
    /// If the current block already has a terminator, the existing terminator
    /// is left untouched.
    pub(crate) fn conditional_branch_with(
        &mut self,
        condition: RegisterId,
        then_block: BasicBlockId,
        then_block_args: &[RegisterId],
        else_block: BasicBlockId,
        else_block_args: &[RegisterId],
        location: Location,
    ) {
        if !self.func.current_block().has_terminator() {
            self.add_edge_from_current(then_block);
            self.add_edge_from_current(else_block);
        }

        self.func.current_block_mut().conditional_branch_with(
            condition,
            then_block,
            then_block_args,
            else_block,
            else_block_args,
            location,
        );
    }

    /// Creates a `switch` terminator on the current block, branching to the
    /// block within the matching arm.
    ///
    /// If the current block already has a terminator, the existing terminator
    /// is left untouched.
    pub(crate) fn switch(
        &mut self,
        operand: RegisterId,
        arms: Vec<(i64, BlockBranchSite)>,
        fallback: BlockBranchSite,
        location: Location,
    ) {
        if !self.func.current_block().has_terminator() {
            for (_, branch) in &arms {
                self.add_edge_from_current(branch.block);
            }

            self.add_edge_from_current(fallback.block);
        }

        self.func.current_block_mut().set_terminator(Terminator {
            kind: TerminatorKind::Switch {
                operand,
                arms,
                fallback,
            },
            location,
        });
    }

    /// Creates a return terminator on the current block, optionally returning
    /// the given operand, if any.
    ///
    /// If the current block already has a terminator, the existing terminator
    /// is left untouched.
    pub(crate) fn return_(&mut self, value: Option<Operand>, location: Location) {
        self.func.current_block_mut().return_any(value, location);
    }

    /// Sets the terminator of the current block to be unreachable.
    ///
    /// If the current block already has a terminator, the existing terminator
    /// is left untouched.
    pub(crate) fn unreachable(&mut self, location: Location) {
        self.func.current_block_mut().unreachable(location);
    }

    /// Creates a branch terminator on the current block, which branches to the
    /// end of the current loop scope.
    ///
    /// If the current block already has a terminator, the existing terminator
    /// is left untouched.
    pub(crate) fn break_out_of_loop(&mut self, location: Location) {
        let (_, end_block) = self.func.expect_loop_target();

        self.branch_to(end_block, location);
    }

    /// Creates a branch terminator on the current block, which branches back to
    /// the entry of the current loop scope.
    ///
    /// If the current block already has a terminator, the existing terminator
    /// is left untouched.
    pub(crate) fn continue_loop(&mut self, location: Location) {
        let (start_block, _) = self.func.expect_loop_target();

        self.branch_to(start_block, location);
    }

    /// Builds a conditional graph for the given expression, branching to the
    /// specified blocks based on the result of the expression.
    pub(crate) fn build_conditional_graph(
        &mut self,
        expr: &lume_tir::Expression,
        then_block: lume_mir::BasicBlockId,
        else_block: lume_mir::BasicBlockId,
    ) {
        if let lume_tir::ExpressionKind::Logical(comp_expr) = &expr.kind {
            match comp_expr.op {
                // Build graph for logical AND expressions
                //
                // For example, given an expression such as `x < 10 && x > 5`, the graph
                // would be visualized as:
                //
                //      BB0
                //     x < 10?
                //    /      \
                //  false    BB1
                //          x > 5?
                //         /     \
                //       false   true
                //
                // where `BB0` is the entry block and `BB1` is an intermediary block.
                lume_tir::LogicalOperator::And => {
                    let inter_block = self.func.new_block();

                    self.with_current_block(|builder, _| {
                        let lhs_val = lower::expression(builder, &comp_expr.lhs);
                        let lhs_expr = builder.declare_operand_as(Type::boolean(), lhs_val, OperandRef::Implicit);

                        builder.conditional_branch(lhs_expr, inter_block, else_block, expr.location());
                    });

                    self.with_block(inter_block, |builder, _| {
                        let rhs_val = lower::expression(builder, &comp_expr.rhs);
                        let rhs_expr = builder.declare_operand_as(Type::boolean(), rhs_val, OperandRef::Implicit);

                        builder.conditional_branch(rhs_expr, then_block, else_block, expr.location());
                    });
                }

                // Build graph for logical OR expressions
                //
                // For example, given an expression such as `x < 10 || y < 10`, the graph
                // would be visualized as:
                //
                //      BB0
                //     x < 10?
                //    /      \
                //  true     BB1
                //          y < 10?
                //         /      \
                //       true     false
                //
                // where `BB0` is the entry block and `BB1` is an intermediary block.
                lume_tir::LogicalOperator::Or => {
                    let inter_block = self.func.new_block();

                    self.with_current_block(|builder, _| {
                        let lhs_val = lower::expression(builder, &comp_expr.lhs);
                        let lhs_expr = builder.declare_operand_as(Type::boolean(), lhs_val, OperandRef::Implicit);

                        builder.conditional_branch(lhs_expr, then_block, inter_block, expr.location());
                    });

                    self.with_block(inter_block, |builder, _| {
                        let rhs_val = lower::expression(builder, &comp_expr.rhs);
                        let rhs_expr = builder.declare_operand_as(Type::boolean(), rhs_val, OperandRef::Implicit);

                        builder.conditional_branch(rhs_expr, then_block, else_block, expr.location());
                    });
                }
            }
        } else {
            self.with_current_block(|builder, _| {
                let cond_val = lower::expression(builder, expr);
                let cond_expr = builder.declare_operand_as(Type::boolean(), cond_val, OperandRef::Implicit);

                builder.conditional_branch(cond_expr, then_block, else_block, expr.location());
            });
        }
    }
}

impl Builder<'_, '_> {
    /// Gets the offset of the given HIR field.
    ///
    /// The returned tuple contains the byte offset of the field, as well as the
    /// MIR type of the field.
    pub(crate) fn field_offset(&self, field: &lume_hir::Field) -> (usize, Type) {
        let owner = self.tcx().owning_struct_of_field(field.id).unwrap();

        let mut offset = 0;

        for (idx, prop) in owner.fields.iter().enumerate() {
            let field_type = self.tcx().mk_type_ref_from(&prop.field_type, owner.id).unwrap();

            if idx == field.index {
                let mir_field_ty = self.lower_type(&field_type);
                return (offset, mir_field_ty);
            }

            offset += self.lower_type(&field_type).bytesize();
        }

        panic!("bug!: field index of {} is out of bounds", field.index)
    }

    /// Declares a new register with the metadata entry of the given type.
    pub(crate) fn declare_metadata_of(&mut self, type_ref: &lume_types::TypeRef, location: Location) -> RegisterId {
        let metadata_id = TypeMetadataId::from(type_ref);

        self.declare_metadata_with_id(metadata_id, location)
    }

    /// Declares a new register with the metadata entry of the given metadata
    /// ID.
    pub(crate) fn declare_metadata_with_id(&mut self, metadata_id: TypeMetadataId, location: Location) -> RegisterId {
        let current_block = self.func.current_block().id;

        if let Some(existing_register) = self.metadata_registers.get(&(current_block, metadata_id)) {
            return *existing_register;
        }

        let metadata_entry = self.mcx.mir().metadata.types.get(&metadata_id).unwrap();
        let metadata_type = Type {
            kind: TypeKind::Metadata {
                inner: Box::new(metadata_entry.to_owned()),
            },
            is_generic: false,
        };

        let metadata_register = self.declare_as(metadata_type, lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic {
                name: lume_mir::Intrinsic::Metadata {
                    metadata: Box::new(metadata_entry.to_owned()),
                },
                args: Vec::new(),
            }),
            location,
        });

        self.metadata_registers
            .insert((current_block, metadata_id), metadata_register);

        metadata_register
    }
}
