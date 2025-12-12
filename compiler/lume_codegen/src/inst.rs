use cranelift::prelude::*;

use super::LowerFunction;

pub const UNREACHABLE_TRAP_CODE: u8 = 0x99;

impl LowerFunction<'_> {
    #[libftrace::traced(level = Trace, fields(func = self.func.name))]
    pub(crate) fn cg_block_alloc_entry(&mut self, mir_block: &lume_mir::BasicBlock) {
        let cg_block = self.builder.create_block();
        self.builder.append_block_params_for_function_params(cg_block);

        self.blocks.insert(mir_block.id, cg_block);
        self.seal_block(mir_block.id);

        for (idx, param) in self.builder.block_params(cg_block).iter().enumerate() {
            self.parameters.insert(lume_mir::RegisterId::new(idx), *param);
        }
    }

    #[libftrace::traced(level = Trace, fields(func = self.func.name))]
    pub(crate) fn cg_block_alloc(&mut self, mir_block: &lume_mir::BasicBlock) {
        let cg_block = self.builder.create_block();

        for reg_id in &mir_block.parameters {
            let reg_ty = self.func.registers.register_ty(*reg_id);
            let param_ty = self.backend.cl_type_of(reg_ty);

            let param_value = self.builder.append_block_param(cg_block, param_ty);
            self.parameters.insert(*reg_id, param_value);
        }

        self.blocks.insert(mir_block.id, cg_block);
    }

    #[libftrace::traced(level = Trace, fields(func = self.func.name))]
    pub(crate) fn cg_block_in(&mut self, mir_block: &lume_mir::BasicBlock) {
        let cg_block = *self.blocks.get(&mir_block.id).unwrap();
        self.builder.switch_to_block(cg_block);

        for inst in mir_block.instructions() {
            self.cg_instruction(inst);
        }

        self.cg_terminator(mir_block.terminator().unwrap());
    }

    #[libftrace::traced(level = Trace, fields(func = self.func.name))]
    pub(crate) fn cg_instruction(&mut self, inst: &lume_mir::Instruction) {
        self.set_srcloc(inst.location.clone_inner());

        match &inst.kind {
            lume_mir::InstructionKind::Let { register, decl, ty } => {
                let var = self.declare_var(*register, ty.to_owned());

                let value = self.cg_declaration(decl);
                libftrace::debug!("define_var {register}({var}) = {value} ({decl})");

                self.builder.def_var(var, value);
            }
            lume_mir::InstructionKind::CreateSlot { slot, ty } => {
                #[expect(clippy::cast_possible_truncation)]
                let size = ty.bytesize() as u32;

                let stack_slot = self.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size,
                    align_shift: 4,
                });

                self.slots.insert(*slot, stack_slot);
            }
            lume_mir::InstructionKind::Allocate { register, ty, metadata } => {
                let ptr_ty = lume_mir::Type::pointer(ty.clone());
                let var = self.declare_var(*register, ptr_ty);
                let ptr = self.cg_alloc_type(ty, *metadata);

                self.builder.def_var(var, ptr);
            }
            lume_mir::InstructionKind::Store { target, value } => {
                let target = self.use_var(*target);
                let value = self.cg_operand(value);

                self.builder.ins().store(MemFlags::new(), value, target, 0);
            }
            lume_mir::InstructionKind::StoreSlot { target, value, offset } => {
                let slot = self.retrieve_slot(*target);
                let value = self.cg_operand(value);

                #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
                let offset = *offset as i32;

                self.builder.ins().stack_store(value, slot, offset);
            }
            lume_mir::InstructionKind::StoreField { target, offset, value } => {
                let target = self.use_var(*target);
                let value = self.cg_operand(value);

                #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
                let offset = *offset as i32;

                self.builder.ins().store(MemFlags::new(), value, target, offset);
            }
            lume_mir::InstructionKind::ObjectRegister { register } => {
                if let Some(param) = self.parameters.get(register) {
                    self.builder.declare_value_needs_stack_map(*param);
                } else {
                    let register = self.retrieve_var(*register);

                    self.builder.declare_var_needs_stack_map(register);
                }
            }
        }
    }

    #[libftrace::traced(level = Trace, fields(func = self.func.name))]
    pub(crate) fn cg_terminator(&mut self, term: &lume_mir::Terminator) {
        self.set_srcloc(term.location.clone_inner());

        match &term.kind {
            lume_mir::TerminatorKind::Return(operand) => {
                self.insert_gc_trigger();

                if let Some(value) = operand
                    && !self.func.signature.return_type.is_void()
                {
                    let value = self.cg_operand(value);

                    self.builder.ins().return_(&[value]);
                } else {
                    self.builder.ins().return_(&[]);
                }
            }
            lume_mir::TerminatorKind::Branch(call) => {
                self.branch(call);
            }
            lume_mir::TerminatorKind::ConditionalBranch {
                condition,
                then_block,
                else_block,
            } => {
                let condition = self.use_var(*condition);

                self.conditional_branch(condition, then_block, else_block);
            }
            lume_mir::TerminatorKind::Switch {
                operand,
                arms,
                fallback,
            } => {
                let operand = self.use_var(*operand);

                self.switch(operand, arms, fallback);
            }
            lume_mir::TerminatorKind::Unreachable => {
                let code = TrapCode::user(UNREACHABLE_TRAP_CODE).unwrap();

                self.builder.ins().trap(code);
            }
        }
    }

    #[libftrace::traced(level = Trace, fields(func = self.func.name))]
    pub(crate) fn cg_alloc_type(&mut self, ty: &lume_mir::Type, metadata: lume_mir::RegisterId) -> Value {
        match &ty.kind {
            lume_mir::TypeKind::Struct { .. } | lume_mir::TypeKind::Union { .. } => {
                let size = ty.bytesize();

                self.alloca(size, Some(metadata))
            }
            lume_mir::TypeKind::Tuple { .. }
            | lume_mir::TypeKind::Integer { .. }
            | lume_mir::TypeKind::Float { .. }
            | lume_mir::TypeKind::Boolean
            | lume_mir::TypeKind::String
            | lume_mir::TypeKind::Pointer { .. } => self.alloca(ty.bytesize(), None),
            lume_mir::TypeKind::Metadata { .. } | lume_mir::TypeKind::Void | lume_mir::TypeKind::Never => {
                unreachable!()
            }
        }
    }
}
