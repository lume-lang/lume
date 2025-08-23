use cranelift::{codegen::ir::immediates::Offset32, prelude::*};

use super::LowerFunction;

pub const UNREACHABLE_TRAP_CODE: u8 = 0x99;

impl LowerFunction<'_> {
    #[tracing::instrument(level = "TRACE", skip_all, fields(func = %self.func.name))]
    pub(crate) fn cg_block_alloc_entry(&mut self, mir_block: &lume_mir::BasicBlock) {
        let cg_block = self.builder.create_block();
        self.builder.append_block_params_for_function_params(cg_block);

        self.blocks.insert(mir_block.id, cg_block);
        self.seal_block(mir_block.id);

        for (idx, param) in self.builder.block_params(cg_block).iter().enumerate() {
            self.parameters.insert(lume_mir::RegisterId::new(idx), *param);
        }
    }

    #[tracing::instrument(level = "TRACE", skip_all, fields(func = %self.func.name))]
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

    #[tracing::instrument(level = "TRACE", skip_all, fields(func = %self.func.name))]
    pub(crate) fn cg_block_in(&mut self, mir_block: &lume_mir::BasicBlock) {
        let cg_block = *self.blocks.get(&mir_block.id).unwrap();
        self.builder.switch_to_block(cg_block);

        for inst in mir_block.instructions() {
            self.cg_instruction(inst);
        }

        self.cg_terminator(mir_block.terminator().unwrap());
    }

    #[tracing::instrument(level = "TRACE", skip_all, fields(func = %self.func.name))]
    pub(crate) fn cg_instruction(&mut self, inst: &lume_mir::Instruction) {
        match inst {
            lume_mir::Instruction::Let { register, decl, ty } => {
                let var = self.declare_var(*register, ty.to_owned());

                let value = self.cg_declaration(decl);
                tracing::debug!("define_var {register} = {value}");

                self.builder.def_var(var, value);
            }
            lume_mir::Instruction::Assign { .. } => {
                panic!("bug!: assignment instructions should not be present in codegen")
            }
            lume_mir::Instruction::CreateSlot { slot, ty } => {
                #[expect(clippy::cast_possible_truncation)]
                let size = ty.bytesize() as u32;

                let stack_slot = self.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size,
                    align_shift: 4,
                });

                self.slots.insert(*slot, stack_slot);
            }
            lume_mir::Instruction::Allocate { register, ty } => {
                let ptr_ty = lume_mir::Type::pointer(ty.clone());
                let var = self.declare_var(*register, ptr_ty);
                let ptr = self.cg_alloc_type(ty);

                self.builder.def_var(var, ptr);
            }
            lume_mir::Instruction::Store { target, value } => {
                let target = self.use_var(*target);
                let value = self.cg_operand(value);

                self.builder
                    .ins()
                    .store(MemFlags::new(), value, target, Offset32::new(0));
            }
            lume_mir::Instruction::StoreSlot { target, value } => {
                let slot = self.retrieve_slot(*target);
                let value = self.cg_operand(value);

                self.builder.ins().stack_store(value, slot, 0);
            }
            lume_mir::Instruction::StoreField { target, offset, value } => {
                let target = self.use_var(*target);
                let value = self.cg_operand(value);

                #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
                let offset = *offset as i32;

                self.builder
                    .ins()
                    .store(MemFlags::new(), value, target, Offset32::new(offset));
            }
        }
    }

    #[tracing::instrument(level = "TRACE", skip_all, fields(func = %self.func.name))]
    pub(crate) fn cg_terminator(&mut self, term: &lume_mir::Terminator) {
        match term {
            lume_mir::Terminator::Return(operand) => {
                if let Some(value) = operand {
                    let value = self.cg_operand(value);

                    self.builder.ins().return_(&[value]);
                } else {
                    self.builder.ins().return_(&[]);
                }
            }
            lume_mir::Terminator::Branch(call) => {
                self.branch(call);
            }
            lume_mir::Terminator::ConditionalBranch {
                condition,
                then_block,
                else_block,
            } => {
                let condition = self.use_var(*condition);

                self.conditional_branch(condition, then_block, else_block);
            }
            lume_mir::Terminator::Unreachable => {
                let code = TrapCode::user(UNREACHABLE_TRAP_CODE).unwrap();

                self.builder.ins().trap(code);
            }
        }
    }

    #[tracing::instrument(level = "TRACE", skip_all, fields(func = %self.func.name))]
    pub(crate) fn cg_alloc_type(&mut self, ty: &lume_mir::Type) -> Value {
        match &ty.kind {
            lume_mir::TypeKind::Struct { .. } | lume_mir::TypeKind::Union { .. } => {
                let size = ty.bytesize();

                self.alloca(size)
            }
            lume_mir::TypeKind::Integer { .. }
            | lume_mir::TypeKind::Float { .. }
            | lume_mir::TypeKind::Boolean
            | lume_mir::TypeKind::String
            | lume_mir::TypeKind::Pointer { .. } => {
                let cl_type = self.backend.cl_type_of(ty);

                self.alloc(cl_type)
            }
            lume_mir::TypeKind::Metadata { .. } | lume_mir::TypeKind::Void => unreachable!(),
        }
    }
}
