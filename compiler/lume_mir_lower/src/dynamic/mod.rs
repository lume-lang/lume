use lume_mir::{BasicBlockId, RegisterId, Signature};
use lume_span::{Location, NodeId};

use crate::FunctionTransformer;

pub(crate) struct DynamicShimBuilder<'shim, 'mir, 'tcx> {
    builder: &'shim mut FunctionTransformer<'mir, 'tcx>,
    signature: Signature,
    param_registers: Vec<RegisterId>,
    func_id: NodeId,
}

impl<'shim, 'mir, 'tcx> DynamicShimBuilder<'shim, 'mir, 'tcx> {
    pub(crate) fn new(builder: &'shim mut FunctionTransformer<'mir, 'tcx>, func_id: NodeId) -> Self {
        builder.func.name = format!("$_dyn_{}", builder.func.name);

        let regs = &builder.func.registers;
        let mut params = regs.iter_params().map(|reg| reg.id).collect::<Vec<_>>();
        params.pop();

        let mut signature = builder.func.signature.clone();
        signature.parameters.pop();

        Self {
            builder,
            signature,
            param_registers: params,
            func_id,
        }
    }

    /// Creates the shim function around the function with the given ID and
    /// returns the ID if the created shim.
    pub(crate) fn build(mut self) -> NodeId {
        let entry_block = self.builder.func.new_block();
        let loop_header = self.builder.func.new_block();
        let loop_body = self.builder.func.new_block();
        let loop_found = self.builder.func.new_block();
        let loop_continue = self.builder.func.new_block();
        let loop_exit = self.builder.func.new_block();

        self.builder.func.set_current_block(entry_block);
        let (len_reg, ptr_reg, idx_reg) = self.build_loop_entry(loop_header);

        self.builder.func.set_current_block(loop_header);
        self.build_loop_header(len_reg, idx_reg, ptr_reg, loop_body, loop_exit);

        self.builder.func.set_current_block(loop_body);
        let method_ptr = self.build_loop_body(len_reg, idx_reg, ptr_reg, loop_found, loop_continue);

        self.builder.func.set_current_block(loop_found);
        self.build_loop_found(method_ptr);

        self.builder.func.set_current_block(loop_continue);
        self.build_loop_continue(len_reg, idx_reg, ptr_reg, loop_header);

        self.builder.func.set_current_block(loop_exit);
        self.builder.func.current_block_mut().unreachable(Location::empty());

        self.func_id
    }

    fn build_loop_entry(&mut self, loop_header: BasicBlockId) -> (RegisterId, RegisterId, RegisterId) {
        const TYPE_METADATA_METHODS_INDEX: usize = 5;
        const TYPE_METADATA_METHODS_OFFSET: usize = 40;

        let parameter_registers = self.builder.func.registers.iter_params().collect::<Vec<_>>();
        let type_metadata_reg = parameter_registers.last().unwrap().id;

        let methods_ptr_reg = self.load_field(
            type_metadata_reg,
            method_metadata_type(),
            TYPE_METADATA_METHODS_INDEX,
            TYPE_METADATA_METHODS_OFFSET,
        );

        let len_reg = self.load(methods_ptr_reg, lume_mir::Type::u64());

        let ptr_reg = self
            .builder
            .func
            .declare(method_metadata_type(), lume_mir::Declaration {
                kind: Box::new(lume_mir::DeclarationKind::Intrinsic {
                    name: lume_mir::Intrinsic::IntAdd {
                        bits: 64,
                        signed: false,
                    },
                    args: vec![
                        lume_mir::Operand {
                            kind: lume_mir::OperandKind::Reference { id: methods_ptr_reg },
                            location: Location::empty(),
                        },
                        lume_mir::Operand {
                            kind: lume_mir::OperandKind::Integer {
                                bits: 64,
                                signed: false,
                                value: 8,
                            },
                            location: Location::empty(),
                        },
                    ],
                }),
                location: Location::empty(),
            });

        let idx_reg = self
            .builder
            .func
            .declare_value(lume_mir::Type::u64(), lume_mir::Operand {
                kind: lume_mir::OperandKind::Integer {
                    bits: 64,
                    signed: false,
                    value: 0,
                },
                location: Location::empty(),
            });

        let branch_params = &[&[idx_reg, len_reg, ptr_reg][..], &self.param_registers[..]].concat();

        self.builder
            .func
            .block_mut(loop_header)
            .parameters
            .extend(branch_params);

        self.builder
            .func
            .current_block_mut()
            .branch_with(loop_header, branch_params, Location::empty());

        (len_reg, ptr_reg, idx_reg)
    }

    fn build_loop_header(
        &mut self,
        len_reg: RegisterId,
        idx_reg: RegisterId,
        ptr_reg: RegisterId,
        loop_body: BasicBlockId,
        loop_exit: BasicBlockId,
    ) {
        let loop_cmp = self.builder.declare(lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic {
                name: lume_mir::Intrinsic::IntLt {
                    bits: 64,
                    signed: false,
                },
                args: vec![
                    lume_mir::Operand {
                        kind: lume_mir::OperandKind::Reference { id: idx_reg },
                        location: Location::empty(),
                    },
                    lume_mir::Operand {
                        kind: lume_mir::OperandKind::Reference { id: len_reg },
                        location: Location::empty(),
                    },
                ],
            }),
            location: Location::empty(),
        });

        let loop_cmp_expr = self
            .builder
            .func
            .declare_value(lume_mir::Type::boolean(), lume_mir::Operand {
                kind: lume_mir::OperandKind::Reference { id: loop_cmp },
                location: Location::empty(),
            });

        let branch_params = &[&[idx_reg, len_reg, ptr_reg][..], &self.param_registers[..]].concat();

        self.builder.func.block_mut(loop_body).parameters.extend(branch_params);

        self.builder.func.current_block_mut().conditional_branch_with(
            loop_cmp_expr,
            loop_body,
            branch_params,
            loop_exit,
            &[],
            Location::empty(),
        );
    }

    fn build_loop_body(
        &mut self,
        len_reg: RegisterId,
        idx_reg: RegisterId,
        ptr_reg: RegisterId,
        loop_found: BasicBlockId,
        loop_continue: BasicBlockId,
    ) -> RegisterId {
        let offset_reg = self.builder.declare(lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic {
                name: lume_mir::Intrinsic::IntMul {
                    bits: 64,
                    signed: false,
                },
                args: vec![
                    lume_mir::Operand {
                        kind: lume_mir::OperandKind::Reference { id: idx_reg },
                        location: Location::empty(),
                    },
                    lume_mir::Operand {
                        kind: lume_mir::OperandKind::Integer {
                            bits: 64,
                            signed: false,
                            value: 8,
                        },
                        location: Location::empty(),
                    },
                ],
            }),
            location: Location::empty(),
        });

        let method_ptr_reg = self
            .builder
            .func
            .declare(method_metadata_type(), lume_mir::Declaration {
                kind: Box::new(lume_mir::DeclarationKind::Intrinsic {
                    name: lume_mir::Intrinsic::IntAdd {
                        bits: 64,
                        signed: false,
                    },
                    args: vec![
                        lume_mir::Operand {
                            kind: lume_mir::OperandKind::Reference { id: ptr_reg },
                            location: Location::empty(),
                        },
                        lume_mir::Operand {
                            kind: lume_mir::OperandKind::Reference { id: offset_reg },
                            location: Location::empty(),
                        },
                    ],
                }),
                location: Location::empty(),
            });

        let method_id_reg = self.load_field(method_ptr_reg, lume_mir::Type::pointer(lume_mir::Type::u64()), 0, 0);

        let id_cmp = self.builder.declare(lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic {
                name: lume_mir::Intrinsic::IntEq {
                    bits: 64,
                    signed: false,
                },
                args: vec![
                    lume_mir::Operand {
                        kind: lume_mir::OperandKind::Load { id: method_id_reg },
                        location: Location::empty(),
                    },
                    lume_mir::Operand {
                        kind: lume_mir::OperandKind::Integer {
                            bits: 64,
                            signed: false,
                            value: self.func_id.as_usize().cast_signed() as i64,
                        },
                        location: Location::empty(),
                    },
                ],
            }),
            location: Location::empty(),
        });

        let id_cmp_expr = self
            .builder
            .func
            .declare_value(lume_mir::Type::boolean(), lume_mir::Operand {
                kind: lume_mir::OperandKind::Reference { id: id_cmp },
                location: Location::empty(),
            });

        let loop_found_params = &[&[method_ptr_reg][..], &self.param_registers[..]].concat();
        let loop_continue_params = &[&[idx_reg, len_reg, ptr_reg][..], &self.param_registers[..]].concat();

        self.builder
            .func
            .block_mut(loop_found)
            .parameters
            .extend(loop_found_params);

        self.builder
            .func
            .block_mut(loop_continue)
            .parameters
            .extend(loop_continue_params);

        self.builder.func.current_block_mut().conditional_branch_with(
            id_cmp_expr,
            loop_found,
            loop_found_params,
            loop_continue,
            loop_continue_params,
            Location::empty(),
        );

        method_ptr_reg
    }

    fn build_loop_found(&mut self, method_ptr: RegisterId) {
        const METHOD_METADATA_FUNC_INDEX: usize = 5;
        const METHOD_METADATA_FUNC_OFFSET: usize = 40;

        let method = self.load_raw(method_ptr, method_metadata_type());
        let func_ptr = self.load_field(
            method,
            lume_mir::Type::pointer(lume_mir::Type::void()),
            METHOD_METADATA_FUNC_INDEX,
            METHOD_METADATA_FUNC_OFFSET,
        );

        let args = self
            .param_registers
            .iter()
            .map(|id| lume_mir::Operand {
                kind: lume_mir::OperandKind::Reference { id: *id },
                location: Location::empty(),
            })
            .collect::<Vec<_>>();

        let ret_ty = self.builder.type_of_function(self.func_id);
        let ret = self
            .builder
            .call_indirect(func_ptr, self.signature.clone(), args, ret_ty, Location::empty());

        self.builder
            .func
            .current_block_mut()
            .return_value(ret, Location::empty());
    }

    fn build_loop_continue(
        &mut self,
        len_reg: RegisterId,
        idx_reg: RegisterId,
        ptr_reg: RegisterId,
        loop_header: BasicBlockId,
    ) {
        let next_idx_reg = self.builder.declare(lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic {
                name: lume_mir::Intrinsic::IntAdd {
                    bits: 64,
                    signed: false,
                },
                args: vec![
                    lume_mir::Operand {
                        kind: lume_mir::OperandKind::Reference { id: idx_reg },
                        location: Location::empty(),
                    },
                    lume_mir::Operand {
                        kind: lume_mir::OperandKind::Integer {
                            bits: 64,
                            signed: false,
                            value: 1,
                        },
                        location: Location::empty(),
                    },
                ],
            }),
            location: Location::empty(),
        });

        self.builder.func.current_block_mut().branch_with(
            loop_header,
            &[&[next_idx_reg, len_reg, ptr_reg][..], &self.param_registers[..]].concat(),
            Location::empty(),
        );
    }

    /// Loads the address from a register into a new register and returns it.
    pub fn load(&mut self, source: RegisterId, ty: lume_mir::Type) -> RegisterId {
        self.builder.func.declare_value(ty, lume_mir::Operand {
            kind: lume_mir::OperandKind::Load { id: source },
            location: Location::empty(),
        })
    }

    /// Loads the address from a register into a new register and returns it.
    pub fn load_raw(&mut self, source: RegisterId, ty: lume_mir::Type) -> RegisterId {
        self.builder.func.declare_value_raw(ty, lume_mir::Operand {
            kind: lume_mir::OperandKind::Load { id: source },
            location: Location::empty(),
        })
    }

    /// Loads a field into a new register and returns it.
    pub fn load_field(&mut self, target: RegisterId, ty: lume_mir::Type, index: usize, offset: usize) -> RegisterId {
        self.builder.func.declare_value(ty.clone(), lume_mir::Operand {
            kind: lume_mir::OperandKind::LoadField {
                target,
                index,
                offset,
                field_type: ty,
            },
            location: Location::empty(),
        })
    }
}

fn method_metadata_type() -> lume_mir::Type {
    lume_mir::Type::pointer(lume_mir::Type::structure(String::from("std::Method"), vec![
        lume_mir::Type::u64(),                           // id,
        lume_mir::Type::pointer(lume_mir::Type::void()), // full_name,
        lume_mir::Type::pointer(lume_mir::Type::void()), // parameters,
        lume_mir::Type::pointer(lume_mir::Type::void()), // type_parameters,
        lume_mir::Type::void(),                          // return_type
        lume_mir::Type::void(),                          // func_ptr
    ]))
}
