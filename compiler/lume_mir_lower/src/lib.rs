pub(crate) mod dynamic;
pub(crate) mod expr;
pub(crate) mod pass;
pub(crate) mod stmt;
pub(crate) mod ty;

use std::collections::HashMap;

use lume_mir::{Function, ModuleMap, RegisterId};
use lume_mir_queries::MirQueryCtx;
use lume_span::{Location, NodeId};
use lume_type_metadata::{TypeMetadata, TypeMetadataId};
use lume_typech::TyCheckCtx;

use crate::dynamic::DynamicShimBuilder;

/// Defines a transformer which will lower a typed HIR map into an MIR map.
pub struct ModuleTransformer<'tcx> {
    /// Defines the MIR map which is being created.
    mcx: MirQueryCtx<'tcx>,
}

impl<'tcx> ModuleTransformer<'tcx> {
    /// Transforms the supplied context into a MIR map.
    pub fn transform(tcx: &'tcx TyCheckCtx, tir: lume_tir::TypedIR) -> ModuleMap {
        let mut transformer = Self {
            mcx: MirQueryCtx::new(tcx, ModuleMap::new(tir.metadata)),
        };

        for func in tir.functions.values() {
            transformer.define_callable(func);
        }

        for func in tir.functions.values() {
            transformer.transform_callable(func);
        }

        transformer.mcx.take_mir()
    }

    fn define_callable(&mut self, func: &lume_tir::Function) {
        let id = func.id;
        let func = FunctionTransformer::define(self, id, func);

        self.mcx.mir_mut().functions.insert(id, func);
    }

    fn transform_callable(&mut self, func: &lume_tir::Function) {
        let id = func.id;
        let func = FunctionTransformer::transform(self, id, func);

        self.mcx.mir_mut().functions.insert(id, func);
    }
}

pub(crate) struct FunctionTransformer<'mir, 'tcx> {
    transformer: &'mir mut ModuleTransformer<'tcx>,

    /// Defines the MIR function which is being created.
    func: Function,

    variables: HashMap<lume_tir::VariableId, RegisterId>,
}

impl<'mir, 'tcx> FunctionTransformer<'mir, 'tcx> {
    /// Defines the MIR function which is being created.
    pub fn define(transformer: &'mir mut ModuleTransformer<'tcx>, id: NodeId, func: &lume_tir::Function) -> Function {
        let mut transformer = Self {
            transformer,
            func: Function::new(id, func.name_as_str(), func.location),
            variables: HashMap::new(),
        };

        transformer.lower_signature(func);

        transformer.func
    }

    /// Transforms the supplied context into a MIR map.
    pub fn transform(
        transformer: &'mir mut ModuleTransformer<'tcx>,
        id: NodeId,
        func: &lume_tir::Function,
    ) -> Function {
        let mut transformer = Self {
            transformer,
            func: Function::new(id, func.name_as_str(), func.location),
            variables: HashMap::new(),
        };

        transformer.lower_signature(func);

        match func.kind {
            lume_tir::FunctionKind::Static | lume_tir::FunctionKind::Dropper => {
                if let Some(body) = &func.block {
                    transformer.lower(body);
                    transformer.run_passes();
                } else {
                    transformer.func.signature.external = true;
                }
            }
            lume_tir::FunctionKind::Dynamic => {
                DynamicShimBuilder::new(&mut transformer, id).build();

                transformer.run_pass::<pass::rename_ssa::RenameSsaVariables>();
            }
            lume_tir::FunctionKind::Unreachable => {
                let _entry_block = transformer.func.new_active_block();
                transformer.func.current_block_mut().unreachable(func.location);
            }
        }

        transformer.func
    }

    fn lower_signature(&mut self, func: &lume_tir::Function) {
        for param in &func.parameters {
            let param_ty = self.lower_type(&param.ty);

            self.func.signature.parameters.push(lume_mir::Parameter {
                name: param.name,
                ty: param_ty.clone(),
                location: param.location,
            });

            // Offset the register counter by the number of parameters
            self.func.registers.allocate_param(param_ty);

            if param.vararg {
                self.func.signature.vararg = true;
            }
        }

        if func.kind == lume_tir::FunctionKind::Dropper {
            self.func.signature.is_dropper = true;
        }

        self.func.signature.return_type = self.lower_type(&func.return_type);
    }

    fn lower(&mut self, block: &lume_tir::Block) {
        let _entry_block = self.func.new_active_block();
        let return_value = self.block(block);

        // Only declare the return value, if the block actually expects a
        // non-void return type.
        if block.has_return_value()
            && let Some(return_value) = return_value
        {
            let location = return_value.location;

            self.func.current_block_mut().return_value(return_value, location);
        }

        // If the current block is not returning, add a return statement so
        // there's always a valid return value.
        //
        // We're assuming it'll be a void return, since the type checker should
        // have detected a missing return statement in a non-void function.
        self.func.current_block_mut().return_void(Location::empty());
    }

    pub(crate) fn tcx(&self) -> &TyCheckCtx {
        self.transformer.mcx.tcx()
    }

    pub(crate) fn function(&self, func_id: NodeId) -> &Function {
        self.transformer.mcx.mir().function(func_id)
    }

    /// Defines a new declaration in the current function block.
    fn declare(&mut self, decl: lume_mir::Declaration) -> RegisterId {
        let ty = self.type_of_decl(&decl);

        self.func.declare(ty, decl)
    }

    /// Defines a new register with a value of `null` and return an reference to
    /// it.
    fn null_operand(&mut self) -> lume_mir::Operand {
        let reg = self.declare_value(lume_mir::Operand {
            kind: lume_mir::OperandKind::Boolean { value: false },
            location: Location::empty(),
        });

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id: reg },
            location: Location::empty(),
        }
    }

    /// Defines a new operand declaration in the current function block.
    fn declare_value(&mut self, value: lume_mir::Operand) -> RegisterId {
        self.declare(lume_mir::Declaration {
            location: value.location,
            kind: lume_mir::DeclarationKind::Operand(value),
        })
    }

    /// Defines a new call instruction in the current function block.
    fn call(
        &mut self,
        func_id: NodeId,
        args: Vec<lume_mir::Operand>,
        ret_ty: lume_mir::Type,
        location: Location,
    ) -> lume_mir::Operand {
        let func_sig = self.transformer.mcx.mir().function(func_id).signature.clone();
        let args = self.normalize_call_argumets(&func_sig.parameters, &args);

        #[cfg(debug_assertions)]
        {
            if func_sig.vararg {
                debug_assert!(func_sig.parameters.len() - 1 <= args.len());
            } else {
                debug_assert!(func_sig.parameters.len() == args.len());
            }
        }

        let call_inst = self.func.declare(ret_ty, lume_mir::Declaration {
            kind: lume_mir::DeclarationKind::Call { func_id, args },
            location,
        });

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id: call_inst },
            location,
        }
    }

    /// Defines a new indirect call instruction in the current function block.
    fn call_indirect(
        &mut self,
        ptr: RegisterId,
        signature: lume_mir::Signature,
        args: Vec<lume_mir::Operand>,
        ret_ty: lume_mir::Type,
        location: Location,
    ) -> lume_mir::Operand {
        let args = self.normalize_call_argumets(&signature.parameters, &args);

        let call_inst = self.func.declare(ret_ty, lume_mir::Declaration {
            kind: lume_mir::DeclarationKind::IndirectCall { ptr, signature, args },
            location,
        });

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id: call_inst },
            location,
        }
    }

    fn normalize_call_argumets(
        &mut self,
        params: &[lume_mir::Parameter],
        args: &[lume_mir::Operand],
    ) -> Vec<lume_mir::Operand> {
        let mut args = args.to_vec();

        // Generic parameters are lowering into accepting pointer types, so all
        // types of argument can be passed.
        //
        // When passing a non-reference argument into a generic parameter, we then
        // need to pass an address to the argument, so the callee can load it. When
        // lowering these arguments, we create a slot in the stack to store the
        // argument, then we pass the address of the stack slot to the function.
        for (arg, param) in args.iter_mut().zip(params.iter()) {
            // If the parameter isn't generic, we let it be.
            if !param.ty.is_generic && !param.ty.kind.is_reference_type() {
                continue;
            }

            // If the passed type is already a reference type, we can pass it without
            // allocating room for it.
            if self.type_of_value(arg).kind.is_reference_type() {
                continue;
            }

            let arg_ty = self.type_of_value(arg);
            let slot = self.func.alloc_slot(arg_ty, arg.location);
            self.func
                .current_block_mut()
                .store_slot(slot, 0, arg.clone(), arg.location);

            *arg = lume_mir::Operand {
                kind: lume_mir::OperandKind::SlotAddress { id: slot, offset: 0 },
                location: arg.location,
            };
        }

        args
    }

    /// Gets the metadata entry of the given type.
    fn metadata_entry_of(&self, type_ref: &lume_types::TypeRef) -> &TypeMetadata {
        let metadata_id = TypeMetadataId::from(type_ref);

        self.transformer.mcx.mir().metadata.metadata.get(&metadata_id).unwrap()
    }

    /// Gets the metadata MIR type of the given type.
    fn metadata_type_of(&self, type_ref: &lume_types::TypeRef) -> lume_mir::Type {
        let metadata_entry = self.metadata_entry_of(type_ref);

        lume_mir::Type {
            kind: lume_mir::TypeKind::Metadata {
                inner: metadata_entry.to_owned(),
            },
            is_generic: false,
        }
    }

    /// Declares a new register with the metadata entry of the given type.
    fn declare_metadata_of(&mut self, type_ref: &lume_types::TypeRef, location: Location) -> RegisterId {
        let metadata_entry = self.metadata_entry_of(type_ref);
        let metadata_type = self.metadata_type_of(type_ref);

        self.func.declare(metadata_type, lume_mir::Declaration {
            kind: lume_mir::DeclarationKind::Intrinsic {
                name: lume_mir::Intrinsic::Metadata {
                    metadata: metadata_entry.to_owned(),
                },
                args: Vec::new(),
            },
            location,
        })
    }
}
