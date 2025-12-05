pub(crate) mod dynamic;
pub(crate) mod expr;
pub(crate) mod pass;
pub(crate) mod stmt;
pub(crate) mod ty;

use std::collections::HashMap;

use lume_mir::{Function, ModuleMap, RegisterId};
use lume_mir_queries::MirQueryCtx;
use lume_session::{Options, Package};
use lume_span::source::Location;
use lume_span::{Internable, NodeId};
use lume_type_metadata::{StaticMetadata, TypeMetadata, TypeMetadataId};
use lume_typech::TyCheckCtx;

use crate::dynamic::DynamicShimBuilder;
use crate::ty::lower_type;

/// Defines a transformer which will lower a typed HIR map into an MIR map.
pub struct ModuleTransformer<'tcx> {
    /// Defines the MIR map which is being created.
    mcx: MirQueryCtx<'tcx>,
}

impl<'tcx> ModuleTransformer<'tcx> {
    pub fn create(package: Package, tcx: &'tcx TyCheckCtx, metadata: StaticMetadata, options: Options) -> Self {
        Self {
            mcx: MirQueryCtx::new(tcx, ModuleMap::new(package, options, metadata)),
        }
    }

    /// Transforms the supplied context into a MIR map.
    pub fn transform(&mut self, functions: &[lume_tir::Function]) -> ModuleMap {
        // Declare all the TIR functions into the MIR before lowering any of
        // them.
        //
        // This ensures that any functions called *before* they are defined
        // within the source file still resolve correctly.
        for func in functions {
            let signature = self.signature_of(func);

            let mangle_version = lume_mangle::Version::default();
            let mangled_name = lume_mangle::mangled(self.mcx.tcx(), func.id, mangle_version).unwrap();

            let mut func = Function::new(
                func.id,
                func.name_as_str().intern(),
                mangled_name,
                func.location.clone_inner(),
            );
            func.signature = signature;

            // Offset the register counter by the number of parameters.
            for parameter in &func.signature.parameters {
                func.registers.allocate_param(parameter.ty.clone());
            }

            self.mcx.mir_mut().functions.insert(func.id, func);
        }

        for func in functions {
            let func_decl = self.mcx.mir().functions.get(&func.id).unwrap().clone();
            let func_transformer = FunctionTransformer::new_from(&mut self.mcx, func_decl);

            let defined_func = func_transformer.lower_function(func);
            self.mcx.mir_mut().functions.insert(func.id, defined_func);
        }

        self.mcx.take_mir()
    }

    /// Creates a MIR signature from the given function.
    fn signature_of(&self, func: &lume_tir::Function) -> lume_mir::Signature {
        let mut signature = lume_mir::Signature::default();

        for param in &func.parameters {
            let param_ty = lower_type(&self.mcx, &param.ty);

            signature.parameters.push(lume_mir::Parameter {
                name: param.name,
                ty: param_ty,
                type_ref: param.ty.clone(),
                location: param.location.clone_inner(),
            });

            if param.vararg {
                signature.vararg = true;
            }
        }

        // Limit symbol visibility according to the visiblity of the function.
        //
        // Unless it's the entrypoint - that should *always* be visible outside the
        // object.
        if !self.mcx.tcx().is_visible_outside_package(func.id) && !self.mcx.tcx().is_entrypoint(func.id) {
            signature.internal = true;
        }

        if matches!(
            func.kind,
            lume_tir::FunctionKind::Static | lume_tir::FunctionKind::Dropper
        ) && func.block.is_none()
        {
            signature.external = true;
        }

        if func.kind == lume_tir::FunctionKind::Dropper {
            signature.is_dropper = true;
        }

        signature.return_type = lower_type(&self.mcx, &func.return_type);

        signature
    }
}

pub(crate) struct FunctionTransformer<'mir, 'tcx> {
    mcx: &'mir mut MirQueryCtx<'tcx>,

    /// Defines the MIR function which is being created.
    func: Function,

    variables: HashMap<lume_tir::VariableId, RegisterId>,
}

impl<'mir, 'tcx> FunctionTransformer<'mir, 'tcx> {
    pub(crate) fn new_from(mcx: &'mir mut MirQueryCtx<'tcx>, func: lume_mir::Function) -> Self {
        Self {
            mcx,
            func,
            variables: HashMap::new(),
        }
    }

    /// Lowers the given TIR function into a single MIR function.
    pub fn lower_function(mut self, func: &lume_tir::Function) -> Function {
        match func.kind {
            lume_tir::FunctionKind::Static | lume_tir::FunctionKind::Dropper => {
                if let Some(body) = &func.block {
                    self.lower_block(body);
                    self.run_passes();
                }
            }
            lume_tir::FunctionKind::Dynamic => {
                DynamicShimBuilder::new(&mut self, func.id).build();

                self.run_pass::<pass::rename_ssa::RenameSsaVariables>();
            }
            lume_tir::FunctionKind::Unreachable => {
                let _entry_block = self.func.new_active_block();
                self.func.current_block_mut().unreachable(func.location.clone_inner());
            }
        }

        self.func
    }

    fn lower_block(&mut self, block: &lume_tir::Block) {
        let _entry_block = self.func.new_active_block();
        let return_value = self.block(block);

        // Only declare the return value, if the block actually expects a
        // non-void return type.
        if block.has_return_value()
            && let Some(return_value) = return_value
        {
            let location = return_value.location.clone();

            self.func.current_block_mut().return_value(return_value, location);
        }

        // If the current block is not returning, add a return statement so
        // there's always a valid return value.
        //
        // We're assuming it'll be a void return, since the type checker should
        // have detected a missing return statement in a non-void function.
        let ret_loc = block
            .statements
            .last()
            .map_or(Location::empty(), |stmt| stmt.location().clone_inner());

        self.func.current_block_mut().return_void(ret_loc);
    }

    pub(crate) fn tcx(&self) -> &TyCheckCtx {
        self.mcx.tcx()
    }

    pub(crate) fn function(&self, func_id: NodeId) -> &Function {
        self.mcx.mir().function(func_id)
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
            location: value.location.clone(),
            kind: Box::new(lume_mir::DeclarationKind::Operand(value)),
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
        let func_decl = self.mcx.mir().function(func_id);
        let func_name = func_decl.name;
        let func_sig = func_decl.signature.clone();

        let args = self.normalize_call_argumets(&func_sig.parameters, &args, func_sig.vararg);

        #[cfg(debug_assertions)]
        {
            if func_sig.vararg {
                debug_assert!(func_sig.parameters.len() - 1 <= args.len());
            } else {
                debug_assert_eq!(func_sig.parameters.len(), args.len());
            }
        }

        let call_inst = self.func.declare(ret_ty, lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Call {
                func_id,
                name: func_name,
                args,
            }),
            location: location.clone(),
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
        let args = self.normalize_call_argumets(&signature.parameters, &args, signature.vararg);

        let call_inst = self.func.declare(ret_ty, lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::IndirectCall { ptr, signature, args }),
            location: location.clone(),
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
        vararg: bool,
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
            let slot = self.func.alloc_slot(arg_ty, arg.location.clone());
            self.func
                .current_block_mut()
                .store_slot(slot, 0, arg.clone(), arg.location.clone());

            *arg = lume_mir::Operand {
                kind: lume_mir::OperandKind::SlotAddress { id: slot, offset: 0 },
                location: arg.location.clone(),
            };
        }

        if vararg && args.len() >= params.len() - 1 {
            return self.merge_vararg_operands(params, args);
        }

        args
    }

    fn merge_vararg_operands(
        &mut self,
        params: &[lume_mir::Parameter],
        mut args: Vec<lume_mir::Operand>,
    ) -> Vec<lume_mir::Operand> {
        let last_arg = args.last().map(|a| a.location.clone());

        let mut new_args = args.drain(..params.len() - 1).collect::<Vec<_>>();
        let vararg_type = &params.last().unwrap().type_ref;

        let vararg_loc = last_arg.unwrap_or_else(|| vararg_type.location.clone_inner());
        let metadata_reg = self.declare_metadata_of(vararg_type, vararg_loc.clone());

        let array_alloc_func_id = self.tcx().lang_item("array_with_capacity").unwrap();
        let array_alloc_func = self.mcx.function(array_alloc_func_id).unwrap();

        let array_push_func_id = self.tcx().lang_item("array_push").unwrap();

        let vararg_arr_reg = self
            .func
            .declare(array_alloc_func.signature.return_type.clone(), lume_mir::Declaration {
                kind: Box::new(lume_mir::DeclarationKind::Call {
                    func_id: array_alloc_func.id,
                    name: array_alloc_func.name,
                    args: vec![
                        lume_mir::Operand::integer(64, false, args.len().cast_signed() as i64),
                        lume_mir::Operand::reference_of(metadata_reg),
                    ],
                }),
                location: vararg_loc.clone(),
            });

        for arg in args {
            self.call(
                array_push_func_id,
                vec![
                    lume_mir::Operand::reference_of(vararg_arr_reg),
                    arg,
                    lume_mir::Operand::reference_of(metadata_reg),
                ],
                lume_mir::Type::void(),
                vararg_loc.clone(),
            );
        }

        new_args.push(lume_mir::Operand::reference_of(vararg_arr_reg));
        new_args
    }

    /// Gets the metadata entry of the given type.
    fn metadata_entry_of(&self, type_ref: &lume_types::TypeRef) -> &TypeMetadata {
        let metadata_id = TypeMetadataId::from(type_ref);

        self.mcx.mir().metadata.metadata.get(&metadata_id).unwrap()
    }

    /// Gets the metadata MIR type of the given type.
    fn metadata_type_of(&self, type_ref: &lume_types::TypeRef) -> lume_mir::Type {
        let metadata_entry = self.metadata_entry_of(type_ref);

        lume_mir::Type {
            kind: lume_mir::TypeKind::Metadata {
                inner: Box::new(metadata_entry.to_owned()),
            },
            is_generic: false,
        }
    }

    /// Declares a new register with the metadata entry of the given type.
    fn declare_metadata_of(&mut self, type_ref: &lume_types::TypeRef, location: Location) -> RegisterId {
        let metadata_entry = self.metadata_entry_of(type_ref);
        let metadata_type = self.metadata_type_of(type_ref);

        self.func.declare(metadata_type, lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic {
                name: lume_mir::Intrinsic::Metadata {
                    metadata: Box::new(metadata_entry.to_owned()),
                },
                args: Vec::new(),
            }),
            location,
        })
    }
}
