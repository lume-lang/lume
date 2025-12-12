pub(crate) mod builder;
pub(crate) mod dynamic;
pub(crate) mod pass;
pub(crate) mod ty;

use lume_mir::{Function, ModuleMap};
use lume_mir_queries::MirQueryCtx;
use lume_session::{Options, Package};
use lume_span::Internable;
use lume_type_metadata::StaticMetadata;
use lume_typech::TyCheckCtx;

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

            let mut func = Function::new(func.id, func.name_as_str().intern(), mangled_name, func.location);
            func.signature = signature;

            // Offset the register counter by the number of parameters.
            for parameter in &func.signature.parameters {
                func.registers.allocate_param(parameter.ty.clone());
            }

            self.mcx.mir_mut().functions.insert(func.id, func);
        }

        for func in functions {
            let func_decl = self.mcx.mir().functions.get(&func.id).unwrap().clone();
            let builder = builder::Builder::create_from(&self.mcx, func_decl);

            let defined_func = builder::lower::lower_function(builder, func);
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
                location: param.location,
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
