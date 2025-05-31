use error_snippet::Result;
use lume_hir::{self, SymbolName, TypeId, TypeParameterId};
use lume_types::*;

use crate::ThirBuildCtx;

pub(super) struct DefineTypeParameters<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineTypeParameters<'_> {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx) -> Result<()> {
        let mut hir = std::mem::take(&mut ctx.hir);
        let mut define = DefineTypeParameters { ctx };

        for (_, symbol) in &mut hir.items {
            match symbol {
                lume_hir::Symbol::Type(t) => define.define_type(t)?,
                lume_hir::Symbol::Impl(i) => define.define_impl(i)?,
                lume_hir::Symbol::Function(f) => define.define_function(f)?,
                lume_hir::Symbol::Use(_) => (),
            }
        }

        ctx.hir = hir;

        Ok(())
    }

    fn define_type(&mut self, ty: &mut lume_hir::TypeDefinition) -> Result<()> {
        match ty {
            lume_hir::TypeDefinition::Struct(struct_def) => {
                let type_id = struct_def.type_id.unwrap();

                for type_param in &mut struct_def.type_parameters {
                    let type_param_id = self.ctx.tcx_mut().type_param_alloc(type_param.name.name.clone());

                    type_param.type_param_id = Some(type_param_id);
                    type_param.type_id = Some(self.wrap_type_param(type_param_id));

                    self.ctx.tcx_mut().push_type_param(type_id, type_param_id)?;
                }

                for method in &mut struct_def.methods_mut() {
                    let method_id = method.method_id.unwrap();

                    for type_param in &mut method.type_parameters {
                        let type_param_id = self.ctx.tcx_mut().type_param_alloc(type_param.name.name.clone());

                        type_param.type_param_id = Some(type_param_id);
                        type_param.type_id = Some(self.wrap_type_param(type_param_id));

                        self.ctx.tcx_mut().push_type_param(method_id, type_param_id)?;
                    }
                }
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let type_id = trait_def.type_id.unwrap();

                for type_param in &mut trait_def.type_parameters {
                    let type_param_id = self.ctx.tcx_mut().type_param_alloc(type_param.name.name.clone());

                    type_param.type_param_id = Some(type_param_id);
                    type_param.type_id = Some(self.wrap_type_param(type_param_id));

                    self.ctx.tcx_mut().push_type_param(type_id, type_param_id)?;
                }

                for method in &mut trait_def.methods {
                    let method_id = method.method_id.unwrap();

                    for type_param in &mut method.type_parameters {
                        let type_param_id = self.ctx.tcx_mut().type_param_alloc(type_param.name.name.clone());

                        type_param.type_param_id = Some(type_param_id);
                        type_param.type_id = Some(self.wrap_type_param(type_param_id));

                        self.ctx.tcx_mut().push_type_param(method_id, type_param_id)?;
                    }
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn define_impl(&mut self, implementation: &mut lume_hir::Implementation) -> Result<()> {
        let impl_id = implementation.impl_id.unwrap();

        for type_param in &mut implementation.type_parameters {
            let type_param_id = self.ctx.tcx_mut().type_param_alloc(type_param.name.name.clone());

            type_param.type_param_id = Some(type_param_id);
            type_param.type_id = Some(self.wrap_type_param(type_param_id));

            self.ctx.tcx_mut().push_type_param(impl_id, type_param_id)?;
        }

        Ok(())
    }

    fn define_function(&mut self, func: &mut lume_hir::FunctionDefinition) -> Result<()> {
        let func_id = func.func_id.unwrap();

        for type_param in &mut func.type_parameters {
            let type_param_id = self.ctx.tcx_mut().type_param_alloc(type_param.name.name.clone());

            type_param.type_param_id = Some(type_param_id);
            type_param.type_id = Some(self.wrap_type_param(type_param_id));

            self.ctx.tcx_mut().push_type_param(func_id, type_param_id)?;
        }

        Ok(())
    }

    fn wrap_type_param(&mut self, type_param_id: TypeParameterId) -> TypeId {
        let name = self.ctx.tcx().type_parameter(type_param_id).unwrap().name.clone();
        let symbol_name = SymbolName {
            name: lume_hir::PathSegment::from(name),
            namespace: None,
            location: lume_span::Location::empty(),
        };

        self.ctx
            .tcx_mut()
            .type_alloc(symbol_name, TypeKindRef::TypeParameter(type_param_id))
    }
}
