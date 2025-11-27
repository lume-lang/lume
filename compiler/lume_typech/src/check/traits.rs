use error_snippet::Result;
use lume_infer::query::CallReference;

use crate::TyCheckCtx;

impl TyCheckCtx {
    /// Type checker pass to check whether implementations
    /// if any given trait is valid against the trait definition.
    #[libftrace::traced(level = Debug, err)]
    pub(crate) fn typech_traits(&mut self) -> Result<()> {
        for (_, item) in &self.hir().nodes {
            if let Err(err) = self.typech_item(item) {
                self.dcx().emit(err);
            }
        }

        Ok(())
    }

    fn typech_item(&self, symbol: &lume_hir::Node) -> Result<()> {
        if let lume_hir::Node::TraitImpl(trait_impl) = symbol {
            self.check_trait_impl(trait_impl)?;
        }

        Ok(())
    }

    fn check_trait_impl(&self, trait_impl: &lume_hir::TraitImplementation) -> Result<()> {
        let use_id = trait_impl.id;
        let trait_type = self.trait_def_of(use_id)?;
        let lume_hir::TypeDefinition::Trait(trait_def) = self.hir_expect_type(trait_type.id) else {
            panic!("bug!: expected HIR ID to reference trait item");
        };

        if trait_impl.type_args().len() != trait_def.type_parameters.len() {
            return Err(crate::check::errors::TraitImplTypeParameterCountMismatch {
                source: trait_impl.location,
                expected: trait_def.type_parameters.len(),
                found: trait_impl.type_args().len(),
            }
            .into());
        }

        for method_def in &trait_def.methods {
            let Some(method_impl) = trait_impl
                .methods
                .iter()
                .find(|method_impl| method_impl.name == method_def.name)
            else {
                // If a block is defined in the trait definition for the method,
                // we can default back to that when resolving.
                if method_def.block.is_some() {
                    continue;
                }

                // Otherwise, the trait impl is missing a method implementation.
                return Err(crate::check::errors::TraitImplMissingMethod {
                    source: trait_impl.location,
                    name: method_def.name.clone(),
                }
                .into());
            };

            self.check_trait_method(trait_def, trait_impl, method_def, method_impl)?;
        }

        for method_impl in &trait_impl.methods {
            if !trait_def
                .methods
                .iter()
                .any(|method_def| method_def.name == method_impl.name)
            {
                return Err(crate::check::errors::TraitImplExtraneousMethod {
                    source: trait_impl.location,
                    name: method_impl.name.clone(),
                }
                .into());
            }
        }

        Ok(())
    }

    fn check_trait_method<'a>(
        &self,
        trait_def: &'a lume_hir::TraitDefinition,
        trait_impl: &'a lume_hir::TraitImplementation,
        method_def: &'a lume_hir::TraitMethodDefinition,
        method_impl: &'a lume_hir::TraitMethodImplementation,
    ) -> Result<()> {
        let type_params = [&trait_def.type_parameters[..], &method_def.type_parameters[..]].concat();

        let type_args = self.mk_type_refs_from(trait_impl.type_args(), trait_impl.id)?;

        let def_sig = self.signature_of_call_ref(CallReference::Method(method_def.id))?;
        let mut inst_def_sig = self.instantiate_signature_isolate(def_sig.as_ref(), &type_params, &type_args);
        inst_def_sig.type_params = method_def.type_parameters.clone();

        let impl_sig = self.signature_of_call_ref(CallReference::Method(method_impl.id))?;
        let mut inst_impl_sig = self.instantiate_signature_isolate(impl_sig.as_ref(), &type_params, &type_args);
        inst_impl_sig.type_params = method_impl.type_parameters.clone();

        if !self.check_signature_compatibility(inst_def_sig.as_ref(), inst_impl_sig.as_ref())? {
            return Err(crate::check::errors::TraitMethodSignatureMismatch {
                source: method_impl.location,
                expected: self.sig_to_string(&method_def.name, inst_def_sig.as_ref(), false)?,
                found: self.sig_to_string(&method_impl.name, inst_impl_sig.as_ref(), false)?,
            }
            .into());
        }

        Ok(())
    }
}
