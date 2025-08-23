use error_snippet::Result;
use lume_infer::query::CallReference;
use lume_span::DefId;

use crate::TyCheckCtx;

impl TyCheckCtx {
    /// Type checker pass to check whether implementations
    /// if any given trait is valid against the trait definition.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(crate) fn typech_traits(&mut self) -> Result<()> {
        for (_, item) in &self.hir().items {
            if let Err(err) = self.typech_item(item) {
                self.dcx().emit(err);
            }
        }

        Ok(())
    }

    fn typech_item(&self, symbol: &lume_hir::Item) -> Result<()> {
        if let lume_hir::Item::TraitImpl(trait_impl) = symbol {
            self.check_trait_impl(trait_impl)?;
        }

        Ok(())
    }

    fn check_trait_impl(&self, trait_impl: &lume_hir::TraitImplementation) -> Result<()> {
        let use_id = trait_impl.use_id.unwrap();
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
        let mut type_params = trait_def.type_parameters.as_id_refs();
        type_params.extend(method_def.type_parameters.as_id_refs());

        let type_args = self.mk_type_refs_from(trait_impl.type_args(), DefId::Item(trait_impl.id))?;

        if method_def.visibility != method_impl.visibility {
            return Err(crate::check::errors::TraitMethodVisibilityMismatch {
                source: method_impl.location,
                expected: method_def.visibility,
                found: method_impl.visibility,
            }
            .into());
        }

        let def_sig = self.signature_of_call_ref(CallReference::Method(method_def.method_id.unwrap()))?;
        let inst_def_sig = self.instantiate_signature_isolate(def_sig.as_ref(), &type_params, &type_args);

        let impl_sig = self.signature_of_call_ref(CallReference::Method(method_impl.method_id.unwrap()))?;
        let inst_impl_sig = self.instantiate_signature_isolate(impl_sig.as_ref(), &type_params, &type_args);

        if inst_def_sig != inst_impl_sig {
            return Err(crate::check::errors::TraitMethodSignatureMismatch {
                source: method_impl.location,
                expected: method_def.signature().to_owned(),
                found: method_impl.signature().to_owned(),
            }
            .into());
        }

        Ok(())
    }
}
