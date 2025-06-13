use error_snippet::Result;

use crate::ThirBuildCtx;
use crate::check::TypeCheckerPass;

/// Type checker pass to check whether implementations
/// if any given trait is valid against the trait definition.
pub(super) struct TraitChecking<'a> {
    tcx: &'a ThirBuildCtx,
}

impl TypeCheckerPass for TraitChecking<'_> {
    #[tracing::instrument(level = "DEBUG", name = "TraitChecking::run", skip_all, err)]
    fn run(tcx: &mut ThirBuildCtx) -> Result<()> {
        for (_, symbol) in &tcx.hir.items {
            if let Err(err) = (TraitChecking { tcx }.visit(symbol)) {
                tcx.dcx.emit(err);
            }
        }

        tcx.dcx().drain()
    }
}

#[allow(clippy::unused_self)]
impl TraitChecking<'_> {
    fn visit(&self, symbol: &lume_hir::Item) -> Result<()> {
        if let lume_hir::Item::Use(trait_impl) = symbol {
            self.check_trait_impl(trait_impl)?;
        }

        Ok(())
    }

    fn check_trait_impl(&self, trait_impl: &lume_hir::TraitImplementation) -> Result<()> {
        let use_id = trait_impl.use_id.unwrap();
        let trait_type = self.tcx.trait_def_of(use_id)?;
        let lume_hir::TypeDefinition::Trait(trait_def) = self.tcx.hir_expect_type(trait_type.id) else {
            panic!("bug!: expected HIR ID to reference trait item");
        };

        if trait_impl.type_parameters.len() != trait_def.type_parameters.len() {
            return Err(crate::check::errors::TraitImplTypeParameterCountMismatch {
                source: trait_impl.location.file.clone(),
                range: trait_impl.location.index.clone(),
                expected: trait_def.type_parameters.len(),
                found: trait_impl.type_parameters.len(),
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
                    source: trait_impl.location.file.clone(),
                    range: trait_impl.location.index.clone(),
                    name: method_def.name.clone(),
                }
                .into());
            };

            self.check_trait_method(method_def, method_impl)?;
        }

        Ok(())
    }

    fn check_trait_method<'a>(
        &self,
        method_def: &'a lume_hir::TraitMethodDefinition,
        method_impl: &'a lume_hir::TraitMethodImplementation,
    ) -> Result<()> {
        if method_def.visibility != method_impl.visibility {
            return Err(crate::check::errors::TraitMethodVisibilityMismatch {
                source: method_impl.location.file.clone(),
                range: method_impl.location.index.clone(),
                expected: method_def.visibility,
                found: method_impl.visibility,
            }
            .into());
        }

        if method_def.signature() != method_impl.signature() {
            return Err(crate::check::errors::TraitMethodSignatureMismatch {
                source: method_impl.location.file.clone(),
                range: method_impl.location.index.clone(),
                expected: method_def.signature().to_owned(),
                found: method_impl.signature().to_owned(),
            }
            .into());
        }

        Ok(())
    }
}
