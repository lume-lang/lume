use std::path::PathBuf;
use std::sync::Arc;

use indexmap::{IndexMap, IndexSet};
use lume_errors::Result;
use lume_hir::Map;
use lume_infer::TyInferCtx;
use lume_session::GlobalCtx;
use lume_span::PackageId;
use lume_typech::TyCheckCtx;

use crate::*;

/// Compiler pipeline wrapper - functions as the entrypoint for any interactions
/// with the compiler directly.
///
/// See [`pipeline()`].
pub struct Pipeline(Arc<GlobalCtx>);

/// Compiler pipeline wrapper - functions as the entrypoint for any interactions
/// with the compiler directly.
///
/// # Examples
///
/// To create a new compiler pipeline, call the function with the global
/// context:
/// ```
/// use std::sync::Arc;
///
/// use lume_driver::pipeline;
/// use lume_session::GlobalCtx;
///
/// let gcx = Arc::new(GlobalCtx::default());
/// let pipeline = pipeline(gcx);
/// ```
#[inline]
pub fn pipeline(gcx: Arc<GlobalCtx>) -> Pipeline {
    Pipeline(gcx)
}

impl<IO> Driver<IO> {
    /// Creates a new pipeline from the current driver instance.
    pub fn to_pipeline(self) -> Pipeline {
        let session = Session {
            dep_graph: self.dependencies,
            workspace_root: self.package.path,
            options: self.config.options,
        };

        let gcx = Arc::new(GlobalCtx::new(session, self.dcx.to_context()));

        pipeline(gcx)
    }
}

/// Compiler stage: HIR
pub struct LoweredToHir {
    pub gcx: Arc<GlobalCtx>,
    pub maps: IndexMap<PackageId, StageResult<TyInferCtx>>,
}

pub enum StageResult<T> {
    Value(Box<T>),
    Cached { bc_path: PathBuf },
}

impl<T> StageResult<T> {
    pub fn value(value: T) -> Self {
        Self::Value(Box::new(value))
    }
}

/// Compiler stage: type-checked
pub struct TypeChecked {
    pub gcx: Arc<GlobalCtx>,
    pub ctx: IndexMap<PackageId, StageResult<TyCheckCtx>>,
}

/// Compiler stage: TIR
pub struct LoweredToTir {
    pub gcx: Arc<GlobalCtx>,
    pub tir: IndexMap<PackageId, StageResult<(lume_typech::TyCheckCtx, lume_tir::TypedIR)>>,
}

/// Compiler stage: MIR
pub struct LoweredToMir {
    pub gcx: Arc<GlobalCtx>,
    pub mir: IndexMap<PackageId, StageResult<(lume_typech::TyCheckCtx, lume_mir::ModuleMap)>>,
}

/// Compiler stage: codegen
#[cfg(feature = "codegen")]
pub struct GeneratedCode {
    pub gcx: Arc<GlobalCtx>,
    pub objects: IndexMap<PackageId, StageResult<GeneratedObject>>,
}

#[cfg(feature = "codegen")]
pub struct GeneratedObject {
    pub name: String,
    pub object: Vec<u8>,
    pub metadata: lume_metadata::PackageMetadata,
}

impl Pipeline {
    #[tracing::instrument(level = "INFO", skip_all)]
    pub fn lower_to_hir(self) -> Result<LoweredToHir> {
        let Pipeline(gcx) = self;

        let mut maps = IndexMap::new();
        let mut dependencies = gcx.session.dep_graph.iter().cloned().collect::<Vec<_>>();

        // Build all the dependencies of the package in reverse, so all the
        // dependencies without any sub-dependencies can be built first.
        dependencies.reverse();

        let mut dependency_hir = Map::empty(PackageId::empty());
        let mut dirty_packages: IndexSet<PackageId> = IndexSet::with_capacity(dependencies.len());

        for dependency in dependencies {
            tracing::info!(
                "lowering {} v{} ({})",
                dependency.name,
                dependency.version,
                dependency.path.display()
            );

            let has_hash_changed = needs_compilation(&gcx, &dependency);
            let bc_path = gcx.obj_bc_path_of(&dependency.name);

            // We can skip recompilation of a package if *all* the following circumstances
            // are true:
            // - none of it's dependencies have been marked as "dirty",
            // - the package hash within the package's `.mlib` file is the same as now,
            // - and the target object file already exists
            if !dirty_packages.contains(&dependency.id) && !has_hash_changed && bc_path.exists() {
                maps.insert(dependency.id, StageResult::Cached { bc_path });
                continue;
            }

            // If the package hash is different from the cached hash, mark all the depending
            // packages as dirty.
            //
            // The check is meant to prevent the re-compilation in the case where the hash
            // is the same, but re-compilation of a dependency was required since the object
            // file was missing.
            if has_hash_changed {
                dirty_packages.extend(gcx.session.dep_graph.dependents_of(dependency.id));
            }

            let mut hir = tracing::info_span!("hir_lower")
                .in_scope(|| gcx.dcx.with(|dcx| lume_hir_lower::lower_to_hir(&dependency, dcx)))?;

            #[allow(clippy::disallowed_macros, reason = "only used in debugging")]
            if gcx.session.options.dump_hir {
                let mut package_hir = hir.clone();
                package_hir
                    .nodes
                    .retain(|id, _node| id.package == gcx.session.dep_graph.root);

                println!("{package_hir:#?}");
            }

            dependency_hir.clone().merge_into(&mut hir);

            // Create an inferencing context so we can partition all the public HIR nodes,
            // for use in any dependants.
            let tcx = tracing::info_span!("type_inference").in_scope(|| -> Result<TyInferCtx> {
                let tcx = lume_types::TyCtx::new(gcx.clone());

                let mut ticx = TyInferCtx::new(tcx, hir);
                ticx.infer()?;

                Ok(ticx)
            })?;

            let public_hir = if gcx.session.options.export_private_nodes {
                tcx.hir().clone()
            } else {
                lume_metadata::partition_public_nodes(&tcx)
            };

            public_hir.merge_into(&mut dependency_hir);

            maps.insert(dependency.id, StageResult::value(tcx));
        }

        Ok(LoweredToHir { gcx, maps })
    }
}

impl LoweredToHir {
    #[tracing::instrument(level = "INFO", skip_all)]
    pub fn type_check(self) -> Result<TypeChecked> {
        let LoweredToHir { gcx, maps } = self;
        let mut ctx = IndexMap::with_capacity(maps.len());

        for (package_id, map) in maps {
            tracing::debug!(package = gcx.package_name(package_id).unwrap_or("<empty>"));

            let mut tcx = match map {
                StageResult::Value(tcx) => tcx,
                StageResult::Cached { bc_path } => {
                    ctx.insert(package_id, StageResult::Cached { bc_path });
                    continue;
                }
            };

            // Unifies all the types within the type inference context.
            tracing::info_span!("type_unification").in_scope(|| lume_unification::unify(&mut tcx))?;

            // Then, make sure they're all valid.
            let tcx = tracing::info_span!("type_checking").in_scope(|| -> Result<TyCheckCtx> {
                let mut tcx = lume_typech::TyCheckCtx::new(*tcx);
                let _ = tcx.typecheck();

                Ok(tcx)
            })?;

            #[allow(clippy::disallowed_macros, reason = "only used in debugging")]
            if tcx.gcx().session.options.print_type_context {
                println!("{:#?}", tcx.tdb());
            }

            ctx.insert(package_id, StageResult::value(tcx));
        }

        Ok(TypeChecked { gcx, ctx })
    }
}

impl TypeChecked {
    #[tracing::instrument(level = "INFO", skip_all)]
    pub fn lower_to_tir(self) -> Result<LoweredToTir> {
        let TypeChecked { gcx, ctx } = self;
        let mut tir = IndexMap::with_capacity(ctx.len());

        for (package_id, tcx) in ctx {
            let tcx = match tcx {
                StageResult::Value(tcx) => *tcx,
                StageResult::Cached { bc_path } => {
                    tir.insert(package_id, StageResult::Cached { bc_path });
                    continue;
                }
            };

            tracing::debug!(package = tcx.gcx().package_name(package_id).unwrap_or("<empty>"));

            let typed_ir = lume_tir_lower::Lower::build(&tcx)?;
            tir.insert(package_id, StageResult::value((tcx, typed_ir)));
        }

        Ok(LoweredToTir { gcx, tir })
    }
}

impl LoweredToTir {
    #[tracing::instrument(level = "INFO", skip_all)]
    pub fn lower_to_mir(self) -> Result<LoweredToMir> {
        let LoweredToTir { gcx, tir } = self;
        let mut mir_maps = IndexMap::with_capacity(tir.len());

        for (package_id, result) in tir {
            let (tcx, typed_ir) = match result {
                StageResult::Value(res) => *res,
                StageResult::Cached { bc_path } => {
                    mir_maps.insert(package_id, StageResult::Cached { bc_path });
                    continue;
                }
            };

            tracing::debug!(package = gcx.package_name(package_id).unwrap_or("<empty>"));

            let opts = gcx.session.options.clone();
            let package = gcx.package(package_id).unwrap().to_owned();

            let mut transformer = lume_mir_lower::ModuleTransformer::create(package, &tcx, typed_ir.metadata, opts);
            let mir = transformer.transform(&typed_ir.functions.into_values().collect::<Vec<_>>());
            let optimized_mir = lume_mir_opt::Optimizer::optimize(&tcx, mir);

            mir_maps.insert(package_id, StageResult::Value(Box::new((tcx, optimized_mir))));
        }

        Ok(LoweredToMir { gcx, mir: mir_maps })
    }
}

impl LoweredToMir {
    #[cfg(feature = "codegen")]
    #[tracing::instrument(level = "INFO", skip_all)]
    pub fn codegen(self) -> Result<GeneratedCode> {
        let LoweredToMir { gcx, mir } = self;
        let mut objects = IndexMap::with_capacity(mir.len());

        for (package_id, map) in mir {
            tracing::debug!(package = gcx.package_name(package_id).unwrap_or("<empty>"));

            let (tcx, mir) = match map {
                StageResult::Value(res) => *res,
                StageResult::Cached { bc_path } => {
                    objects.insert(package_id, StageResult::Cached { bc_path });
                    continue;
                }
            };

            let metadata = lume_metadata::PackageMetadata::create(&mir.package, &tcx);
            let object = lume_codegen::generate(mir)?;

            objects.insert(
                package_id,
                StageResult::value(GeneratedObject {
                    name: metadata.header.name.clone(),
                    object,
                    metadata,
                }),
            );
        }

        Ok(GeneratedCode { gcx, objects })
    }
}

/// Determines whether the given package needs to be compiled or re-compiled.
///
/// This takes the state of the current package metadata into account, as well
/// as if anything has changed within it' source code.
#[tracing::instrument(level = "DEBUG", skip_all, fields(package = %package.name), ret)]
pub(crate) fn needs_compilation(gcx: &Arc<GlobalCtx>, package: &Package) -> bool {
    // If incremental compilation is disabled, we should alwas re-compile.
    if !gcx.session.options.enable_incremental {
        tracing::debug!("re-compilation required: incremental compilation disabled");
        return true;
    }

    let metadata_directory = gcx.obj_metadata_path();
    let metadata_filename = lume_metadata::metadata_filename_of(&package.name);
    let metadata_path = metadata_directory.join(metadata_filename);

    // If no metadata file could be found, the package has likely not been built
    // yet - in which case it obviously needs to be built.
    let Ok(Some(metadata)) = lume_metadata::read_metadata_header(metadata_path) else {
        tracing::debug!("re-compilation required: could not read metadata header");
        return true;
    };

    let current_hash = package.package_hash();

    #[allow(clippy::needless_bool, reason = "lint only raised when tracing is disabled")]
    if metadata.hash == current_hash {
        tracing::debug!("hash matched, compilation not required");

        false
    } else {
        tracing::debug!(
            message = "hash mismatch between packages",
            current = %current_hash,
            build = %metadata.hash
        );

        true
    }
}
