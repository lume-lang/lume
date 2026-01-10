use lume_errors::Result;
use lume_span::NodeId;
use lume_type_metadata::*;
use lume_typech::TyCheckCtx;
use lume_typech::query::Callable;
use lume_types::TypeRef;

/// Defines the byte size of the current build architecture.
const PTR_SIZE: usize = std::mem::size_of::<usize>();

/// Defines the alignment of a pointer on the current build architecture.
const PTR_ALIGNMENT: usize = std::mem::align_of::<usize>();

pub(crate) struct MetadataBuilder<'tcx> {
    tcx: &'tcx TyCheckCtx,
    metadata: StaticMetadata,
}

impl<'tcx> MetadataBuilder<'tcx> {
    pub(crate) fn new(tcx: &'tcx TyCheckCtx) -> Self {
        Self {
            tcx,
            metadata: StaticMetadata::default(),
        }
    }

    #[libftrace::traced(level = Debug, fields(type_ref), err)]
    pub(crate) fn build_metadata(mut self) -> Result<StaticMetadata> {
        lume_type_metadata::visitor::traverse(self.tcx, &mut self)?;

        Ok(self.metadata)
    }

    fn find_drop_method(&self, type_ref: &lume_types::TypeRef) -> Option<NodeId> {
        for method in self.tcx.methods_defined_on(type_ref) {
            if self.tcx.is_method_dropper(method.id) {
                return Some(method.id);
            }
        }

        None
    }

    fn kind_of_type(&self, ty: &TypeRef) -> TypeKind {
        match self.tcx.tdb().expect_type(ty.instance_of).unwrap().kind {
            lume_types::TypeKind::Struct
            | lume_types::TypeKind::Void
            | lume_types::TypeKind::Bool
            | lume_types::TypeKind::Int(_)
            | lume_types::TypeKind::UInt(_)
            | lume_types::TypeKind::Float(_)
            | lume_types::TypeKind::String => TypeKind::Struct,
            lume_types::TypeKind::Enum => TypeKind::Enum,
            lume_types::TypeKind::Trait => TypeKind::Trait,
            lume_types::TypeKind::TypeParameter => TypeKind::TypeParameter,
        }
    }

    fn size_of_type(&self, ty: &TypeRef) -> Result<usize> {
        match ty {
            ty if ty.is_void() => Ok(0),
            ty if ty.is_bool() => Ok(1),
            ty if ty.is_float() || ty.is_integer() => Ok((ty.bitwidth() / 8) as usize),
            _ => match self.tcx.hir_expect_node(ty.instance_of) {
                lume_hir::Node::Type(lume_hir::TypeDefinition::Struct(struct_def)) => {
                    // Take metadata pointer into account when calculating the size.
                    let mut size = PTR_SIZE;

                    for prop in self.tcx.fields_on(struct_def.id)? {
                        let prop_ty = self.tcx.mk_type_ref_from(&prop.field_type, struct_def.id)?;

                        size += if prop_ty.is_scalar_type() {
                            self.size_of_type(&prop_ty)?
                        } else {
                            PTR_SIZE
                        };
                    }

                    Ok(size)
                }
                lume_hir::Node::Type(lume_hir::TypeDefinition::Enum(enum_def)) => {
                    // We start with 1 byte for the discriminant, plus the size
                    // of the metadata pointer.
                    let mut size = PTR_SIZE + 1;

                    for variant in self.tcx.cases_of_enum_definition(&enum_def.name)? {
                        for param in &variant.parameters {
                            let param_type_ref = self.tcx.mk_type_ref_from(param, enum_def.id)?;

                            size += if param_type_ref.is_scalar_type() {
                                self.size_of_type(&param_type_ref)?
                            } else {
                                PTR_SIZE
                            };
                        }
                    }

                    Ok(size)
                }
                lume_hir::Node::Type(
                    lume_hir::TypeDefinition::Trait(_) | lume_hir::TypeDefinition::TypeParameter(_),
                ) => Ok(PTR_SIZE),
                _ => unreachable!(),
            },
        }
    }

    fn alignment_of_type(&self, ty: &TypeRef) -> Result<usize> {
        match ty {
            ty if ty.is_void() => Ok(0),
            ty if ty.is_scalar_type() => self.size_of_type(ty),
            _ => match self.tcx.hir_expect_node(ty.instance_of) {
                lume_hir::Node::Type(lume_hir::TypeDefinition::Struct(struct_def)) => {
                    // Arrays are aligned to their elemental type alignment
                    if self.tcx.is_std_array(ty) {
                        let Some(elemental_type) = ty.bound_types.first() else {
                            return Ok(PTR_SIZE);
                        };

                        return self.alignment_of_type(elemental_type);
                    }

                    // Otherwise, use the maximum alignment of all fields on the type.
                    // We start at alignment 1, since an alignment of 0 is invalid.
                    let mut max_alignment = 1;

                    for prop in self.tcx.fields_on(ty.instance_of)? {
                        let prop_ty = self.tcx.mk_type_ref_from(&prop.field_type, struct_def.id)?;

                        max_alignment = if self.tcx.tdb().is_reference_type(prop_ty.instance_of).unwrap() {
                            PTR_SIZE.max(max_alignment)
                        } else {
                            self.alignment_of_type(&prop_ty)?.max(max_alignment)
                        };
                    }

                    Ok(max_alignment)
                }
                lume_hir::Node::Type(_) => Ok(PTR_ALIGNMENT),
                _ => unreachable!(),
            },
        }
    }
}

impl lume_type_metadata::visitor::Visitor for MetadataBuilder<'_> {
    fn visit_type(&mut self, type_ref: &lume_types::TypeRef) -> Result<()> {
        let id = TypeMetadataId::from(type_ref);

        let type_id = type_ref.instance_of;
        let full_name = self.tcx.hir_path_of_node(type_id).to_wide_string();

        let mangle_version = lume_mangle::Version::default();
        let mangled_name = lume_mangle::mangled(self.tcx, type_id, mangle_version).unwrap();

        let kind = self.kind_of_type(type_ref);
        let size = self.size_of_type(type_ref)?;
        let alignment = self.alignment_of_type(type_ref)?;

        let fields = self
            .tcx
            .fields_on(type_ref.instance_of)?
            .iter()
            .map(|field| field.id)
            .collect();

        let methods = self
            .tcx
            .methods_defined_on(type_ref)
            .iter()
            .filter(|method| !method.is_intrinsic())
            .map(|method| method.id)
            .collect();

        let drop_method = self.find_drop_method(type_ref);
        let type_parameters = self.tcx.available_type_params_at(type_id);

        self.metadata.types.insert(id, TypeMetadata {
            id,
            full_name,
            mangled_name,
            kind,
            size,
            alignment,
            type_id,
            fields,
            methods,
            type_parameters,
            drop_method,
        });

        Ok(())
    }

    fn visit_field(&mut self, field: &lume_hir::Field) -> Result<()> {
        let name = field.name.to_string();
        let field_type = self.tcx.mk_type_ref_from(&field.field_type, field.id)?;
        let metadata_id = TypeMetadataId::from(&field_type);

        self.metadata
            .fields
            .insert(field.id, FieldMetadata { name, ty: metadata_id });

        Ok(())
    }

    fn visit_method(&mut self, method: &lume_types::Method) -> Result<()> {
        let func_id = method.id;
        let full_name = method.name.to_wide_string();

        let definition_id = if let Some(lume_hir::Node::TraitMethodImpl(method_impl)) = self.tcx.hir_node(func_id) {
            self.tcx.trait_method_definition_of_method_impl(method_impl)?.id
        } else {
            func_id
        };

        let signature = self.tcx.signature_of(Callable::Method(method))?;

        let parameters = signature.params.iter().map(|param| param.id).collect();
        let type_parameters = signature.type_params;
        let return_type = TypeMetadataId::from(&signature.ret_ty);

        self.metadata.methods.insert(func_id, MethodMetadata {
            full_name,
            func_id,
            definition_id,
            parameters,
            type_parameters,
            return_type,
        });

        Ok(())
    }

    fn visit_parameter(&mut self, parameter: &lume_types::Parameter) -> Result<()> {
        self.metadata.parameters.insert(parameter.id, ParameterMetadata {
            id: parameter.id,
            name: parameter.name.clone(),
            ty: TypeMetadataId::from(&parameter.ty),
            vararg: parameter.vararg,
        });

        Ok(())
    }

    fn visit_type_parameter(&mut self, type_param: &lume_hir::TypeParameter) -> Result<()> {
        let name = type_param.name.to_string();
        let constraints = type_param
            .constraints
            .iter()
            .map(|constraint| {
                let constraint_type = self.tcx.mk_type_ref_from(constraint, type_param.id)?;

                Ok(TypeMetadataId::from(&constraint_type))
            })
            .collect::<Result<Vec<_>>>()?;

        self.metadata
            .type_parameters
            .insert(type_param.id, TypeParameterMetadata {
                id: type_param.id,
                name,
                constraints,
            });

        Ok(())
    }
}
