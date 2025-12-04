use lume_errors::Result;
use lume_span::NodeId;
use lume_type_metadata::*;
use lume_typech::query::Callable;
use lume_types::TypeRef;

use crate::reify::ReificationPass;

/// Defines the byte size of the current build architecture.
const PTR_SIZE: usize = std::mem::size_of::<usize>();

/// Defines the alignment of a pointer on the current build architecture.
const PTR_ALIGNMENT: usize = std::mem::align_of::<usize>();

impl ReificationPass<'_> {
    #[libftrace::traced(level = Debug, fields(type_ref), err)]
    pub(crate) fn build_type_metadata_of(&mut self, type_ref: &TypeRef) -> Result<TypeMetadataId> {
        let id = TypeMetadataId::from(type_ref);

        if self.static_metadata.metadata.contains_key(&id) {
            return Ok(id);
        }

        // Insert a temporary type metadata structure, so we can prevent recursive
        // lookups
        self.static_metadata.metadata.insert(id, TypeMetadata::default());

        let type_id = type_ref.instance_of;
        let full_name = format!("{:+}", self.tcx.hir_path_of_node(type_id));

        let size = self.size_of_ty(type_ref)?;
        let alignment = self.alignment_of_ty(type_ref)?;

        let fields = self.fields_on_type(type_ref)?;
        let methods = self.methods_on_type(type_ref)?;
        let drop_method = self.find_drop_method(type_ref);

        let type_parameters = self
            .tcx
            .available_type_params_at(type_id)
            .iter()
            .map(|param| self.type_parameter_metadata(*param))
            .collect::<Result<Vec<_>>>()?;

        let type_arguments = type_ref
            .bound_types
            .iter()
            .filter(|arg| self.tcx.is_type_parameter(arg).unwrap_or(false))
            .map(|arg| self.build_type_metadata_of(arg))
            .collect::<Result<Vec<_>>>()?;

        self.static_metadata.metadata.insert(id, TypeMetadata {
            id,
            full_name,
            size,
            alignment,
            type_id,
            fields,
            methods,
            type_parameters,
            type_arguments,
            drop_method,
        });

        Ok(id)
    }

    fn size_of_ty(&self, ty: &TypeRef) -> Result<usize> {
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
                            self.size_of_ty(&prop_ty)?
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
                                self.size_of_ty(&param_type_ref)?
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

    fn alignment_of_ty(&self, ty: &TypeRef) -> Result<usize> {
        match ty {
            ty if ty.is_void() => Ok(0),
            ty if ty.is_scalar_type() => self.size_of_ty(ty),
            _ => match self.tcx.hir_expect_node(ty.instance_of) {
                lume_hir::Node::Type(lume_hir::TypeDefinition::Struct(struct_def)) => {
                    // Arrays are aligned to their elemental type alignment
                    if self.tcx.is_std_array(ty) {
                        let Some(elemental_type) = ty.bound_types.first() else {
                            return Ok(PTR_SIZE);
                        };

                        return self.alignment_of_ty(elemental_type);
                    }

                    // Otherwise, use the maximum alignment of all fields on the type.
                    // We start at alignment 1, since an alignment of 0 is invalid.
                    let mut max_alignment = 1;

                    for prop in self.tcx.fields_on(ty.instance_of)? {
                        let prop_ty = self.tcx.mk_type_ref_from(&prop.field_type, struct_def.id)?;

                        max_alignment = if self.tcx.tdb().is_reference_type(prop_ty.instance_of).unwrap() {
                            PTR_SIZE.max(max_alignment)
                        } else {
                            self.alignment_of_ty(&prop_ty)?.max(max_alignment)
                        };
                    }

                    Ok(max_alignment)
                }
                lume_hir::Node::Type(_) => Ok(PTR_ALIGNMENT),
                _ => unreachable!(),
            },
        }
    }

    fn fields_on_type(&mut self, ty: &TypeRef) -> Result<Vec<FieldMetadata>> {
        self.tcx
            .fields_on(ty.instance_of)?
            .iter()
            .map(|field| {
                let name = field.name.to_string();
                let field_type = self.tcx.mk_type_ref_from(&field.field_type, ty.instance_of)?;
                let metadata_id = self.build_type_metadata_of(&field_type)?;

                Ok(FieldMetadata { name, ty: metadata_id })
            })
            .collect()
    }

    fn methods_on_type(&mut self, type_ref: &TypeRef) -> Result<Vec<MethodMetadata>> {
        let mut methods = Vec::new();

        for method in self.tcx.methods_defined_on(type_ref) {
            if method.is_intrinsic() {
                continue;
            }

            let full_name = format!("{:+}", method.name);
            let func_id = method.id;

            let definition_id = if let Some(lume_hir::Node::TraitMethodImpl(method_impl)) = self.tcx.hir_node(func_id) {
                self.tcx.trait_method_definition_of_method_impl(method_impl)?.id
            } else {
                func_id
            };

            let signature = self.tcx.signature_of(Callable::Method(method))?;

            let parameters = signature
                .params
                .iter()
                .map(|param| {
                    let name = param.name.clone();
                    let ty = self.build_type_metadata_of(&param.ty)?;

                    Ok(ParameterMetadata {
                        name,
                        ty,
                        vararg: param.vararg,
                    })
                })
                .collect::<Result<Vec<_>>>()?;

            let type_parameters = signature
                .type_params
                .iter()
                .map(|param| self.type_parameter_metadata(*param))
                .collect::<Result<Vec<_>>>()?;

            let return_type = self.build_type_metadata_of(&signature.ret_ty)?;

            methods.push(MethodMetadata {
                full_name,
                func_id,
                definition_id,
                parameters,
                type_parameters,
                return_type,
            });
        }

        Ok(methods)
    }

    fn find_drop_method(&self, type_ref: &lume_types::TypeRef) -> Option<NodeId> {
        for method in self.tcx.methods_defined_on(type_ref) {
            if self.tcx.is_method_dropper(method.id) {
                return Some(method.id);
            }
        }

        None
    }

    fn type_parameter_metadata(&mut self, id: lume_span::NodeId) -> Result<TypeParameterMetadata> {
        let type_param = self.tcx.hir_expect_type_parameter(id);

        let name = type_param.name.to_string();
        let constraints = type_param
            .constraints
            .iter()
            .map(|constraint| {
                let constraint_type = self.tcx.mk_type_ref_from(constraint, type_param.id)?;

                self.build_type_metadata_of(&constraint_type)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(TypeParameterMetadata { id, name, constraints })
    }
}
