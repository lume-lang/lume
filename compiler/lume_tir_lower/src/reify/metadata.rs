use lume_errors::Result;
use lume_span::DefId;
use lume_type_metadata::*;

use crate::reify::ReificationPass;

/// Defines the byte size of the current build architecture.
const PTR_SIZE: usize = std::mem::size_of::<usize>();

/// Defines the alignment of a pointer on the current build architecture.
const PTR_ALIGNMENT: usize = std::mem::align_of::<usize>();

impl ReificationPass<'_> {
    pub(crate) fn build_type_metadata_of(&mut self, type_ref: &lume_types::TypeRef) -> Result<TypeMetadataId> {
        let id = TypeMetadataId::from(type_ref);

        if self.static_metadata.metadata.contains_key(&id) {
            return Ok(id);
        }

        // Insert a temporary type metadata structure, so we can prevent recursive lookups
        self.static_metadata.metadata.insert(id, TypeMetadata::default());

        let full_name = format!("{:+}", self.tcx.tdb().ty_expect(type_ref.instance_of)?.name);
        let ty = self.tcx.tdb().ty_expect(type_ref.instance_of)?;
        let type_id = ty.id;

        let size = self.size_of_ty(ty)?;
        let alignment = self.alignment_of_ty(type_ref)?;

        let fields = self.fields_on_type(ty)?;
        let methods = self.methods_on_type(type_ref)?;

        let type_arguments = type_ref
            .type_arguments
            .iter()
            .filter(|arg| self.tcx.is_type_parameter(arg).unwrap_or(false))
            .map(|arg| self.build_type_metadata_of(arg))
            .collect::<Result<Vec<_>>>()?;

        self.static_metadata.metadata.insert(
            id,
            TypeMetadata {
                id,
                full_name,
                size,
                alignment,
                type_id,
                fields,
                methods,
                type_arguments,
            },
        );

        Ok(id)
    }

    fn size_of_ty(&self, ty: &lume_types::Type) -> Result<usize> {
        let size = match &ty.kind {
            lume_types::TypeKind::Void => 0,
            lume_types::TypeKind::Bool => 1,
            lume_types::TypeKind::Int(n) | lume_types::TypeKind::UInt(n) | lume_types::TypeKind::Float(n) => {
                (*n / 8) as usize
            }
            lume_types::TypeKind::String
            | lume_types::TypeKind::TypeParameter(_)
            | lume_types::TypeKind::User(lume_types::UserType::Trait(_)) => PTR_SIZE,
            lume_types::TypeKind::User(lume_types::UserType::Struct(_)) => {
                let mut size = 0;

                for prop in self.tcx.tdb().find_fields(ty.id) {
                    let prop_ty = self.tcx.tdb().ty_expect(prop.field_type.instance_of)?;

                    size += if prop_ty.kind.is_ref_type() {
                        PTR_SIZE
                    } else {
                        self.size_of_ty(prop_ty)?
                    };
                }

                size
            }
            lume_types::TypeKind::User(lume_types::UserType::Enum(def)) => {
                // We start with 1 byte for the discriminant.
                let mut size = 1;

                for variant in self.tcx.enum_cases_of_name(&def.name)? {
                    for param in &variant.parameters {
                        let param_type_ref = self.tcx.mk_type_ref_from(param, DefId::Item(def.id))?;
                        let param_ty = self.tcx.tdb().ty_expect(param_type_ref.instance_of)?;

                        size += if param_ty.kind.is_ref_type() {
                            PTR_SIZE
                        } else {
                            self.size_of_ty(param_ty)?
                        };
                    }
                }

                size
            }
        };

        Ok(size)
    }

    fn alignment_of_ty(&self, type_ref: &lume_types::TypeRef) -> Result<usize> {
        let ty = self.tcx.tdb().ty_expect(type_ref.instance_of)?;

        match &ty.kind {
            lume_types::TypeKind::Void
            | lume_types::TypeKind::Bool
            | lume_types::TypeKind::Int(_)
            | lume_types::TypeKind::UInt(_)
            | lume_types::TypeKind::Float(_) => self.size_of_ty(ty),
            lume_types::TypeKind::String
            | lume_types::TypeKind::TypeParameter(_)
            | lume_types::TypeKind::User(lume_types::UserType::Trait(_) | lume_types::UserType::Enum(_)) => {
                Ok(PTR_ALIGNMENT)
            }
            lume_types::TypeKind::User(lume_types::UserType::Struct(_)) => {
                // Arrays are aligned to their elemental type alignment
                if type_ref.instance_of == lume_types::TYPEREF_ARRAY_ID {
                    return self.alignment_of_ty(type_ref.type_arguments.first().unwrap());
                }

                // Otherwise, use the maximum alignment of all fields on the type.
                // We start at alignment 1, since an alignment of 0 is invalid.
                let mut max_alignment = 1;

                for prop in self.tcx.tdb().find_fields(ty.id) {
                    let prop_ty = &prop.field_type;

                    max_alignment = if self.tcx.tdb().is_reference_type(prop_ty.instance_of).unwrap() {
                        PTR_SIZE.max(max_alignment)
                    } else {
                        self.alignment_of_ty(prop_ty)?.max(max_alignment)
                    };
                }

                Ok(max_alignment)
            }
        }
    }

    fn fields_on_type(&mut self, ty: &lume_types::Type) -> Result<Vec<FieldMetadata>> {
        self.tcx
            .tdb()
            .find_fields(ty.id)
            .map(|field| {
                let name = field.name.clone();
                let ty = self.build_type_metadata_of(&field.field_type)?;

                Ok(FieldMetadata { name, ty })
            })
            .collect()
    }

    fn methods_on_type(&mut self, type_ref: &lume_types::TypeRef) -> Result<Vec<MethodMetadata>> {
        let mut methods = Vec::new();

        for method in self.tcx.methods_defined_on(type_ref) {
            if method.is_intrinsic() {
                continue;
            }

            let full_name = format!("{:+}", method.name);
            let func_id = method.hir;

            let definition_id = if let lume_hir::Def::TraitMethodImpl(method_impl) = self.tcx.hir_expect_def(func_id) {
                self.tcx.hir_trait_method_def_of_impl(method_impl)?.id
            } else {
                func_id
            };

            let parameters = method
                .parameters
                .inner()
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

            let type_parameters = method
                .type_parameters
                .iter()
                .map(|param| self.type_parameter_metadata(*param))
                .collect::<Result<Vec<_>>>()?;

            let return_type = self.build_type_metadata_of(&method.return_type)?;

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

    fn type_parameter_metadata(&mut self, id: lume_hir::TypeParameterId) -> Result<TypeParameterMetadata> {
        let type_param = self.tcx.tdb().type_parameter(id).unwrap();

        let name = type_param.name.clone();
        let constraints = type_param
            .constraints
            .iter()
            .map(|constraint| self.build_type_metadata_of(constraint))
            .collect::<Result<Vec<_>>>()?;

        Ok(TypeParameterMetadata { name, constraints })
    }
}
