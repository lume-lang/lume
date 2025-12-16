use lume_mir::RegisterId;
use lume_span::Location;

use crate::builder::Builder;
use crate::builder::decl::OperandRef;

pub(crate) fn pattern(
    builder: &mut Builder<'_, '_>,
    pattern: &lume_tir::Pattern,
    operand: lume_mir::Operand,
    operand_type: lume_mir::Type,
) -> lume_mir::Operand {
    let loaded_op = builder.declare_operand_as(operand_type, operand.clone(), OperandRef::Implicit);

    match &pattern.kind {
        lume_tir::PatternKind::Literal(lit) => literal_pattern(builder, operand, lit),
        lume_tir::PatternKind::Variant(variant) => {
            variant_pattern(builder, loaded_op, pattern, variant, pattern.location)
        }

        // Testing against a variable is effectively a noop, since it will always be true.
        lume_tir::PatternKind::Variable(var) => {
            let block = builder.func.current_block().id;
            builder.declare_var(block, *var, loaded_op);

            lume_mir::Operand {
                kind: lume_mir::OperandKind::Boolean { value: true },
                location: pattern.location,
            }
        }

        // Wildcard patterns are always true, so we implicitly replace it with a `true` expression.
        lume_tir::PatternKind::Wildcard => lume_mir::Operand {
            kind: lume_mir::OperandKind::Boolean { value: true },
            location: pattern.location,
        },
    }
}

fn literal_pattern(
    builder: &mut Builder<'_, '_>,
    operand: lume_mir::Operand,
    literal: &lume_tir::Literal,
) -> lume_mir::Operand {
    // Matching against a literal is effectively the same as testing
    // the equality against the operand, so we replace it will an intrinsic call,
    // depending on the type of the operand.
    let intrinsic = match &literal.kind {
        lume_tir::LiteralKind::Boolean(bool) => lume_mir::DeclarationKind::Intrinsic {
            name: lume_mir::Intrinsic::BooleanEq,
            args: vec![operand, lume_mir::Operand {
                kind: lume_mir::OperandKind::Boolean { value: *bool },
                location: literal.location,
            }],
        },
        lume_tir::LiteralKind::Int(int) => lume_mir::DeclarationKind::Intrinsic {
            name: lume_mir::Intrinsic::IntEq {
                bits: int.bits(),
                signed: int.signed(),
            },
            args: vec![operand, lume_mir::Operand {
                kind: lume_mir::OperandKind::Integer {
                    bits: int.bits(),
                    signed: int.signed(),
                    value: int.value(),
                },
                location: literal.location,
            }],
        },
        lume_tir::LiteralKind::Float(float) => lume_mir::DeclarationKind::Intrinsic {
            name: lume_mir::Intrinsic::FloatEq { bits: float.bits() },
            args: vec![operand, lume_mir::Operand {
                kind: lume_mir::OperandKind::Float {
                    bits: float.bits(),
                    value: float.value(),
                },
                location: literal.location,
            }],
        },
        lume_tir::LiteralKind::String(_) => unimplemented!(),
    };

    let result = builder.declare(lume_mir::Declaration {
        kind: Box::new(intrinsic),
        location: literal.location,
    });

    lume_mir::Operand {
        kind: lume_mir::OperandKind::Reference { id: result },
        location: literal.location,
    }
}

fn variant_pattern(
    builder: &mut Builder<'_, '_>,
    loaded_op: RegisterId,
    pattern: &lume_tir::Pattern,
    variant: &lume_tir::VariantPattern,
    location: Location,
) -> lume_mir::Operand {
    let discriminant_value = builder
        .tcx()
        .discriminant_of_variant(variant.ty.instance_of, variant.name.name.name())
        .unwrap();

    let operand_disc = builder.declare_operand_as(
        lume_mir::Type::u8(),
        lume_mir::Operand {
            kind: lume_mir::OperandKind::LoadField {
                target: loaded_op,
                offset: 0,
                field_type: lume_mir::Type::u8(),
            },
            location,
        },
        OperandRef::Implicit,
    );

    let mut cmp_result = builder.ieq_imm(
        lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id: operand_disc },
            location,
        },
        discriminant_value.cast_signed() as i64,
        8,
        false,
    );

    for (idx, field_pattern) in variant.fields.iter().enumerate() {
        // We intentionally ignore wildcard patterns, as they are always true and
        // have no other side effects.
        if matches!(field_pattern.kind, lume_tir::PatternKind::Wildcard) {
            continue;
        }

        let field_cmp_operand = load_variant_subpattern(builder, pattern, loaded_op, field_pattern, idx);

        let field_cmp_intrinsic = lume_mir::DeclarationKind::Intrinsic {
            name: lume_mir::Intrinsic::BooleanAnd,
            args: vec![
                lume_mir::Operand {
                    kind: lume_mir::OperandKind::Reference { id: cmp_result },
                    location,
                },
                field_cmp_operand,
            ],
        };

        cmp_result = builder.declare(lume_mir::Declaration {
            kind: Box::new(field_cmp_intrinsic),
            location: field_pattern.location,
        });
    }

    lume_mir::Operand {
        kind: lume_mir::OperandKind::Reference { id: cmp_result },
        location,
    }
}

/// Loads the given subpattern from some parent pattern into a register and
/// returns it as an operand.
fn load_variant_subpattern(
    builder: &mut Builder<'_, '_>,
    parent_pattern: &lume_tir::Pattern,
    parent_operand: RegisterId,
    subpattern: &lume_tir::Pattern,
    field_idx: usize,
) -> lume_mir::Operand {
    let field_offset = variant_field_offset(builder, parent_pattern.id, field_idx);
    let field_type = variant_field_type(builder, parent_pattern.id, field_idx);

    // If the field is a scalar type, it should be loaded directly since any
    // following operations might expect it to be a non-pointer.
    if !field_type.is_reference_type() || field_type.is_generic {
        let field_operand = lume_mir::Operand {
            kind: lume_mir::OperandKind::LoadField {
                target: parent_operand,
                offset: field_offset,
                field_type: field_type.clone(),
            },
            location: subpattern.location,
        };

        return pattern(builder, subpattern, field_operand, field_type);
    }

    // If the field is a reference type, we define a new register which is equal to
    // the field pointer itself, plus the size of the discriminant.
    let discriminant_size = lume_mir::Type::u8().bytesize();
    let parent_operand = lume_mir::Operand {
        kind: lume_mir::OperandKind::Reference { id: parent_operand },
        location: parent_pattern.location,
    };

    let field_operand_reg = builder.iadd_imm(parent_operand, discriminant_size.cast_signed() as i64, 64, false);

    let mut field_operand = lume_mir::Operand::reference_of(field_operand_reg);
    field_operand.location = parent_pattern.location;

    pattern(
        builder,
        subpattern,
        field_operand,
        lume_mir::Type::pointer(lume_mir::Type::void()),
    )
}

fn variant_field_type(builder: &Builder<'_, '_>, id: lume_span::NodeId, field_idx: usize) -> lume_mir::Type {
    let pattern = builder.tcx().hir_expect_pattern(id);
    let lume_hir::PatternKind::Variant(variant_pattern) = &pattern.kind else {
        panic!("bug!: attempting to get field offset of non-variant pattern");
    };

    let field_type = builder
        .tcx()
        .type_of_variant_field_uninstantiated(&variant_pattern.name, field_idx)
        .unwrap();

    builder.lower_type(&field_type)
}

fn variant_field_offset(builder: &Builder<'_, '_>, id: lume_span::NodeId, field_idx: usize) -> usize {
    // We start off with the size of the discriminant of the variant.
    let mut offset = lume_mir::Type::u8().bytesize();

    let pattern = builder.tcx().hir_expect_pattern(id);
    let lume_hir::PatternKind::Variant(variant_pattern) = &pattern.kind else {
        panic!("bug!: attempting to get field offset of non-variant pattern");
    };

    let enum_case = builder.tcx().enum_case_with_name(&variant_pattern.name).unwrap();

    for (idx, field_type) in enum_case.parameters.iter().enumerate() {
        if idx == field_idx {
            break;
        }

        let field_type_ty = builder.tcx().mk_type_ref_from(field_type, id).unwrap();
        let prop_ty = builder.lower_type(&field_type_ty);
        offset += prop_ty.bytesize();
    }

    offset
}
