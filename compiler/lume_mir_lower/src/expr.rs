use lume_mir::RegisterId;
use lume_span::Location;

use crate::FunctionTransformer;

impl FunctionTransformer<'_, '_> {
    pub(super) fn expression(&mut self, expr: &lume_tir::Expression) -> lume_mir::Operand {
        let op = match &expr.kind {
            lume_tir::ExpressionKind::Assignment(expr) => self.assignment(expr),
            lume_tir::ExpressionKind::Binary(expr) => self.binary(expr),
            lume_tir::ExpressionKind::Cast(expr) => self.cast(expr),
            lume_tir::ExpressionKind::Construct(expr) => self.construct(expr),
            lume_tir::ExpressionKind::Call(expr) => self.call_expression(expr),
            lume_tir::ExpressionKind::If(cond) => self.if_condition(cond),
            lume_tir::ExpressionKind::Is(expr) => self.is_expression(expr),
            lume_tir::ExpressionKind::IntrinsicCall(call) => self.intrinsic_call(call),
            lume_tir::ExpressionKind::Literal(lit) => self.literal(&lit.kind, expr.location()),
            lume_tir::ExpressionKind::Logical(expr) => self.logical(expr),
            lume_tir::ExpressionKind::Member(expr) => self.member(expr),
            lume_tir::ExpressionKind::Scope(expr) => self.scope(expr),
            lume_tir::ExpressionKind::Switch(var) => self.switch(var),
            lume_tir::ExpressionKind::Variable(var) => self.variable_reference(var),
            lume_tir::ExpressionKind::Variant(var) => self.variant(var),
        };

        if expr.ty.is_scalar_type() && self.type_of_value(&op).is_reference_type() {
            let return_ty = self.lower_type(&expr.ty);

            let target_reg = self.load_operand(&op);
            let loaded_reg = self.func.declare_value_raw(return_ty, lume_mir::Operand {
                kind: lume_mir::OperandKind::Load { id: target_reg },
                location: expr.location().clone_inner(),
            });

            return lume_mir::Operand {
                kind: lume_mir::OperandKind::Reference { id: loaded_reg },
                location: expr.location().clone_inner(),
            };
        }

        op
    }

    fn assignment(&mut self, expr: &lume_tir::Assignment) -> lume_mir::Operand {
        let value = self.expression(&expr.value);
        let target_expr = self.expression(&expr.target);

        match &target_expr.kind {
            lume_mir::OperandKind::Reference { id } => {
                self.func
                    .current_block_mut()
                    .assign(*id, value, expr.location.clone_inner());

                lume_mir::Operand {
                    kind: lume_mir::OperandKind::Reference { id: *id },
                    location: expr.location.clone_inner(),
                }
            }
            lume_mir::OperandKind::Load { id } => {
                self.func
                    .current_block_mut()
                    .store(*id, value, expr.location.clone_inner());

                lume_mir::Operand {
                    kind: lume_mir::OperandKind::Load { id: *id },
                    location: expr.location.clone_inner(),
                }
            }
            lume_mir::OperandKind::LoadField { target, offset, .. } => {
                self.func
                    .current_block_mut()
                    .store_field(*target, *offset, value, expr.location.clone_inner());

                target_expr
            }
            lume_mir::OperandKind::LoadSlot { target, offset, .. } => {
                self.func
                    .current_block_mut()
                    .store_slot(*target, *offset, value, expr.location.clone_inner());

                target_expr
            }
            lume_mir::OperandKind::Bitcast { .. } => panic!("bug!: attempted to assign bitcast"),
            lume_mir::OperandKind::SlotAddress { .. } => panic!("bug!: attempted to assign slot-address"),
            lume_mir::OperandKind::Boolean { .. }
            | lume_mir::OperandKind::Integer { .. }
            | lume_mir::OperandKind::Float { .. }
            | lume_mir::OperandKind::String { .. } => panic!("bug!: attempted to assign literal"),
        }
    }

    fn binary(&mut self, expr: &lume_tir::Binary) -> lume_mir::Operand {
        let lhs = self.expression(&expr.lhs);
        let rhs = self.expression(&expr.rhs);
        let args = vec![lhs, rhs];

        let expr_ty = &expr.lhs.ty;

        let name = if expr_ty.is_integer() {
            let bits = expr_ty.bitwidth();
            let signed = expr_ty.signed();

            match expr.op {
                lume_tir::BinaryOperator::And => lume_mir::Intrinsic::IntAnd { bits, signed },
                lume_tir::BinaryOperator::Or => lume_mir::Intrinsic::IntOr { bits, signed },
                lume_tir::BinaryOperator::Xor => lume_mir::Intrinsic::IntXor { bits, signed },
            }
        } else {
            unreachable!()
        };

        let decl = lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic { name, args }),
            location: expr.location.clone_inner(),
        };

        let reg = self.declare(decl);

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Load { id: reg },
            location: expr.location.clone_inner(),
        }
    }

    fn cast(&mut self, expr: &lume_tir::Cast) -> lume_mir::Operand {
        let expr_ty = &expr.source.ty;
        debug_assert!(expr_ty.is_integer());

        let source = self.expression(&expr.source);
        let operand = self.declare(lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Operand(source)),
            location: expr.location.clone_inner(),
        });

        let decl = lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Cast {
                operand,
                bits: expr.target.bitwidth(),
            }),
            location: expr.location.clone_inner(),
        };

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id: self.declare(decl) },
            location: expr.location.clone_inner(),
        }
    }

    fn construct(&mut self, expr: &lume_tir::Construct) -> lume_mir::Operand {
        let name = self.tcx().new_named_type(&expr.ty, true).unwrap().to_string();
        let props = self.tcx().tdb().find_fields(expr.ty.instance_of).collect::<Vec<_>>();

        // Sort all the constructor expressions so they have the same order as
        // the fields within the type. If this isn't done, the value of a field may be
        // set to the wrong field.
        let mut field_exprs = expr.fields.clone();
        field_exprs.sort_by_key(|field| {
            props
                .iter()
                .enumerate()
                .find_map(|(idx, prop)| {
                    if prop.name.as_str() == field.name.as_str() {
                        Some(idx)
                    } else {
                        None
                    }
                })
                .unwrap()
        });

        let prop_types = props
            .iter()
            .map(|prop| self.lower_type(&prop.field_type))
            .collect::<Vec<_>>();

        // The first type in all object allocations must be a pointer to the metadata
        // of the type, so it can be reflected at runtime.
        let metadata_reg = self.declare_metadata_of(&expr.ty, expr.location.clone_inner());
        let metadata_ptr_size = std::mem::size_of::<*const ()>();

        let prop_sizes = prop_types.iter().map(lume_mir::Type::bytesize).collect::<Vec<_>>();
        let struct_type = lume_mir::Type::structure(name, prop_types);
        let struct_ptr = lume_mir::Type::pointer(struct_type.clone());

        let alloc_reg = self.func.add_register(struct_ptr.clone());
        self.func
            .current_block_mut()
            .allocate(alloc_reg, struct_type, metadata_reg, expr.location.clone_inner());

        // Store the metadata reference in the first element in the structure.
        let metadata_value = lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id: metadata_reg },
            location: expr.location.clone_inner(),
        };

        self.func
            .current_block_mut()
            .store_field(alloc_reg, 0, metadata_value, expr.location.clone_inner());

        // Since the element at offset 0 is the metadata, we start just after it.
        let mut offset = metadata_ptr_size;

        for (field, size) in field_exprs.iter().zip(prop_sizes) {
            let value = self.expression(&field.value);

            self.func
                .current_block_mut()
                .store_field(alloc_reg, offset, value, expr.location.clone_inner());

            offset += size;
        }

        let offset_reg = self.func.declare(struct_ptr, lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic {
                name: lume_mir::Intrinsic::IntAdd {
                    bits: 64,
                    signed: false,
                },
                args: vec![
                    lume_mir::Operand {
                        kind: lume_mir::OperandKind::Reference { id: alloc_reg },
                        location: expr.location.clone_inner(),
                    },
                    lume_mir::Operand {
                        kind: lume_mir::OperandKind::Integer {
                            bits: 64,
                            signed: false,
                            value: lume_mir::POINTER_SIZE.cast_signed() as i64,
                        },
                        location: expr.location.clone_inner(),
                    },
                ],
            }),
            location: expr.location.clone_inner(),
        });

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id: offset_reg },
            location: expr.location.clone_inner(),
        }
    }

    fn call_expression(&mut self, expr: &lume_tir::Call) -> lume_mir::Operand {
        let args = expr
            .arguments
            .iter()
            .map(|arg| self.expression(arg))
            .collect::<Vec<_>>();

        let mut ret_ty = self.lower_type(&expr.return_type);

        if self.tcx().is_type_parameter(&expr.return_type).unwrap() {
            ret_ty = lume_mir::Type::pointer(ret_ty);
        }

        self.call(expr.function, args, ret_ty, expr.location.clone_inner())
    }

    fn is_expression(&mut self, expr: &lume_tir::Is) -> lume_mir::Operand {
        let operand = self.expression(&expr.target);
        let operand_type = self.type_of_value(&operand);

        self.pattern(&expr.pattern, operand, operand_type)
    }

    fn pattern(
        &mut self,
        pattern: &lume_tir::Pattern,
        operand: lume_mir::Operand,
        operand_type: lume_mir::Type,
    ) -> lume_mir::Operand {
        let loaded_op = if let lume_mir::OperandKind::Reference { id } = &operand.kind {
            *id
        } else {
            self.func.declare_value(operand_type, operand.clone())
        };

        match &pattern.kind {
            // Matching against a literal is effectively the same as testing
            // the equality against the operand, so we replace it will an intrinsic call,
            // depending on the type of the operand.
            lume_tir::PatternKind::Literal(lit) => {
                let intrinsic = match &lit.kind {
                    lume_tir::LiteralKind::Boolean(bool) => lume_mir::DeclarationKind::Intrinsic {
                        name: lume_mir::Intrinsic::BooleanEq,
                        args: vec![operand, lume_mir::Operand {
                            kind: lume_mir::OperandKind::Boolean { value: *bool },
                            location: lit.location.clone_inner(),
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
                            location: lit.location.clone_inner(),
                        }],
                    },
                    lume_tir::LiteralKind::Float(float) => lume_mir::DeclarationKind::Intrinsic {
                        name: lume_mir::Intrinsic::FloatEq { bits: float.bits() },
                        args: vec![operand, lume_mir::Operand {
                            kind: lume_mir::OperandKind::Float {
                                bits: float.bits(),
                                value: float.value(),
                            },
                            location: lit.location.clone_inner(),
                        }],
                    },
                    lume_tir::LiteralKind::String(_) => unimplemented!(),
                };

                let result = self.declare(lume_mir::Declaration {
                    kind: Box::new(intrinsic),
                    location: pattern.location.clone_inner(),
                });

                lume_mir::Operand {
                    kind: lume_mir::OperandKind::Reference { id: result },
                    location: pattern.location.clone_inner(),
                }
            }

            // Testing against a variable is effectively a noop, since it will always be true.
            lume_tir::PatternKind::Variable(var) => {
                self.variables.insert(*var, loaded_op);

                lume_mir::Operand {
                    kind: lume_mir::OperandKind::Boolean { value: true },
                    location: pattern.location.clone_inner(),
                }
            }

            lume_tir::PatternKind::Variant(variant) => {
                let discriminant_value = self
                    .tcx()
                    .discriminant_of_variant_ty(variant.ty.instance_of, variant.name.name.name())
                    .unwrap();

                let operand_disc = self.func.declare_raw(lume_mir::Type::u8(), lume_mir::Declaration {
                    kind: Box::new(lume_mir::DeclarationKind::Operand(lume_mir::Operand {
                        kind: lume_mir::OperandKind::LoadField {
                            target: loaded_op,
                            offset: 0,
                            index: 0,
                            field_type: lume_mir::Type::u8(),
                        },
                        location: pattern.location.clone_inner(),
                    })),
                    location: pattern.location.clone_inner(),
                });

                let cmp_intrinsic = lume_mir::DeclarationKind::Intrinsic {
                    name: lume_mir::Intrinsic::IntEq { bits: 8, signed: false },
                    args: vec![
                        lume_mir::Operand {
                            kind: lume_mir::OperandKind::Reference { id: operand_disc },
                            location: pattern.location.clone_inner(),
                        },
                        lume_mir::Operand {
                            kind: lume_mir::OperandKind::Integer {
                                bits: 8,
                                signed: false,
                                value: (discriminant_value as u64).cast_signed(),
                            },
                            location: pattern.location.clone_inner(),
                        },
                    ],
                };

                let mut cmp_result = self.declare(lume_mir::Declaration {
                    kind: Box::new(cmp_intrinsic),
                    location: pattern.location.clone_inner(),
                });

                for (idx, field_pattern) in variant.fields.iter().enumerate() {
                    // We intentionally ignore wildcard patterns, as they are always true and
                    // have no other side effects.
                    if matches!(field_pattern.kind, lume_tir::PatternKind::Wildcard) {
                        continue;
                    }

                    let field_cmp_operand = self.load_variant_subpattern(pattern, loaded_op, field_pattern, idx);

                    let field_cmp_intrinsic = lume_mir::DeclarationKind::Intrinsic {
                        name: lume_mir::Intrinsic::BooleanAnd,
                        args: vec![
                            lume_mir::Operand {
                                kind: lume_mir::OperandKind::Reference { id: cmp_result },
                                location: pattern.location.clone_inner(),
                            },
                            field_cmp_operand,
                        ],
                    };

                    cmp_result = self.declare(lume_mir::Declaration {
                        kind: Box::new(field_cmp_intrinsic),
                        location: field_pattern.location.clone_inner(),
                    });
                }

                lume_mir::Operand {
                    kind: lume_mir::OperandKind::Reference { id: cmp_result },
                    location: pattern.location.clone_inner(),
                }
            }

            // Wildcard patterns are always true, so we implicitly replace it with a `true` expression.
            lume_tir::PatternKind::Wildcard => lume_mir::Operand {
                kind: lume_mir::OperandKind::Boolean { value: true },
                location: pattern.location.clone_inner(),
            },
        }
    }

    /// Loads the given subpattern from some parent pattern into a register and
    /// returns it as an operand.
    fn load_variant_subpattern(
        &mut self,
        parent_pattern: &lume_tir::Pattern,
        parent_operand: RegisterId,
        subpattern: &lume_tir::Pattern,
        field_idx: usize,
    ) -> lume_mir::Operand {
        let field_offset = self.variant_field_offset(parent_pattern.id, field_idx);
        let field_type = self.variant_field_type(parent_pattern.id, field_idx);

        // If the field is a scalar type, it should be loaded directly since any
        // following operations might expect it to be a non-pointer.
        if !field_type.is_reference_type() || field_type.is_generic {
            let field_operand = lume_mir::Operand {
                kind: lume_mir::OperandKind::LoadField {
                    target: parent_operand,
                    offset: field_offset,
                    index: field_idx + 1,
                    field_type: field_type.clone(),
                },
                location: subpattern.location.clone_inner(),
            };

            return self.pattern(subpattern, field_operand, field_type);
        }

        // If the field is a reference type, we define a new register which is equal to
        // the field pointer itself, plus the size of the discriminant.

        let discriminant_size = lume_mir::Type::u8().bytesize();

        let field_operand_ptr = lume_mir::DeclarationKind::Intrinsic {
            name: lume_mir::Intrinsic::IntAdd {
                bits: 64,
                signed: false,
            },
            args: vec![
                lume_mir::Operand {
                    kind: lume_mir::OperandKind::Reference { id: parent_operand },
                    location: parent_pattern.location.clone_inner(),
                },
                lume_mir::Operand {
                    kind: lume_mir::OperandKind::Integer {
                        bits: 64,
                        signed: false,
                        value: discriminant_size.cast_signed() as i64,
                    },
                    location: parent_pattern.location.clone_inner(),
                },
            ],
        };

        let field_operand_reg =
            self.func
                .declare(lume_mir::Type::pointer(lume_mir::Type::void()), lume_mir::Declaration {
                    kind: Box::new(field_operand_ptr),
                    location: parent_pattern.location.clone_inner(),
                });

        let mut field_operand = lume_mir::Operand::reference_of(field_operand_reg);
        field_operand.location = parent_pattern.location.clone_inner();

        self.pattern(
            subpattern,
            field_operand,
            lume_mir::Type::pointer(lume_mir::Type::void()),
        )
    }

    fn intrinsic_call(&mut self, expr: &lume_tir::IntrinsicCall) -> lume_mir::Operand {
        let name = self.intrinsic_of(&expr.kind);
        let args = expr.arguments.iter().map(|arg| self.expression(arg)).collect();

        let decl = lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic { name, args }),
            location: expr.location.clone_inner(),
        };

        let reg = self.declare(decl);

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id: reg },
            location: expr.location.clone_inner(),
        }
    }

    fn if_condition(&mut self, expr: &lume_tir::If) -> lume_mir::Operand {
        let merge_block = if expr.is_returning() {
            None
        } else {
            Some(self.func.new_block())
        };

        let return_reg = if let Some(merge_block) = merge_block
            && let Some(return_type) = expr.return_type.as_ref()
        {
            let expr_ty = self.lower_type(return_type);
            let param_reg = self.func.add_block_parameter(merge_block, expr_ty);

            Some(param_reg)
        } else {
            None
        };

        for case in &expr.cases {
            // Ignore the `else` condition, if present.
            let Some(condition) = case.condition.as_ref() else {
                continue;
            };

            let cond_then = self.func.new_block();
            let cond_else = self.func.new_block();

            self.build_conditional_graph(condition, cond_then, cond_else);

            self.func.set_current_block(cond_then);
            let value = self.block(&case.block);

            if let Some(merge_block) = merge_block {
                let args = if let Some(cond_result) = value
                    && let Some(return_reg) = return_reg
                {
                    let loaded_result = self.load_operand(&cond_result);

                    self.func
                        .current_block_mut()
                        .push_phi_register(loaded_result, return_reg);

                    &[loaded_result][..]
                } else {
                    &[]
                };

                self.func
                    .current_block_mut()
                    .branch_with(merge_block, args, expr.location.clone_inner());
            }

            self.func.set_current_block(cond_else);
        }

        if let Some(else_block) = &expr.else_branch() {
            let value = self.block(&else_block.block);

            if let Some(merge_block) = merge_block {
                let args = if let Some(cond_result) = value
                    && let Some(return_reg) = return_reg
                {
                    let loaded_result = self.load_operand(&cond_result);

                    self.func
                        .current_block_mut()
                        .push_phi_register(loaded_result, return_reg);

                    &[loaded_result][..]
                } else {
                    &[]
                };

                self.func
                    .current_block_mut()
                    .branch_with(merge_block, args, expr.location.clone_inner());
            }
        }

        if let Some(merge_block) = merge_block {
            self.func
                .current_block_mut()
                .branch(merge_block, expr.location.clone_inner());

            self.func.set_current_block(merge_block);
        }

        if let Some(return_reg) = return_reg {
            lume_mir::Operand {
                kind: lume_mir::OperandKind::Reference { id: return_reg },
                location: expr.location.clone_inner(),
            }
        } else {
            self.null_operand()
        }
    }

    fn build_conditional_graph(
        &mut self,
        expr: &lume_tir::Expression,
        then_block: lume_mir::BasicBlockId,
        else_block: lume_mir::BasicBlockId,
    ) {
        if let lume_tir::ExpressionKind::Logical(comp_expr) = &expr.kind {
            match comp_expr.op {
                // Build graph for logical AND expressions
                //
                // For example, given an expression such as `x < 10 && x > 5`, the graph
                // would be visualized as:
                //
                //      BB0
                //     x < 10?
                //    /      \
                //  false    BB1
                //          x > 5?
                //         /     \
                //       false   true
                //
                // where `BB0` is the entry block and `BB1` is an intermediary block.
                lume_tir::LogicalOperator::And => {
                    let inter_block = self.func.new_block();

                    let lhs_val = self.expression(&comp_expr.lhs);
                    let lhs_expr = self.func.declare_value(lume_mir::Type::boolean(), lhs_val);

                    self.func.current_block_mut().conditional_branch(
                        lhs_expr,
                        inter_block,
                        else_block,
                        expr.location().clone_inner(),
                    );

                    self.func.set_current_block(inter_block);

                    let rhs_val = self.expression(&comp_expr.rhs);
                    let rhs_expr = self.func.declare_value(lume_mir::Type::boolean(), rhs_val);

                    self.func.current_block_mut().conditional_branch(
                        rhs_expr,
                        then_block,
                        else_block,
                        expr.location().clone_inner(),
                    );
                }

                // Build graph for logical OR expressions
                //
                // For example, given an expression such as `x < 10 || y < 10`, the graph
                // would be visualized as:
                //
                //      BB0
                //     x < 10?
                //    /      \
                //  true     BB1
                //          y < 10?
                //         /      \
                //       true     false
                //
                // where `BB0` is the entry block and `BB1` is an intermediary block.
                lume_tir::LogicalOperator::Or => {
                    let inter_block = self.func.new_block();

                    let lhs_val = self.expression(&comp_expr.lhs);
                    let lhs_expr = self.func.declare_value(lume_mir::Type::boolean(), lhs_val);

                    self.func.current_block_mut().conditional_branch(
                        lhs_expr,
                        then_block,
                        inter_block,
                        expr.location().clone_inner(),
                    );

                    self.func.set_current_block(inter_block);

                    let rhs_val = self.expression(&comp_expr.rhs);
                    let rhs_expr = self.func.declare_value(lume_mir::Type::boolean(), rhs_val);

                    self.func.current_block_mut().conditional_branch(
                        rhs_expr,
                        then_block,
                        else_block,
                        expr.location().clone_inner(),
                    );
                }
            }
        } else {
            let cond_val = self.expression(expr);
            let cond_expr = self.func.declare_value(lume_mir::Type::boolean(), cond_val);

            self.func.current_block_mut().conditional_branch(
                cond_expr,
                then_block,
                else_block,
                expr.location().clone_inner(),
            );
        }
    }

    fn intrinsic_of(&self, expr: &lume_tir::IntrinsicKind) -> lume_mir::Intrinsic {
        match expr {
            lume_tir::IntrinsicKind::FloatEq { bits } => lume_mir::Intrinsic::FloatEq { bits: *bits },
            lume_tir::IntrinsicKind::FloatNe { bits } => lume_mir::Intrinsic::FloatNe { bits: *bits },
            lume_tir::IntrinsicKind::FloatGe { bits } => lume_mir::Intrinsic::FloatGe { bits: *bits },
            lume_tir::IntrinsicKind::FloatGt { bits } => lume_mir::Intrinsic::FloatGt { bits: *bits },
            lume_tir::IntrinsicKind::FloatLe { bits } => lume_mir::Intrinsic::FloatLe { bits: *bits },
            lume_tir::IntrinsicKind::FloatLt { bits } => lume_mir::Intrinsic::FloatLt { bits: *bits },
            lume_tir::IntrinsicKind::FloatAdd { bits } => lume_mir::Intrinsic::FloatAdd { bits: *bits },
            lume_tir::IntrinsicKind::FloatSub { bits } => lume_mir::Intrinsic::FloatSub { bits: *bits },
            lume_tir::IntrinsicKind::FloatMul { bits } => lume_mir::Intrinsic::FloatMul { bits: *bits },
            lume_tir::IntrinsicKind::FloatDiv { bits } => lume_mir::Intrinsic::FloatDiv { bits: *bits },
            lume_tir::IntrinsicKind::FloatNegate { bits } => lume_mir::Intrinsic::FloatNegate { bits: *bits },
            lume_tir::IntrinsicKind::IntEq { bits, signed } => lume_mir::Intrinsic::IntEq {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntNe { bits, signed } => lume_mir::Intrinsic::IntNe {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntGe { bits, signed } => lume_mir::Intrinsic::IntGe {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntGt { bits, signed } => lume_mir::Intrinsic::IntGt {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntLe { bits, signed } => lume_mir::Intrinsic::IntLe {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntLt { bits, signed } => lume_mir::Intrinsic::IntLt {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntAdd { bits, signed } => lume_mir::Intrinsic::IntAdd {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntSub { bits, signed } => lume_mir::Intrinsic::IntSub {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntMul { bits, signed } => lume_mir::Intrinsic::IntMul {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntDiv { bits, signed } => lume_mir::Intrinsic::IntDiv {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntAnd { bits, signed } => lume_mir::Intrinsic::IntAnd {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntOr { bits, signed } => lume_mir::Intrinsic::IntOr {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntXor { bits, signed } => lume_mir::Intrinsic::IntXor {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntNegate { bits, signed } => lume_mir::Intrinsic::IntNegate {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::BooleanEq => lume_mir::Intrinsic::BooleanEq,
            lume_tir::IntrinsicKind::BooleanNe => lume_mir::Intrinsic::BooleanNe,
            lume_tir::IntrinsicKind::BooleanAnd => lume_mir::Intrinsic::BooleanAnd,
            lume_tir::IntrinsicKind::BooleanOr => lume_mir::Intrinsic::BooleanOr,
            lume_tir::IntrinsicKind::BooleanNot => lume_mir::Intrinsic::BooleanNot,
            lume_tir::IntrinsicKind::Metadata { id } => {
                let metadata_store = &self.transformer.mcx.mir().metadata.metadata;
                let metadata_entry = metadata_store.get(id).unwrap();

                lume_mir::Intrinsic::Metadata {
                    metadata: Box::new(metadata_entry.clone()),
                }
            }
        }
    }

    fn logical(&mut self, expr: &lume_tir::Logical) -> lume_mir::Operand {
        let lhs = self.expression(&expr.lhs);
        let rhs = self.expression(&expr.rhs);
        let args = vec![lhs, rhs];

        let name = match expr.op {
            lume_tir::LogicalOperator::And => lume_mir::Intrinsic::BooleanAnd,
            lume_tir::LogicalOperator::Or => lume_mir::Intrinsic::BooleanOr,
        };

        let decl = lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic { name, args }),
            location: expr.location.clone_inner(),
        };

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id: self.declare(decl) },
            location: expr.location.clone_inner(),
        }
    }

    fn member(&mut self, expr: &lume_tir::Member) -> lume_mir::Operand {
        let target_val = self.expression(&expr.callee);
        let target_reg = self.load_operand(&target_val);

        let index = expr.field.index;
        let offset = self.field_offset(&expr.field);
        let field_type = self.field_type(&expr.field);

        lume_mir::Operand {
            kind: lume_mir::OperandKind::LoadField {
                target: target_reg,
                offset,
                index,
                field_type,
            },
            location: expr.location.clone_inner(),
        }
    }

    fn scope(&mut self, expr: &lume_tir::Scope) -> lume_mir::Operand {
        let mut val: Option<lume_mir::Operand> = None;

        for stmt in &expr.body {
            val = self.statement(stmt);
        }

        val.unwrap_or_else(|| self.null_operand())
    }

    fn switch(&mut self, expr: &lume_tir::Switch) -> lume_mir::Operand {
        let location = expr.location;
        let mut arms = Vec::new();

        let entry_block = self.func.current_block().id;
        let merge_block = self.func.new_block();

        self.func.set_current_block(entry_block);

        let operand_val_orig = self.expression(&expr.operand);
        let operand_val = if expr.operand.ty.is_float() {
            let source = self.load_operand(&operand_val_orig);
            let target = match expr.operand.ty.bitwidth() {
                32 => lume_mir::Type::i32(),
                64 => lume_mir::Type::i64(),
                _ => unreachable!(),
            };

            lume_mir::Operand {
                kind: lume_mir::OperandKind::Bitcast { source, target },
                location: location.clone_inner(),
            }
        } else {
            self.expression(&expr.operand)
        };

        let operand_reg = self.load_operand(&operand_val);

        self.variables.insert(expr.operand_var, operand_reg);

        let value_ty = self.lower_type(&expr.fallback.ty);
        let value_slot = self.func.alloc_slot(value_ty, location.clone_inner());

        for (pattern, branch) in &expr.entries {
            let block = self.func.new_active_block();

            let arm_pattern = match pattern {
                lume_tir::SwitchConstantPattern::Literal(lit) => match lit {
                    lume_tir::SwitchConstantLiteral::Boolean(lit) => i64::from(*lit),
                    lume_tir::SwitchConstantLiteral::Float(lit) => lit.to_bits().cast_signed(),
                    lume_tir::SwitchConstantLiteral::Integer(lit) => *lit,
                },
                lume_tir::SwitchConstantPattern::Variable(_) => todo!(),
            };

            let value = self.expression(branch);
            self.func
                .current_block_mut()
                .store_slot(value_slot, 0, value, location.clone_inner());
            self.func
                .current_block_mut()
                .branch(merge_block, location.clone_inner());

            arms.push((arm_pattern, lume_mir::BlockBranchSite::new(block)));
        }

        let fallback_block = self.func.new_active_block();

        let value = self.expression(&expr.fallback);
        self.func
            .current_block_mut()
            .store_slot(value_slot, 0, value, location.clone_inner());
        self.func
            .current_block_mut()
            .branch(merge_block, location.clone_inner());

        let fallback = lume_mir::BlockBranchSite::new(fallback_block);

        self.func.set_current_block(entry_block);
        self.func.current_block_mut().set_terminator(lume_mir::Terminator {
            kind: lume_mir::TerminatorKind::Switch {
                operand: operand_reg,
                arms,
                fallback,
            },
            location: location.clone_inner(),
        });

        self.func.set_current_block(merge_block);

        let slot_addr = self.declare_value(lume_mir::Operand {
            kind: lume_mir::OperandKind::SlotAddress {
                id: value_slot,
                offset: 0,
            },
            location: location.clone_inner(),
        });

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Load { id: slot_addr },
            location: location.clone_inner(),
        }
    }

    fn variable_reference(&mut self, expr: &lume_tir::VariableReference) -> lume_mir::Operand {
        let id = match &expr.source {
            lume_tir::VariableSource::Parameter => lume_mir::RegisterId::new(expr.reference.0),
            lume_tir::VariableSource::Variable => *self.variables.get(&expr.reference).unwrap(),
        };

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id },
            location: expr.location.clone_inner(),
        }
    }

    fn variant(&mut self, expr: &lume_tir::Variant) -> lume_mir::Operand {
        let discriminant = lume_mir::Operand {
            kind: lume_mir::OperandKind::Integer {
                bits: 8,
                signed: false,
                value: i64::from(expr.index),
            },
            location: expr.location.clone_inner(),
        };

        let enum_ty = self.tcx().type_of(expr.id).unwrap();
        let enum_def = self.tcx().enum_def_type(enum_ty.instance_of).unwrap();
        let enum_def_id = enum_def.id;

        // The first type in all object allocations must be a pointer to the metadata
        // of the type, so it can be reflected at runtime.
        let metadata_reg = self.declare_metadata_of(&expr.ty, expr.location.clone_inner());
        let metadata_ptr_size = std::mem::size_of::<*const ()>();

        let mut union_cases = Vec::new();

        for variant in self.tcx().enum_cases_expr(expr.id).unwrap() {
            let mut items = Vec::new();

            let params = &variant.parameters;
            let case_refs = self.tcx().mk_type_refs_from(params, enum_def_id).unwrap();

            union_cases.reserve_exact(case_refs.len());
            for case_ref in case_refs {
                items.push(self.lower_type(&case_ref));
            }

            union_cases.push(lume_mir::Type::tuple(items));
        }

        let union_type = lume_mir::Type::union(union_cases);
        let alloc_reg = self.func.add_register(union_type.clone());
        self.func.current_block_mut().allocate(
            alloc_reg,
            union_type.clone(),
            metadata_reg,
            expr.location.clone_inner(),
        );

        // Store the metadata reference in the first element in the union.
        let metadata_value = lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id: metadata_reg },
            location: expr.location.clone_inner(),
        };

        self.func
            .current_block_mut()
            .store_field(alloc_reg, 0, metadata_value, expr.location.clone_inner());

        // Since the element at offset 0 is the metadata, we start just after it.
        let mut offset = metadata_ptr_size;

        // Store the discriminant of the variant right after the metadata
        self.func
            .current_block_mut()
            .store_field(alloc_reg, offset, discriminant, expr.location.clone_inner());

        offset += 1;

        for argument in &expr.arguments {
            let value = self.expression(argument);
            let operand_size = usize::from(value.bitsize().div_ceil(8));

            self.func
                .current_block_mut()
                .store_field(alloc_reg, offset, value, expr.location.clone_inner());

            offset += operand_size;
        }

        let offset_reg = self.func.declare(union_type, lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic {
                name: lume_mir::Intrinsic::IntAdd {
                    bits: 64,
                    signed: false,
                },
                args: vec![
                    lume_mir::Operand {
                        kind: lume_mir::OperandKind::Reference { id: alloc_reg },
                        location: expr.location.clone_inner(),
                    },
                    lume_mir::Operand {
                        kind: lume_mir::OperandKind::Integer {
                            bits: 64,
                            signed: false,
                            value: lume_mir::POINTER_SIZE.cast_signed() as i64,
                        },
                        location: expr.location.clone_inner(),
                    },
                ],
            }),
            location: expr.location.clone_inner(),
        });

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id: offset_reg },
            location: expr.location.clone_inner(),
        }
    }

    #[expect(clippy::unused_self)]
    fn literal(&self, expr: &lume_tir::LiteralKind, location: Location) -> lume_mir::Operand {
        match expr {
            lume_tir::LiteralKind::Boolean(val) => lume_mir::Operand {
                kind: lume_mir::OperandKind::Boolean { value: *val },
                location: location.clone_inner(),
            },
            lume_tir::LiteralKind::Int(val) => {
                let (bits, signed, value) = match val {
                    lume_tir::IntLiteral::I8(val) => (8, true, *val),
                    lume_tir::IntLiteral::I16(val) => (16, true, *val),
                    lume_tir::IntLiteral::I32(val) => (32, true, *val),
                    lume_tir::IntLiteral::I64(val) => (64, true, *val),
                    lume_tir::IntLiteral::U8(val) => (8, false, *val),
                    lume_tir::IntLiteral::U16(val) => (16, false, *val),
                    lume_tir::IntLiteral::U32(val) => (32, false, *val),
                    lume_tir::IntLiteral::U64(val) => (64, false, *val),
                };

                lume_mir::Operand {
                    kind: lume_mir::OperandKind::Integer { value, bits, signed },
                    location: location.clone_inner(),
                }
            }
            lume_tir::LiteralKind::Float(val) => {
                let (bits, value) = match val {
                    lume_tir::FloatLiteral::F32(val) => (32, *val),
                    lume_tir::FloatLiteral::F64(val) => (64, *val),
                };

                lume_mir::Operand {
                    kind: lume_mir::OperandKind::Float { value, bits },
                    location: location.clone_inner(),
                }
            }
            lume_tir::LiteralKind::String(val) => lume_mir::Operand {
                kind: lume_mir::OperandKind::String { value: *val },
                location: location.clone_inner(),
            },
        }
    }

    fn load_operand(&mut self, op: &lume_mir::Operand) -> lume_mir::RegisterId {
        if let lume_mir::OperandKind::Reference { id } = &op.kind {
            *id
        } else {
            self.declare_value(op.clone())
        }
    }

    fn field_offset(&self, field: &lume_types::Field) -> usize {
        let mut offset = 0;

        for (idx, prop) in self.tcx().tdb().find_fields(field.owner).enumerate() {
            if idx == field.index {
                break;
            }

            let prop_ty = self.lower_type(&prop.field_type);
            offset += prop_ty.bytesize();
        }

        offset
    }

    fn field_type(&self, field: &lume_types::Field) -> lume_mir::Type {
        for (idx, prop) in self.tcx().tdb().find_fields(field.owner).enumerate() {
            if idx == field.index {
                return self.lower_type(&prop.field_type);
            }
        }

        panic!("bug!: field index of {} is out of bounds", field.index)
    }

    fn variant_field_type(&self, id: lume_span::NodeId, field_idx: usize) -> lume_mir::Type {
        let pattern = self.tcx().hir_expect_pattern(id);
        let lume_hir::PatternKind::Variant(variant_pattern) = &pattern.kind else {
            panic!("bug!: attempting to get field offset of non-variant pattern");
        };

        let field_type = self
            .tcx()
            .type_of_variant_field(&variant_pattern.name, field_idx)
            .unwrap();

        self.lower_type(&field_type)
    }

    fn variant_field_offset(&self, id: lume_span::NodeId, field_idx: usize) -> usize {
        // We start off with the size of the discriminant of the variant.
        let mut offset = lume_mir::Type::u8().bytesize();

        let pattern = self.tcx().hir_expect_pattern(id);
        let lume_hir::PatternKind::Variant(variant_pattern) = &pattern.kind else {
            panic!("bug!: attempting to get field offset of non-variant pattern");
        };

        let enum_case = self.tcx().enum_case_with_name(&variant_pattern.name).unwrap();

        for (idx, field_type) in enum_case.parameters.iter().enumerate() {
            if idx == field_idx {
                break;
            }

            let field_type_ty = self.tcx().mk_type_ref_from(field_type, id).unwrap();
            let prop_ty = self.lower_type(&field_type_ty);
            offset += prop_ty.bytesize();
        }

        offset
    }
}
