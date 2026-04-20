use lume_mir::{BasicBlockId, Instance, RegisterId};
use lume_span::Location;

use crate::builder::Builder;
use crate::builder::decl::OperandRef;
use crate::builder::pattern::pattern;
use crate::dynamic::DynamicShimBuilder;

pub(crate) fn lower_function(mut builder: Builder<'_, '_>, func: &lume_tir::Function) -> lume_mir::Function {
    match func.kind {
        lume_tir::FunctionKind::Static | lume_tir::FunctionKind::Dropper => {
            if let Some(body) = &func.block {
                builder.with_new_block(|builder, _| {
                    declare_parameters(builder, func);
                    lower_block(builder, body);
                });

                builder.run_passes();
            }
        }
        lume_tir::FunctionKind::Dynamic => {
            builder.with_new_block(|builder, _| {
                declare_parameters(builder, func);

                DynamicShimBuilder::new(builder, func).build();
            });

            builder.run_passes();
        }
        lume_tir::FunctionKind::Intrinsic => unreachable!(),
    }

    builder.func
}

/// Declares all parameters of the function as valid variables within the
/// function.
pub(crate) fn declare_parameters(builder: &mut Builder<'_, '_>, func: &lume_tir::Function) {
    let entry_block = builder.func.current_block().id;

    let zipped_parameters = func
        .parameters
        .iter()
        .zip(builder.func.parameters())
        .map(|(param, reg)| (param.var, reg.id))
        .collect::<Vec<_>>();

    for (variable, register) in zipped_parameters {
        builder.declare_var(entry_block, variable, register);
    }
}

pub(crate) fn lower_block(builder: &mut Builder<'_, '_>, body: &lume_tir::Block) {
    let return_value = block(builder, body);

    // Only declare the return value, if the block actually expects a
    // non-void return type.
    if body.has_return_value()
        && let Some(return_value) = return_value
    {
        let location = return_value.location;

        builder.return_(Some(return_value), location);
    }

    // If the current block is not returning, add a return statement so
    // there's always a valid return value.
    //
    // We're assuming it'll be a void return, since the type checker should
    // have detected a missing return statement in a non-void function.
    let ret_loc = body.statements.last().map_or(Location::empty(), |stmt| stmt.location());

    builder.return_(None, ret_loc);
}

fn block(builder: &mut Builder<'_, '_>, block: &lume_tir::Block) -> Option<lume_mir::Operand> {
    let mut val = None;

    for stmt in &block.statements {
        val = statement(builder, stmt);
    }

    val
}

fn statement(builder: &mut Builder<'_, '_>, stmt: &lume_tir::Statement) -> Option<lume_mir::Operand> {
    match stmt {
        lume_tir::Statement::Variable(stmt) => {
            variable_declaration(builder, stmt);
        }
        lume_tir::Statement::Break(stmt) => {
            break_loop(builder, stmt);
        }
        lume_tir::Statement::Continue(stmt) => {
            continue_loop(builder, stmt);
        }
        lume_tir::Statement::Final(stmt) => return Some(final_value(builder, stmt)),
        lume_tir::Statement::Return(stmt) => {
            return_value(builder, stmt);
        }
        lume_tir::Statement::InfiniteLoop(stmt) => {
            infinite_loop(builder, stmt);
        }
        lume_tir::Statement::Expression(expr) => return Some(expression(builder, expr)),
    }

    None
}

fn variable_declaration(builder: &mut Builder<'_, '_>, decl: &lume_tir::VariableDeclaration) {
    builder.with_current_block(|builder, _| {
        let (register, _) = builder.use_value(&decl.value);
        let block = builder.func.register_block(register).unwrap();

        builder.declare_var(block, decl.var, register);
    });
}

fn break_loop(builder: &mut Builder<'_, '_>, stmt: &lume_tir::Break) {
    builder.with_current_block(|builder, _| builder.break_out_of_loop(stmt.location));
}

fn continue_loop(builder: &mut Builder<'_, '_>, stmt: &lume_tir::Continue) {
    builder.with_current_block(|builder, _| builder.continue_loop(stmt.location));
}

fn final_value(builder: &mut Builder<'_, '_>, final_value: &lume_tir::Final) -> lume_mir::Operand {
    builder.with_current_block(|builder, _| {
        let (value, _) = builder.use_value(&final_value.value);

        builder.use_register(value, final_value.location)
    })
}

fn return_value(builder: &mut Builder<'_, '_>, stmt: &lume_tir::Return) {
    builder.with_current_block(|builder, _| match &stmt.value {
        Some(value) => {
            let (_, value_operand) = builder.use_value(value);

            builder.return_(Some(value_operand), stmt.location);
        }
        None => {
            builder.return_(None, stmt.location);
        }
    });
}

fn infinite_loop(builder: &mut Builder<'_, '_>, stmt: &lume_tir::InfiniteLoop) {
    let body_block = builder.new_block();

    let merge_block = if stmt.is_returning() {
        None
    } else {
        Some(builder.new_block())
    };

    builder.with_current_block(|builder, _| {
        builder.branch_to(body_block, stmt.location);
    });

    builder.in_loop_scope(body_block, merge_block, |builder, _| {
        builder.with_block(body_block, |builder, _| {
            // Expand all the statements in the loop body
            block(builder, &stmt.block);

            // Loop back to the start of the loop body
            builder.branch_to(body_block, stmt.location);
        });

        if let Some(merge_block) = merge_block {
            // If we created any new blocks from the loop body, branch to the merge block
            builder.branch_to(merge_block, stmt.location);

            // Place any following statements into the merge block.
            builder.func.set_current_block(merge_block);
        }
    });
}

pub(crate) fn expression(builder: &mut Builder<'_, '_>, expr: &lume_tir::Expression) -> lume_mir::Operand {
    let op = match &expr.kind {
        lume_tir::ExpressionKind::Assignment(expr) => assignment(builder, expr),
        lume_tir::ExpressionKind::Binary(expr) => binary(builder, expr),
        lume_tir::ExpressionKind::Bitcast(expr) => bitcast(builder, expr),
        lume_tir::ExpressionKind::Construct(expr) => construct(builder, expr),
        lume_tir::ExpressionKind::Call(expr) => call_expression(builder, expr),
        lume_tir::ExpressionKind::Ref(expr) => ref_expression(builder, expr),
        lume_tir::ExpressionKind::Deref(expr) => deref_expression(builder, expr),
        lume_tir::ExpressionKind::If(expr) => if_condition(builder, expr),
        lume_tir::ExpressionKind::Is(expr) => is_condition(builder, expr),
        lume_tir::ExpressionKind::IntrinsicCall(expr) => intrinsic_expression(builder, expr),
        lume_tir::ExpressionKind::Literal(expr) => literal(builder, expr),
        lume_tir::ExpressionKind::Logical(expr) => logical(builder, expr),
        lume_tir::ExpressionKind::Member(expr) => member_field(builder, expr),
        lume_tir::ExpressionKind::Scope(expr) => scope(builder, expr),
        lume_tir::ExpressionKind::Switch(expr) => switch(builder, expr),
        lume_tir::ExpressionKind::Variable(expr) => variable_reference(builder, expr),
        lume_tir::ExpressionKind::Variant(expr) => variant_expression(builder, expr),
    };

    builder.unbox_value_if_needed(op, &expr.ty)
}

fn assignment(builder: &mut Builder<'_, '_>, expr: &lume_tir::Assignment) -> lume_mir::Operand {
    builder.with_current_block(|builder, _| {
        let (_, target_operand) = builder.use_value(&expr.target);
        let (_, value_operand) = builder.use_value(&expr.value);

        match &target_operand.kind {
            lume_mir::OperandKind::Reference { id } => {
                let reassign_id = builder.replace_register(*id, value_operand);

                lume_mir::Operand {
                    kind: lume_mir::OperandKind::Reference { id: reassign_id },
                    location: expr.location,
                }
            }
            lume_mir::OperandKind::Load { id, loaded_type } => {
                let id = builder.func.moved_register(*id);
                builder.store(id, value_operand, expr.location);

                lume_mir::Operand {
                    kind: lume_mir::OperandKind::Load {
                        id,
                        loaded_type: loaded_type.clone(),
                    },
                    location: expr.location,
                }
            }
            lume_mir::OperandKind::LoadField { target, offset, .. } => {
                let target = builder.func.moved_register(*target);
                builder.store_field(target, value_operand, *offset, expr.location);

                target_operand
            }
            lume_mir::OperandKind::LoadSlot { target, offset, .. } => {
                builder.store_slot(*target, value_operand, *offset, expr.location);

                target_operand
            }
            lume_mir::OperandKind::Untagged { .. } => panic!("bug!: attempted to assign untagged pointer"),
            lume_mir::OperandKind::Bitcast { .. } => panic!("bug!: attempted to assign bitcast"),
            lume_mir::OperandKind::SlotAddress { .. } => panic!("bug!: attempted to assign slot-address"),
            lume_mir::OperandKind::Boolean { .. }
            | lume_mir::OperandKind::Integer { .. }
            | lume_mir::OperandKind::Float { .. }
            | lume_mir::OperandKind::String { .. } => panic!("bug!: attempted to assign literal"),
        }
    })
}

fn binary(builder: &mut Builder<'_, '_>, expr: &lume_tir::Binary) -> lume_mir::Operand {
    builder.with_current_block(|builder, _| {
        let bits = expr.lhs.ty.bitwidth();
        let signed = expr.lhs.ty.signed();

        let name = match expr.op {
            lume_tir::BinaryOperator::And => lume_mir::Intrinsic::IntAnd { bits, signed },
            lume_tir::BinaryOperator::Or => lume_mir::Intrinsic::IntOr { bits, signed },
            lume_tir::BinaryOperator::Xor => lume_mir::Intrinsic::IntXor { bits, signed },
        };

        let args = vec![builder.use_value(&expr.lhs).1, builder.use_value(&expr.rhs).1];

        let return_value = builder.declare(lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic { name, args }),
            location: expr.location,
        });

        builder.use_register(return_value, expr.location)
    })
}

fn bitcast(builder: &mut Builder<'_, '_>, expr: &lume_tir::Bitcast) -> lume_mir::Operand {
    builder.with_current_block(|builder, _| {
        let operand = builder.use_value(&expr.source).0;
        let bits = expr.target.bitwidth();

        let result = builder.bitcast(operand, bits, expr.location);
        builder.use_register(result, expr.location)
    })
}

fn construct(builder: &mut Builder<'_, '_>, expr: &lume_tir::Construct) -> lume_mir::Operand {
    builder.with_current_block(|builder, _| {
        let struct_name = builder.tcx().new_named_type(&expr.ty, true).unwrap();
        let type_fields = builder.tcx().fields_on(expr.ty.instance_of).unwrap();

        // Sort all the constructor expressions so they have the same order as
        // the fields within the type. If this isn't done, the value of a field may be
        // set to the wrong field.
        let mut field_exprs = expr.fields.clone();

        #[allow(unused_variables, reason = "rustc lint bug")]
        field_exprs.sort_by_key(|field| {
            type_fields
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

        let field_types = type_fields
            .iter()
            .map(|field| {
                let field_type = builder
                    .tcx()
                    .mk_type_ref_from(&field.field_type, expr.ty.instance_of)
                    .unwrap();

                builder.lower_type(&field_type)
            })
            .collect::<Vec<_>>();

        let prop_sizes = field_types.iter().map(lume_mir::Type::bytesize).collect::<Vec<_>>();
        let struct_type = lume_mir::Type::structure(struct_name, field_types);

        let struct_alloc_reg = builder.alloca(struct_type.clone(), &expr.ty, expr.location);
        let struct_untagged_op = builder.declare_untagged(struct_alloc_reg);

        let mut offset = 0;
        for (field, size) in field_exprs.iter().zip(prop_sizes) {
            let (value_reg, _) = builder.use_value(&field.value);
            let value_op = builder.use_register(value_reg, field.value.location());

            builder.store_field(struct_untagged_op, value_op, offset, expr.location);

            offset += size;
        }

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id: struct_alloc_reg },
            location: expr.location,
        }
    })
}

fn call_expression(builder: &mut Builder<'_, '_>, expr: &lume_tir::Call) -> lume_mir::Operand {
    builder.with_current_block(|builder, _| {
        let is_ffi_call = builder.tcx().hir_is_callable_external(expr.function);
        let call_instance = Instance::from(expr.function);

        let mut signature = builder.signature_of(&call_instance);
        signature.return_type = builder.lower_type(&expr.return_type);

        let return_type = expr.uninst_return_type.as_ref().unwrap_or(&expr.return_type);
        if builder.tcx().is_type_parameter(return_type) {
            signature.return_type = lume_mir::Type::boxed(signature.return_type);
        }

        let mut call_arguments = Vec::with_capacity(expr.arguments.len());
        for (idx, argument) in expr.arguments.iter().enumerate() {
            let mut arg_operand = expression(builder, argument);

            let Some(param) = signature.parameters.get(idx) else {
                call_arguments.push(arg_operand);
                break;
            };

            // If the argument is passed to an FFI function or may otherwise be used as a
            // direct memory pointer, the pointer must be untagged.
            //
            // If the argument is passed whilst tagged, the pointer would point to an
            // incorrect memory location.
            if is_ffi_call && builder.type_of_value(&arg_operand).requires_stack_map() {
                let arg_register = builder.declare_operand(arg_operand, OperandRef::Implicit);

                arg_operand = lume_mir::Operand::untagged_of(arg_register);
            }

            // Generic parameters are lowering into accepting pointer types, so all
            // types of argument can be passed.
            //
            // When passing a non-reference argument into a generic parameter, we then
            // need to pass an address to the argument, so the callee can load it. When
            // lowering these arguments, we create a slot in the stack to store the
            // argument, then we pass the address of the stack slot to the function.
            arg_operand = builder.box_value_if_needed(arg_operand, &argument.ty, &param.type_ref);

            call_arguments.push(arg_operand);
        }

        let return_value = builder.call_with_signature(
            call_instance,
            &signature,
            expr.type_arguments.clone(),
            call_arguments,
            expr.location,
        );

        builder.use_register(return_value, expr.location)
    })
}

fn ref_expression(builder: &mut Builder<'_, '_>, expr: &lume_tir::Ref) -> lume_mir::Operand {
    builder.with_current_block(|builder, _| {
        let expr_type = builder.tcx().type_of(expr.id).unwrap();
        let (_, target_op) = builder.use_value(&expr.target);

        builder.store_on_stack(target_op, &expr_type, expr.location)
    })
}

fn deref_expression(builder: &mut Builder<'_, '_>, expr: &lume_tir::Deref) -> lume_mir::Operand {
    builder.with_current_block(|builder, _| match &expr.op {
        lume_tir::DerefOp::Read { target, type_ } => {
            let (target_id, _) = builder.use_value(target);

            let loaded_type = builder.lower_type(type_);
            let loaded_id = builder.load_as(loaded_type, target_id, expr.location);

            lume_mir::Operand {
                kind: lume_mir::OperandKind::Reference { id: loaded_id },
                location: expr.location,
            }
        }
        lume_tir::DerefOp::Write { target, value } => {
            let (target_id, target_op) = builder.use_value(target);
            let (_, value_op) = builder.use_value(value);

            builder.store(target_id, value_op, expr.location);

            target_op
        }
    })
}

fn if_condition(builder: &mut Builder<'_, '_>, expr: &lume_tir::If) -> lume_mir::Operand {
    fn branch_to_merge(
        builder: &mut Builder<'_, '_>,
        merge_block: BasicBlockId,
        merge_param: Option<RegisterId>,
        merge_value: Option<lume_mir::Operand>,
        location: Location,
    ) {
        let args = if let Some(merge_value) = merge_value
            && let Some(merge_param) = merge_param
        {
            let loaded_result = builder.declare_operand(merge_value, OperandRef::Implicit);

            builder
                .func
                .current_block_mut()
                .push_phi_register(loaded_result, merge_param);

            &[loaded_result][..]
        } else {
            &[]
        };

        builder.branch_with(merge_block, args, location);
    }

    builder.with_current_block(|builder, _| {
        let merge_block = if expr.is_returning() {
            None
        } else {
            Some(builder.new_block())
        };

        let merge_parameter = if let Some(merge_block) = merge_block
            && builder.tcx().does_type_have_value(&expr.return_type)
        {
            let expr_ty = builder.lower_type(&expr.return_type);
            let parameter = builder.func.add_block_parameter(merge_block, expr_ty);

            Some(parameter)
        } else {
            None
        };

        for case in &expr.cases {
            // Ignore the `else` condition, if present.
            let Some(condition) = case.condition.as_ref() else {
                continue;
            };

            let cond_then = builder.new_block();
            let cond_else = builder.new_block();

            builder.build_conditional_graph(condition, cond_then, cond_else);

            builder.with_block(cond_then, |builder, _| {
                let value = block(builder, &case.block);

                if let Some(merge_block) = merge_block {
                    branch_to_merge(builder, merge_block, merge_parameter, value, expr.location);
                }
            });

            // Whether the next expression is the `else` block, the merge block or just the
            // next condition case, place it inside the `false` branch block.
            builder.func.set_current_block(cond_else);
        }

        if let Some(else_block) = &expr.else_branch() {
            // Note the lack of `builder.with_block`:
            //
            // Since we used `set_current_block` at the end of every non-else arm of the
            // conditional, we're ensured to already have a valid block to
            // place statements in.
            let value = block(builder, &else_block.block);

            if let Some(merge_block) = merge_block {
                branch_to_merge(builder, merge_block, merge_parameter, value, expr.location);
            }
        }

        if let Some(merge_block) = merge_block {
            // Ensure the current block, whatever it may be, also branches to the merge
            // block.
            //
            // `branch_to` doesn't overwrite the terminator, so if the block already
            // branches, it will do nothing.
            builder.branch_to(merge_block, expr.location);

            // Place all the statements following the `if` expression into the merge block.
            builder.func.set_current_block(merge_block);
        }

        if let Some(merge_parameter) = merge_parameter {
            builder.use_register(merge_parameter, expr.location)
        } else {
            builder.null_const()
        }
    })
}

fn intrinsic_expression(builder: &mut Builder<'_, '_>, expr: &lume_tir::IntrinsicCall) -> lume_mir::Operand {
    builder.with_current_block(|builder, _| {
        // Niche for metadata intrinsics - tries to prevent cluttering up the MIR
        // with metadata intrinsics.
        if let lume_tir::IntrinsicKind::Metadata { id } = &expr.kind {
            let metadata_reg = builder.declare_metadata_with_id(*id, expr.location);

            return builder.use_register(metadata_reg, expr.location);
        }

        let name = builder.intrinsic_of(&expr.kind);

        let mut call_arguments = Vec::with_capacity(expr.arguments.len());
        for argument in &expr.arguments {
            call_arguments.push(expression(builder, argument));
        }

        let return_value = builder.declare(lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic {
                name,
                args: call_arguments,
            }),
            location: expr.location,
        });

        builder.use_register(return_value, expr.location)
    })
}

fn is_condition(builder: &mut Builder<'_, '_>, expr: &lume_tir::Is) -> lume_mir::Operand {
    builder.with_current_block(|builder, _| {
        let (_, operand) = builder.use_value(&expr.target);
        let operand_type = builder.type_of_value(&operand);

        pattern(builder, &expr.pattern, operand, operand_type)
    })
}

fn literal(builder: &mut Builder<'_, '_>, expr: &lume_tir::Literal) -> lume_mir::Operand {
    match &expr.kind {
        lume_tir::LiteralKind::Boolean(val) => lume_mir::Operand {
            kind: lume_mir::OperandKind::Boolean { value: *val },
            location: expr.location,
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
                location: expr.location,
            }
        }
        lume_tir::LiteralKind::Float(val) => {
            let (bits, value) = match val {
                lume_tir::FloatLiteral::F32(val) => (32, *val),
                lume_tir::FloatLiteral::F64(val) => (64, *val),
            };

            lume_mir::Operand {
                kind: lume_mir::OperandKind::Float { value, bits },
                location: expr.location,
            }
        }
        lume_tir::LiteralKind::String(val) => {
            let string_operand = lume_mir::Operand {
                kind: lume_mir::OperandKind::String { value: *val },
                location: expr.location,
            };

            let string_type = builder.tcx().type_of(expr.id).unwrap();

            let alloc_ptr = builder.alloca(lume_mir::Type::string(), &string_type, expr.location);
            let untagged_ptr = builder.declare_untagged(alloc_ptr);

            builder.store(untagged_ptr, string_operand, expr.location);

            lume_mir::Operand {
                kind: lume_mir::OperandKind::Reference { id: alloc_ptr },
                location: expr.location,
            }
        }
    }
}

fn logical(builder: &mut Builder<'_, '_>, expr: &lume_tir::Logical) -> lume_mir::Operand {
    builder.with_current_block(|builder, _| {
        let name = match expr.op {
            lume_tir::LogicalOperator::And => lume_mir::Intrinsic::BooleanAnd,
            lume_tir::LogicalOperator::Or => lume_mir::Intrinsic::BooleanOr,
        };

        let args = vec![builder.use_value(&expr.lhs).1, builder.use_value(&expr.rhs).1];

        let return_value = builder.declare(lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic { name, args }),
            location: expr.location,
        });

        builder.use_register(return_value, expr.location)
    })
}

fn member_field(builder: &mut Builder<'_, '_>, expr: &lume_tir::Member) -> lume_mir::Operand {
    builder.with_current_block(|builder, _| {
        let callee_expr = expression(builder, &expr.callee);
        let callee_reg = builder.declare_operand(callee_expr, OperandRef::Implicit);

        let callee = builder.declare_untagged(callee_reg);

        let field = builder.tcx().hir_expect_field(expr.field);
        let (offset, field_type) = builder.field_offset(field);

        lume_mir::Operand {
            kind: lume_mir::OperandKind::LoadField {
                target: callee,
                offset,
                field_type,
            },
            location: expr.location,
        }
    })
}

fn scope(builder: &mut Builder<'_, '_>, expr: &lume_tir::Scope) -> lume_mir::Operand {
    builder.with_current_block(|builder, _| {
        let mut val: Option<lume_mir::Operand> = None;

        for stmt in &expr.body {
            val = statement(builder, stmt);
        }

        val.unwrap_or_else(|| builder.null_const())
    })
}

fn switch(builder: &mut Builder<'_, '_>, expr: &lume_tir::Switch) -> lume_mir::Operand {
    let entry_block = builder.func.current_block().id;
    let merge_block = builder.new_block();

    let result_type = builder.lower_type(&expr.fallback.ty);
    let result_slot = builder.alloc_slot(result_type.clone(), expr.location);

    let operand = builder.with_current_block(|builder, block| {
        let operand = if expr.operand.ty.is_float() {
            let (operand, _) = builder.use_value(&expr.operand);
            let bits = expr.operand.ty.bitwidth();

            builder.bitcast(operand, bits, expr.location)
        } else {
            let (operand, _) = builder.use_value(&expr.operand);

            operand
        };

        builder.declare_var(block, expr.operand_var, operand);

        operand
    });

    let mut arms = Vec::new();

    for (pattern, branch) in &expr.entries {
        builder.with_new_block(|builder, block| {
            let arm_pattern = match pattern {
                lume_tir::SwitchConstantPattern::Literal(lit) => match lit {
                    lume_tir::SwitchConstantLiteral::Boolean(lit) => i128::from(*lit),
                    lume_tir::SwitchConstantLiteral::Float(lit) => i128::from(lit.to_bits().cast_signed()),
                    lume_tir::SwitchConstantLiteral::Integer(lit) => *lit,
                },
                lume_tir::SwitchConstantPattern::Variable(_) => unreachable!(),
            };

            let (_, branch_value) = builder.use_value(branch);
            builder.store_slot(result_slot, branch_value, 0, branch.location());
            builder.branch_to(merge_block, branch.location());

            arms.push((arm_pattern, lume_mir::BlockBranchSite::new(block)));
        });
    }

    let fallback = builder.with_new_block(|builder, block| {
        let (_, branch_value) = builder.use_value(&expr.fallback);
        builder.store_slot(result_slot, branch_value, 0, expr.fallback.location());
        builder.branch_to(merge_block, expr.fallback.location());

        lume_mir::BlockBranchSite::new(block)
    });

    builder.with_block(entry_block, |builder, _| {
        builder.switch(operand, arms, fallback, expr.location);
    });

    builder.with_block(merge_block, |builder, _| {
        let slot_address = builder.slot_address(result_slot, 0, expr.location);

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Load {
                id: slot_address,
                loaded_type: result_type,
            },
            location: expr.location,
        }
    })
}

fn variable_reference(builder: &mut Builder<'_, '_>, expr: &lume_tir::VariableReference) -> lume_mir::Operand {
    builder.with_current_block(|builder, _| {
        let register = builder.reference_var(expr.reference);

        builder.use_register(register, expr.location)
    })
}

fn variant_expression(builder: &mut Builder<'_, '_>, expr: &lume_tir::Variant) -> lume_mir::Operand {
    builder.with_current_block(|builder, _| {
        let discriminant = lume_mir::Operand::integer(8, false, i128::from(expr.index));

        let enum_union_type = builder.union_of(&expr.ty);
        let variant_alloc = builder.alloca(enum_union_type, &expr.ty, expr.location);
        let variant_untagged_reg = builder.declare_untagged(variant_alloc);

        let mut offset = 0;

        // Store the discriminant of the variant right after the metadata
        builder.store_field(variant_untagged_reg, discriminant, offset, expr.location);
        offset += 1;

        // Store all of the arguments of the variant inside the allocation.
        for argument in &expr.arguments {
            let value = expression(builder, argument);
            let value_size = value.byte_size();

            builder.store_field(variant_untagged_reg, value, offset, expr.location);

            offset += value_size;
        }

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id: variant_alloc },
            location: expr.location,
        }
    })
}
