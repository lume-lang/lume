use error_snippet::Result;
use lume_span::{Internable, NodeId};
use lume_tir::VariableId;
use lume_types::TypeRef;

use crate::LowerFunction;

impl LowerFunction<'_> {
    #[libftrace::traced(level = Trace, err)]
    pub(crate) fn expression(&mut self, expr: lume_span::NodeId) -> Result<lume_tir::Expression> {
        let expr = self.lower.tcx.hir_expect_expr(expr);

        let kind = match &expr.kind {
            lume_hir::ExpressionKind::Assignment(expr) => self.assignment_expression(expr)?,
            lume_hir::ExpressionKind::Cast(expr) => self.cast_expression(expr)?,
            lume_hir::ExpressionKind::Construct(expr) => self.construct_expression(expr)?,
            lume_hir::ExpressionKind::InstanceCall(expr) => {
                self.call_expression(lume_hir::CallExpression::Instanced(expr))?
            }
            lume_hir::ExpressionKind::StaticCall(expr) => {
                self.call_expression(lume_hir::CallExpression::Static(expr))?
            }
            lume_hir::ExpressionKind::If(stmt) => self.if_expression(stmt)?,
            lume_hir::ExpressionKind::IntrinsicCall(expr) => self.intrinsic_expression(expr)?,
            lume_hir::ExpressionKind::Is(expr) => self.is_expression(expr)?,
            lume_hir::ExpressionKind::Literal(expr) => self.literal_expression(expr),
            lume_hir::ExpressionKind::Member(expr) => self.member_expression(expr)?,
            lume_hir::ExpressionKind::Switch(expr) => self.switch_expression(expr)?,
            lume_hir::ExpressionKind::Scope(expr) => self.scope_expression(expr)?,
            lume_hir::ExpressionKind::Variable(expr) => self.variable_expression(expr),
            lume_hir::ExpressionKind::Variant(expr) => self.variant_expression(expr)?,
        };

        let ty = self.lower.tcx.type_of_expr(expr)?;

        Ok(lume_tir::Expression { kind, ty })
    }

    fn assignment_expression(&mut self, expr: &lume_hir::Assignment) -> Result<lume_tir::ExpressionKind> {
        let target = self.expression(expr.target)?;
        let value = self.expression(expr.value)?;

        Ok(lume_tir::ExpressionKind::Assignment(Box::new(lume_tir::Assignment {
            id: expr.id,
            target,
            value,
            location: expr.location,
        })))
    }

    #[libftrace::traced(level = Trace, err)]
    fn cast_expression(&mut self, expr: &lume_hir::Cast) -> Result<lume_tir::ExpressionKind> {
        let source = self.expression(expr.source)?;

        let source_type = self.lower.tcx.type_of(expr.source)?;
        let target_type = self.lower.tcx.mk_type_ref_from_expr(&expr.target, expr.id)?;

        let is_integer_bitcast = source_type.is_integer() && target_type.is_integer();
        let is_float_bitcast = source_type.is_float() && target_type.is_float();

        if is_integer_bitcast || is_float_bitcast {
            return Ok(lume_tir::ExpressionKind::Bitcast(Box::new(lume_tir::Bitcast {
                id: expr.id,
                source,
                target: target_type,
                location: expr.location,
            })));
        }

        let Some(trait_impl) = self.lower.tcx.cast_impl_of(&source_type, &target_type) else {
            panic!("bug!: expected non-primitive Cast to implemented");
        };

        let method_impl = trait_impl
            .methods
            .first()
            .expect("bug!: expected Cast<T> implementation to have a single method");

        Ok(lume_tir::ExpressionKind::Call(Box::new(lume_tir::Call {
            id: expr.id,
            function: method_impl.id,
            arguments: vec![source],
            type_arguments: Vec::new(),
            return_type: target_type,
            location: expr.location,
        })))
    }

    #[libftrace::traced(level = Trace, err)]
    fn construct_expression(&mut self, expr: &lume_hir::Construct) -> Result<lume_tir::ExpressionKind> {
        let constructed_type = self.lower.tcx.find_type_ref_from(&expr.path, expr.id)?.unwrap();

        let mut constructed = expr.fields.clone();
        let fields = self.lower.tcx.tdb().find_fields(constructed_type.instance_of);

        for field in fields {
            if let Some(default_field) = self.lower.tcx.constructer_default_field_of(expr, &field.name) {
                constructed.push(default_field);
            }
        }

        let constructed = constructed
            .into_iter()
            .map(|field| {
                let name = field.name.to_string().intern();
                let value = self.expression(field.value)?;

                Ok(lume_tir::ConstructorField {
                    name,
                    value,
                    location: field.location,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(lume_tir::ExpressionKind::Construct(Box::new(lume_tir::Construct {
            id: expr.id,
            ty: constructed_type,
            fields: constructed,
            location: expr.location,
        })))
    }

    #[libftrace::traced(level = Trace, err)]
    fn call_expression(&mut self, expr: lume_hir::CallExpression) -> Result<lume_tir::ExpressionKind> {
        let callable = self.lower.tcx.lookup_callable(expr)?;
        let instantiated_signature = self.lower.tcx.signature_of_instantiated(callable, expr)?;

        libftrace::debug!(
            "resolved callable `{:+}` from call expression `{}`",
            callable.name(),
            expr.name()
        );

        let function = callable.id();
        let mut arguments = Vec::with_capacity(expr.arguments().len());

        if let lume_hir::CallExpression::Instanced(instance_call) = &expr {
            arguments.push(self.expression(instance_call.callee)?);
        }

        for arg in expr.arguments() {
            arguments.push(self.expression(arg)?);
        }

        let mut type_arguments = Vec::with_capacity(expr.type_arguments().len());

        // Add the surround type arguments of the expression, which are also required
        // by the function to be invoked.
        match &expr {
            lume_hir::CallExpression::Instanced(call) => {
                let callee = self.expression(call.callee)?;

                type_arguments.extend(callee.ty.type_arguments);
            }
            lume_hir::CallExpression::Static(call) => {
                for type_arg in call.name.all_root_type_arguments() {
                    let tir_type_arg = self.lower.tcx.mk_type_ref_from_expr(&type_arg, expr.id())?;

                    type_arguments.push(tir_type_arg);
                }
            }
            lume_hir::CallExpression::Intrinsic(_) => {}
        }

        for type_arg in expr.type_arguments() {
            let tir_type_arg = self.lower.tcx.mk_type_ref_from_expr(type_arg, expr.id())?;

            type_arguments.push(tir_type_arg);
        }

        Ok(lume_tir::ExpressionKind::Call(Box::new(lume_tir::Call {
            id: expr.id(),
            function,
            arguments,
            type_arguments,
            return_type: instantiated_signature.ret_ty,
            location: expr.location(),
        })))
    }

    fn if_expression(&mut self, expr: &lume_hir::If) -> Result<lume_tir::ExpressionKind> {
        let cases = expr
            .cases
            .iter()
            .map(|case| {
                let condition = if let Some(val) = case.condition {
                    Some(self.expression(val)?)
                } else {
                    None
                };

                let block = self.lower_block(&case.block)?;

                Ok(lume_tir::Conditional {
                    condition,
                    block,
                    location: case.location,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        let return_type = self.lower.tcx.type_of_if_conditional(expr)?;

        Ok(lume_tir::ExpressionKind::If(lume_tir::If {
            id: expr.id,
            cases,
            return_type: if return_type.is_void() { None } else { Some(return_type) },
            location: expr.location,
        }))
    }

    #[libftrace::traced(level = Trace, err)]
    fn intrinsic_expression(&mut self, expr: &lume_hir::IntrinsicCall) -> Result<lume_tir::ExpressionKind> {
        let arguments = expr
            .kind
            .arguments()
            .into_iter()
            .map(|arg| self.expression(arg))
            .collect::<Result<Vec<_>>>()?;

        let Some(kind) = self.intrinsic_of(expr) else {
            let callable = self.lower.tcx.probe_callable_intrinsic(expr)?;

            return Ok(lume_tir::ExpressionKind::Call(Box::new(lume_tir::Call {
                id: expr.id,
                function: callable.id(),
                arguments,
                type_arguments: Vec::new(),
                return_type: callable.return_type().to_owned(),
                location: expr.location,
            })));
        };

        Ok(lume_tir::ExpressionKind::IntrinsicCall(Box::new(
            lume_tir::IntrinsicCall {
                id: expr.id,
                kind,
                arguments,
                location: expr.location,
            },
        )))
    }

    fn intrinsic_of(&mut self, expr: &lume_hir::IntrinsicCall) -> Option<lume_tir::IntrinsicKind> {
        let callee_ty = self.lower.tcx.type_of(expr.kind.callee()).unwrap();

        if callee_ty.is_integer() {
            let bits = callee_ty.bitwidth();
            let signed = callee_ty.signed();

            match &expr.kind {
                lume_hir::IntrinsicKind::Equal { .. } => Some(lume_tir::IntrinsicKind::IntEq { bits, signed }),
                lume_hir::IntrinsicKind::NotEqual { .. } => Some(lume_tir::IntrinsicKind::IntNe { bits, signed }),
                lume_hir::IntrinsicKind::Greater { .. } => Some(lume_tir::IntrinsicKind::IntGt { bits, signed }),
                lume_hir::IntrinsicKind::GreaterEqual { .. } => Some(lume_tir::IntrinsicKind::IntGe { bits, signed }),
                lume_hir::IntrinsicKind::Less { .. } => Some(lume_tir::IntrinsicKind::IntLt { bits, signed }),
                lume_hir::IntrinsicKind::LessEqual { .. } => Some(lume_tir::IntrinsicKind::IntLe { bits, signed }),
                lume_hir::IntrinsicKind::BinaryAnd { .. } => Some(lume_tir::IntrinsicKind::IntAnd { bits, signed }),
                lume_hir::IntrinsicKind::BinaryOr { .. } => Some(lume_tir::IntrinsicKind::IntOr { bits, signed }),
                lume_hir::IntrinsicKind::BinaryXor { .. } => Some(lume_tir::IntrinsicKind::IntXor { bits, signed }),
                lume_hir::IntrinsicKind::Add { .. } => Some(lume_tir::IntrinsicKind::IntAdd { bits, signed }),
                lume_hir::IntrinsicKind::Sub { .. } => Some(lume_tir::IntrinsicKind::IntSub { bits, signed }),
                lume_hir::IntrinsicKind::Mul { .. } => Some(lume_tir::IntrinsicKind::IntMul { bits, signed }),
                lume_hir::IntrinsicKind::Div { .. } => Some(lume_tir::IntrinsicKind::IntDiv { bits, signed }),
                lume_hir::IntrinsicKind::Negate { .. } => Some(lume_tir::IntrinsicKind::IntNegate { bits, signed }),
                _ => None,
            }
        } else if callee_ty.is_float() {
            let bits = match callee_ty {
                ty if ty.is_f32() => 32,
                ty if ty.is_f64() => 64,
                _ => unreachable!(),
            };

            match &expr.kind {
                lume_hir::IntrinsicKind::Equal { .. } => Some(lume_tir::IntrinsicKind::FloatEq { bits }),
                lume_hir::IntrinsicKind::NotEqual { .. } => Some(lume_tir::IntrinsicKind::FloatNe { bits }),
                lume_hir::IntrinsicKind::Greater { .. } => Some(lume_tir::IntrinsicKind::FloatGt { bits }),
                lume_hir::IntrinsicKind::GreaterEqual { .. } => Some(lume_tir::IntrinsicKind::FloatGe { bits }),
                lume_hir::IntrinsicKind::Less { .. } => Some(lume_tir::IntrinsicKind::FloatLt { bits }),
                lume_hir::IntrinsicKind::LessEqual { .. } => Some(lume_tir::IntrinsicKind::FloatLe { bits }),
                lume_hir::IntrinsicKind::Add { .. } => Some(lume_tir::IntrinsicKind::FloatAdd { bits }),
                lume_hir::IntrinsicKind::Sub { .. } => Some(lume_tir::IntrinsicKind::FloatSub { bits }),
                lume_hir::IntrinsicKind::Mul { .. } => Some(lume_tir::IntrinsicKind::FloatMul { bits }),
                lume_hir::IntrinsicKind::Div { .. } => Some(lume_tir::IntrinsicKind::FloatDiv { bits }),
                lume_hir::IntrinsicKind::Negate { .. } => Some(lume_tir::IntrinsicKind::FloatNegate { bits }),
                _ => None,
            }
        } else if callee_ty.is_bool() {
            match &expr.kind {
                lume_hir::IntrinsicKind::Equal { .. } => Some(lume_tir::IntrinsicKind::BooleanEq),
                lume_hir::IntrinsicKind::NotEqual { .. } => Some(lume_tir::IntrinsicKind::BooleanNe),
                lume_hir::IntrinsicKind::And { .. } => Some(lume_tir::IntrinsicKind::BooleanAnd),
                lume_hir::IntrinsicKind::Or { .. } => Some(lume_tir::IntrinsicKind::BooleanOr),
                lume_hir::IntrinsicKind::Not { .. } => Some(lume_tir::IntrinsicKind::BooleanNot),
                _ => None,
            }
        } else {
            None
        }
    }

    #[libftrace::traced(level = Trace, err)]
    fn is_expression(&mut self, expr: &lume_hir::Is) -> Result<lume_tir::ExpressionKind> {
        let target = self.expression(expr.target)?;
        let pattern = self.pattern(&expr.pattern)?;

        Ok(lume_tir::ExpressionKind::Is(Box::new(lume_tir::Is {
            id: expr.id,
            target,
            pattern,
            location: expr.location,
        })))
    }

    fn literal_expression(&self, expr: &lume_hir::Literal) -> lume_tir::ExpressionKind {
        let literal = self.literal(expr);

        lume_tir::ExpressionKind::Literal(literal)
    }

    #[allow(clippy::unused_self)]
    pub(super) fn literal(&self, expr: &lume_hir::Literal) -> lume_tir::Literal {
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        let kind = match &expr.kind {
            lume_hir::LiteralKind::Int(int) => {
                let kind = if let Some(k) = int.kind {
                    k
                } else {
                    self.lower.tcx.kind_of_int(int).unwrap()
                };

                match kind {
                    lume_hir::IntKind::I8 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::I8(int.value)),
                    lume_hir::IntKind::U8 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::U8(int.value)),
                    lume_hir::IntKind::I16 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::I16(int.value)),
                    lume_hir::IntKind::U16 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::U16(int.value)),
                    lume_hir::IntKind::I32 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::I32(int.value)),
                    lume_hir::IntKind::U32 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::U32(int.value)),
                    lume_hir::IntKind::I64 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::I64(int.value)),
                    lume_hir::IntKind::U64 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::U64(int.value)),
                }
            }
            lume_hir::LiteralKind::Float(float) => {
                let kind = if let Some(k) = float.kind {
                    k
                } else {
                    self.lower.tcx.kind_of_float(float).unwrap()
                };

                match kind {
                    lume_hir::FloatKind::F32 => lume_tir::LiteralKind::Float(lume_tir::FloatLiteral::F32(float.value)),
                    lume_hir::FloatKind::F64 => lume_tir::LiteralKind::Float(lume_tir::FloatLiteral::F64(float.value)),
                }
            }
            lume_hir::LiteralKind::Boolean(bool) => lume_tir::LiteralKind::Boolean(bool.value),
            lume_hir::LiteralKind::String(string) => lume_tir::LiteralKind::String(string.value.intern()),
        };

        lume_tir::Literal {
            id: expr.id,
            kind,
            location: expr.location,
        }
    }

    #[libftrace::traced(level = Trace, err)]
    fn member_expression(&mut self, expr: &lume_hir::Member) -> Result<lume_tir::ExpressionKind> {
        let callee = self.expression(expr.callee)?;
        let name = expr.name.intern();

        let callee_ty = self.lower.tcx.type_of(expr.callee)?;
        let field = self
            .lower
            .tcx
            .tdb()
            .find_field(callee_ty.instance_of, &expr.name)
            .unwrap()
            .clone();

        Ok(lume_tir::ExpressionKind::Member(Box::new(lume_tir::Member {
            id: expr.id,
            callee,
            field,
            name,
            location: expr.location,
        })))
    }

    #[libftrace::traced(level = Trace)]
    fn scope_expression(&mut self, expr: &lume_hir::Scope) -> Result<lume_tir::ExpressionKind> {
        let mut body = Vec::with_capacity(expr.body.len());
        for stmt in &expr.body {
            body.push(self.statement(*stmt)?);
        }

        let return_type = self.lower.tcx.type_of_scope(expr)?;

        Ok(lume_tir::ExpressionKind::Scope(Box::new(lume_tir::Scope {
            id: expr.id,
            body,
            return_type,
            location: expr.location,
        })))
    }

    #[libftrace::traced(level = Trace)]
    fn switch_expression(&mut self, expr: &lume_hir::Switch) -> Result<lume_tir::ExpressionKind> {
        if self.lower.tcx.switch_table_const_literal(expr) {
            self.switch_expression_constant(expr)
        } else {
            self.switch_expression_dynamic(expr)
        }
    }

    #[libftrace::traced(level = Trace)]
    fn switch_expression_constant(&mut self, expr: &lume_hir::Switch) -> Result<lume_tir::ExpressionKind> {
        let operand = self.expression(expr.operand)?;
        let mut fallback: Option<lume_tir::Expression> = None;
        let mut entries = Vec::with_capacity(expr.cases.len());

        let operand_var = self.mark_variable(lume_tir::VariableSource::Variable);
        self.variable_mapping.insert(expr.operand, operand_var);

        for (idx, case) in expr.cases.iter().enumerate() {
            let is_last_case = idx >= expr.cases.len() - 1;
            let branch = self.expression(case.branch)?;

            if is_last_case && fallback.is_none() {
                fallback = Some(branch);
                break;
            }

            let pattern = match &case.pattern.kind {
                lume_hir::PatternKind::Literal(literal) => {
                    let const_literal = match &literal.literal.kind {
                        lume_hir::LiteralKind::Int(lit) => lume_tir::SwitchConstantLiteral::Integer(lit.value),
                        lume_hir::LiteralKind::Boolean(lit) => lume_tir::SwitchConstantLiteral::Boolean(lit.value),
                        lume_hir::LiteralKind::Float(lit) => lume_tir::SwitchConstantLiteral::Float(lit.value),
                        lume_hir::LiteralKind::String(_) => unreachable!(),
                    };

                    lume_tir::SwitchConstantPattern::Literal(const_literal)
                }
                lume_hir::PatternKind::Identifier(_) | lume_hir::PatternKind::Wildcard(_) => {
                    fallback = Some(branch);
                    continue;
                }
                lume_hir::PatternKind::Variant(_) => unreachable!(),
            };

            entries.push((pattern, branch));
        }

        let Some(fallback) = fallback else {
            return Err(crate::diagnostics::MissingWildcardCase { source: expr.location }.into());
        };

        Ok(lume_tir::ExpressionKind::Switch(Box::new(lume_tir::Switch {
            id: expr.id,
            operand,
            operand_var,
            entries,
            fallback,
            location: expr.location,
        })))
    }

    #[libftrace::traced(level = Trace)]
    fn switch_expression_dynamic(&mut self, expr: &lume_hir::Switch) -> Result<lume_tir::ExpressionKind> {
        let operand = self.expression(expr.operand)?;
        let mut cases = Vec::with_capacity(expr.cases.len());
        let switch_ret_type = self.lower.tcx.type_of(expr.id)?;

        let operand_var = self.mark_variable(lume_tir::VariableSource::Variable);
        self.variable_mapping.insert(expr.operand, operand_var);

        for (idx, case) in expr.cases.iter().enumerate() {
            let is_last_case = idx >= expr.cases.len() - 1;
            let return_type = self.lower.tcx.type_of(case.branch)?;

            let pattern = match &case.pattern.kind {
                lume_hir::PatternKind::Literal(_) | lume_hir::PatternKind::Variant(_) => self.pattern(&case.pattern)?,
                lume_hir::PatternKind::Identifier(_) | lume_hir::PatternKind::Wildcard(_) => {
                    let branch = self.expression(case.branch)?;

                    cases.push(lume_tir::Conditional {
                        condition: None,
                        block: lume_tir::Block {
                            statements: vec![lume_tir::Statement::Expression(branch)],
                            return_type,
                        },
                        location: case.location,
                    });

                    // If a fallback is present, there's no reason to process
                    // the rest of the cases, since they can never be met.
                    break;
                }
            };

            let branch = self.expression(case.branch)?;

            let condition = if is_last_case {
                None
            } else {
                Some(lume_tir::Expression {
                    kind: lume_tir::ExpressionKind::Is(Box::new(lume_tir::Is {
                        id: NodeId::empty(case.branch.package),
                        target: operand.clone(),
                        pattern,
                        location: case.location,
                    })),
                    ty: TypeRef::bool(),
                })
            };

            let conditional = lume_tir::Conditional {
                condition,
                block: lume_tir::Block {
                    statements: vec![lume_tir::Statement::Expression(branch)],
                    return_type,
                },
                location: case.location,
            };

            cases.push(conditional);
        }

        Ok(lume_tir::ExpressionKind::If(lume_tir::If {
            id: NodeId::empty(expr.id.package),
            cases,
            return_type: Some(switch_ret_type),
            location: expr.location,
        }))
    }

    #[libftrace::traced(level = Trace)]
    fn variable_expression(&self, expr: &lume_hir::Variable) -> lume_tir::ExpressionKind {
        let reference = match &expr.reference {
            lume_hir::VariableSource::Parameter(param) => VariableId(param.index),
            lume_hir::VariableSource::Variable(var) => *self.variable_mapping.get(&var.id).unwrap(),
            lume_hir::VariableSource::Pattern(pat) => *self.variable_mapping.get(&pat.id).unwrap(),
        };

        let source = match expr.reference {
            lume_hir::VariableSource::Parameter(_) => lume_tir::VariableSource::Parameter,
            lume_hir::VariableSource::Variable(_) | lume_hir::VariableSource::Pattern(_) => {
                lume_tir::VariableSource::Variable
            }
        };

        lume_tir::ExpressionKind::Variable(Box::new(lume_tir::VariableReference {
            id: expr.id,
            name: expr.name.to_string().intern(),
            reference,
            source,
            location: expr.location,
        }))
    }

    #[libftrace::traced(level = Trace, err)]
    fn variant_expression(&mut self, expr: &lume_hir::Variant) -> Result<lume_tir::ExpressionKind> {
        let name = self.path_hir(&expr.name, expr.id)?;
        let enum_type = expr.name.clone().parent().unwrap();

        let ty = self.lower.tcx.find_type_ref_from(&enum_type, expr.id)?.unwrap();
        let index = self.lower.tcx.enum_case_with_name(&expr.name)?.idx;

        let arguments = expr
            .arguments
            .iter()
            .map(|arg| self.expression(*arg))
            .collect::<Result<Vec<_>>>()?;

        #[allow(clippy::cast_possible_truncation)]
        Ok(lume_tir::ExpressionKind::Variant(Box::new(lume_tir::Variant {
            id: expr.id,
            index: index as u8,
            name,
            ty,
            arguments,
            location: expr.location,
        })))
    }
}
