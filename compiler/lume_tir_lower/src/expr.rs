use error_snippet::Result;
use lume_span::{DefId, Internable};
use lume_tir::VariableId;
use lume_type_metadata::{FunctionId, FunctionKind};

use crate::LowerFunction;

impl LowerFunction<'_> {
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub(crate) fn expression(&mut self, expr: lume_span::ExpressionId) -> Result<lume_tir::Expression> {
        let expr = self.lower.tcx.hir_expect_expr(expr);

        let kind = match &expr.kind {
            lume_hir::ExpressionKind::Assignment(expr) => self.assignment_expression(expr)?,
            lume_hir::ExpressionKind::Binary(expr) => self.binary_expression(expr)?,
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
            lume_hir::ExpressionKind::Literal(expr) => self.literal_expression(expr),
            lume_hir::ExpressionKind::Logical(expr) => self.logical_expression(expr)?,
            lume_hir::ExpressionKind::Member(expr) => self.member_expression(expr)?,
            lume_hir::ExpressionKind::Field(_) | lume_hir::ExpressionKind::Switch(_) => todo!(),
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
        })))
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn binary_expression(&mut self, expr: &lume_hir::Binary) -> Result<lume_tir::ExpressionKind> {
        let lhs = self.expression(expr.lhs)?;
        let rhs = self.expression(expr.rhs)?;

        let op = match expr.op.kind {
            lume_hir::BinaryOperatorKind::And => lume_tir::BinaryOperator::And,
            lume_hir::BinaryOperatorKind::Or => lume_tir::BinaryOperator::Or,
            lume_hir::BinaryOperatorKind::Xor => lume_tir::BinaryOperator::Xor,
        };

        Ok(lume_tir::ExpressionKind::Binary(Box::new(lume_tir::Binary {
            id: expr.id,
            lhs,
            op,
            rhs,
        })))
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn cast_expression(&mut self, expr: &lume_hir::Cast) -> Result<lume_tir::ExpressionKind> {
        let source = self.expression(expr.source)?;
        let target = self.lower.tcx.mk_type_ref_from_expr(&expr.target, expr.id)?;

        Ok(lume_tir::ExpressionKind::Cast(Box::new(lume_tir::Cast {
            id: expr.id,
            source,
            target,
        })))
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn construct_expression(&mut self, expr: &lume_hir::Construct) -> Result<lume_tir::ExpressionKind> {
        let constructed_type = self
            .lower
            .tcx
            .find_type_ref_from(&expr.path, DefId::Expression(expr.id))?
            .unwrap();

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

                Ok(lume_tir::ConstructorField { name, value })
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(lume_tir::ExpressionKind::Construct(Box::new(lume_tir::Construct {
            id: expr.id,
            ty: constructed_type,
            fields: constructed,
        })))
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn call_expression(&mut self, expr: lume_hir::CallExpression) -> Result<lume_tir::ExpressionKind> {
        let function = match self.lower.tcx.lookup_callable(expr)? {
            lume_typech::query::Callable::Function(call) => {
                #[cfg(debug_assertions)]
                if !call.sig().is_vararg() {
                    debug_assert!(call.parameters.len() == expr.arguments().len());
                }

                FunctionId::new(FunctionKind::Function, call.id.index.as_usize())
            }
            lume_typech::query::Callable::Method(call) => FunctionId::new(FunctionKind::Method, call.id.0),
        };

        let mut arguments = Vec::with_capacity(expr.arguments().len());

        if let lume_hir::CallExpression::Instanced(instance_call) = &expr {
            arguments.push(self.expression(instance_call.callee)?);
        }

        for arg in expr.arguments() {
            arguments.push(self.expression(*arg)?);
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

                Ok(lume_tir::Conditional { condition, block })
            })
            .collect::<Result<Vec<_>>>()?;

        let return_type = self.lower.tcx.type_of_if_conditional(expr)?;

        Ok(lume_tir::ExpressionKind::If(lume_tir::If {
            id: expr.id,
            cases,
            return_type: if return_type.is_void() { None } else { Some(return_type) },
        }))
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn intrinsic_expression(&mut self, expr: &lume_hir::IntrinsicCall) -> Result<lume_tir::ExpressionKind> {
        let kind = self.intrinsic_of(expr);
        let arguments = expr
            .arguments
            .iter()
            .map(|arg| self.expression(*arg))
            .collect::<Result<Vec<_>>>()?;

        Ok(lume_tir::ExpressionKind::IntrinsicCall(Box::new(
            lume_tir::IntrinsicCall {
                id: expr.id,
                kind,
                arguments,
            },
        )))
    }

    fn intrinsic_of(&mut self, expr: &lume_hir::IntrinsicCall) -> lume_tir::IntrinsicKind {
        let callee_ty = self.lower.tcx.type_of(expr.callee()).unwrap();

        if callee_ty.is_integer() {
            let bits = callee_ty.bitwidth();
            let signed = callee_ty.signed();

            match expr.name.name().as_str() {
                "==" => lume_tir::IntrinsicKind::IntEq { bits, signed },
                "!=" => lume_tir::IntrinsicKind::IntNe { bits, signed },
                ">" => lume_tir::IntrinsicKind::IntGt { bits, signed },
                ">=" => lume_tir::IntrinsicKind::IntGe { bits, signed },
                "<" => lume_tir::IntrinsicKind::IntLt { bits, signed },
                "<=" => lume_tir::IntrinsicKind::IntLe { bits, signed },
                "+" => lume_tir::IntrinsicKind::IntAdd { bits, signed },
                "-" => lume_tir::IntrinsicKind::IntSub { bits, signed },
                "*" => lume_tir::IntrinsicKind::IntMul { bits, signed },
                "/" => lume_tir::IntrinsicKind::IntDiv { bits, signed },
                _ => unreachable!(),
            }
        } else if callee_ty.is_float() {
            let bits = match callee_ty {
                ty if ty.is_f32() => 32,
                ty if ty.is_f64() => 64,
                _ => unreachable!(),
            };

            match expr.name.name().as_str() {
                "==" => lume_tir::IntrinsicKind::FloatEq { bits },
                "!=" => lume_tir::IntrinsicKind::FloatNe { bits },
                ">" => lume_tir::IntrinsicKind::FloatGt { bits },
                ">=" => lume_tir::IntrinsicKind::FloatGe { bits },
                "<" => lume_tir::IntrinsicKind::FloatLt { bits },
                "<=" => lume_tir::IntrinsicKind::FloatLe { bits },
                "+" => lume_tir::IntrinsicKind::FloatAdd { bits },
                "-" => lume_tir::IntrinsicKind::FloatSub { bits },
                "*" => lume_tir::IntrinsicKind::FloatMul { bits },
                "/" => lume_tir::IntrinsicKind::FloatDiv { bits },
                _ => unreachable!(),
            }
        } else if callee_ty.is_bool() {
            match expr.name.name().as_str() {
                "==" => lume_tir::IntrinsicKind::BooleanEq,
                "!=" => lume_tir::IntrinsicKind::BooleanNe,
                _ => unreachable!(),
            }
        } else {
            unreachable!()
        }
    }

    #[allow(clippy::unused_self)]
    fn literal_expression(&self, expr: &lume_hir::Literal) -> lume_tir::ExpressionKind {
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        let lit = match &expr.kind {
            lume_hir::LiteralKind::Int(int) => match int.kind {
                lume_hir::IntKind::I8 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::I8(int.value)),
                lume_hir::IntKind::U8 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::U8(int.value)),
                lume_hir::IntKind::I16 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::I16(int.value)),
                lume_hir::IntKind::U16 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::U16(int.value)),
                lume_hir::IntKind::I32 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::I32(int.value)),
                lume_hir::IntKind::U32 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::U32(int.value)),
                lume_hir::IntKind::I64 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::I64(int.value)),
                lume_hir::IntKind::U64 => lume_tir::LiteralKind::Int(lume_tir::IntLiteral::U64(int.value)),
            },
            lume_hir::LiteralKind::Float(float) => match float.kind {
                lume_hir::FloatKind::F32 => lume_tir::LiteralKind::Float(lume_tir::FloatLiteral::F32(float.value)),
                lume_hir::FloatKind::F64 => lume_tir::LiteralKind::Float(lume_tir::FloatLiteral::F64(float.value)),
            },
            lume_hir::LiteralKind::Boolean(bool) => lume_tir::LiteralKind::Boolean(bool.value),
            lume_hir::LiteralKind::String(string) => lume_tir::LiteralKind::String(string.value.intern()),
        };

        lume_tir::ExpressionKind::Literal(lume_tir::Literal { id: expr.id, kind: lit })
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn logical_expression(&mut self, expr: &lume_hir::Logical) -> Result<lume_tir::ExpressionKind> {
        let lhs = self.expression(expr.lhs)?;
        let rhs = self.expression(expr.rhs)?;

        let op = match expr.op.kind {
            lume_hir::LogicalOperatorKind::And => lume_tir::LogicalOperator::And,
            lume_hir::LogicalOperatorKind::Or => lume_tir::LogicalOperator::Or,
        };

        Ok(lume_tir::ExpressionKind::Logical(Box::new(lume_tir::Logical {
            id: expr.id,
            lhs,
            op,
            rhs,
        })))
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
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
        })))
    }

    #[tracing::instrument(level = "TRACE", skip_all)]
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
        })))
    }

    #[tracing::instrument(level = "TRACE", skip_all)]
    fn variable_expression(&self, expr: &lume_hir::Variable) -> lume_tir::ExpressionKind {
        let reference = match &expr.reference {
            lume_hir::VariableSource::Parameter(param) => VariableId(param.index),
            lume_hir::VariableSource::Variable(var) => *self.variable_mapping.get(&var.id).unwrap(),
            lume_hir::VariableSource::Pattern(_) => todo!(),
        };

        let source = match expr.reference {
            lume_hir::VariableSource::Parameter(_) => lume_tir::VariableSource::Parameter,
            lume_hir::VariableSource::Variable(_) => lume_tir::VariableSource::Variable,
            lume_hir::VariableSource::Pattern(_) => todo!(),
        };

        lume_tir::ExpressionKind::Variable(Box::new(lume_tir::VariableReference {
            id: expr.id,
            name: expr.name.to_string().intern(),
            reference,
            source,
        }))
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn variant_expression(&mut self, expr: &lume_hir::Variant) -> Result<lume_tir::ExpressionKind> {
        let name = self.path_hir(&expr.name, DefId::Expression(expr.id))?;
        let enum_type = expr.name.clone().parent().unwrap();

        let ty = self
            .lower
            .tcx
            .find_type_ref_from(&enum_type, DefId::Expression(expr.id))?
            .unwrap();

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
        })))
    }
}
