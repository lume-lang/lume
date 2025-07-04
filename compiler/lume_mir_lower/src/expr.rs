use crate::FunctionTransformer;

impl FunctionTransformer<'_> {
    pub(super) fn expression(&mut self, expr: &lume_hir::Expression) -> lume_mir::Value {
        match &expr.kind {
            lume_hir::ExpressionKind::Assignment(expr) => self.assignment(expr),
            lume_hir::ExpressionKind::Literal(lit) => self.literal(&lit.kind),
            lume_hir::ExpressionKind::Variable(var) => self.variable_reference(var),
            _ => lume_mir::Value::Boolean { value: false },
        }
    }

    fn assignment(&mut self, expr: &lume_hir::Assignment) -> lume_mir::Value {
        let lume_mir::Value::Reference { id } = self.expression(&expr.target) else {
            todo!()
        };

        let value = self.expression(&expr.value);
        let local = self.func.current_block_mut().assign(id, value);

        lume_mir::Value::Reference { id: local }
    }

    fn variable_reference(&mut self, expr: &lume_hir::Variable) -> lume_mir::Value {
        let local = match &expr.reference {
            lume_hir::VariableSource::Parameter(id) => lume_mir::Local::param(id.index),
            lume_hir::VariableSource::Variable(id) => *self.func.variables.get(&id.id).unwrap(),
        };

        lume_mir::Value::Reference { id: local }
    }

    #[expect(clippy::unused_self)]
    fn literal(&self, expr: &lume_hir::LiteralKind) -> lume_mir::Value {
        match expr {
            lume_hir::LiteralKind::Boolean(val) => lume_mir::Value::Boolean { value: val.value },
            lume_hir::LiteralKind::Int(val) => {
                let (bits, signed) = match val.kind {
                    lume_hir::IntKind::I8 => (8, true),
                    lume_hir::IntKind::I16 => (16, true),
                    lume_hir::IntKind::I32 => (32, true),
                    lume_hir::IntKind::I64 => (64, true),
                    lume_hir::IntKind::U8 => (8, false),
                    lume_hir::IntKind::U16 => (16, false),
                    lume_hir::IntKind::U32 => (32, false),
                    lume_hir::IntKind::U64 => (64, false),
                };

                lume_mir::Value::Integer {
                    value: val.value,
                    bits,
                    signed,
                }
            }
            lume_hir::LiteralKind::Float(val) => {
                let bits = match val.kind {
                    lume_hir::FloatKind::F32 => 32,
                    lume_hir::FloatKind::F64 => 64,
                };

                lume_mir::Value::Float { value: val.value, bits }
            }
            lume_hir::LiteralKind::String(val) => lume_mir::Value::String {
                value: val.value.clone(),
            },
        }
    }
}
