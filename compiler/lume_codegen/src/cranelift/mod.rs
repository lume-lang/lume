pub(crate) mod inst;
pub(crate) mod metadata;
pub(crate) mod ty;
pub(crate) mod value;

use std::{
    collections::HashMap,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use cranelift::{
    codegen::{
        ir::{BlockArg, GlobalValue, StackSlot, immediates::Offset32},
        verify_function,
    },
    prelude::*,
};
use cranelift_module::{DataDescription, DataId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use error_snippet::SimpleDiagnostic;
use indexmap::IndexMap;
use lume_errors::Result;
use lume_mir::{BlockBranchSite, RegisterId, SlotId};

use crate::{Backend, CompiledModule, Context};

#[derive(Debug, Clone)]
struct DeclaredFunction {
    pub id: cranelift_module::FuncId,
    pub sig: Signature,
}

#[derive(Debug, Clone)]
struct IntrinsicFunctions {
    pub malloc: cranelift_module::FuncId,
}

pub(crate) struct CraneliftBackend<'ctx> {
    context: Context<'ctx>,
    module: Option<Arc<RwLock<ObjectModule>>>,

    declared_funcs: IndexMap<lume_mir::FunctionId, DeclaredFunction>,
    intrinsics: IntrinsicFunctions,

    static_data: RwLock<HashMap<String, DataId>>,
}

impl<'ctx> Backend<'ctx> for CraneliftBackend<'ctx> {
    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn initialize(&mut self) -> lume_errors::Result<()> {
        Ok(())
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn generate(&mut self) -> lume_errors::Result<CompiledModule> {
        self.declare_type_metadata();

        let functions = std::mem::take(&mut self.context.mir.functions);

        for func in functions.values() {
            if func.signature.external {
                continue;
            }

            let sig = self.declare_function(func);

            let func_id = self
                .module_mut()
                .declare_function(&func.name, cranelift_module::Linkage::Export, &sig)
                .map_error()?;

            self.declared_funcs
                .insert(func.id, DeclaredFunction { id: func_id, sig });
        }

        let mut ctx = self.module_mut().make_context();
        let mut builder_ctx = FunctionBuilderContext::new();

        for func in functions.values() {
            if func.signature.external {
                continue;
            }

            self.define_function(func, &mut ctx, &mut builder_ctx)?;
            self.module().clear_context(&mut ctx);
        }

        let module = Arc::into_inner(self.module.take().unwrap())
            .unwrap()
            .into_inner()
            .unwrap();

        let object_binary = module.finish().emit().unwrap();

        Ok(CompiledModule {
            name: self.context.package.name.clone(),
            bytecode: object_binary,
        })
    }
}

impl<'ctx> CraneliftBackend<'ctx> {
    pub fn new(context: Context<'ctx>) -> Result<Self> {
        let mut settings = cranelift::codegen::settings::builder();
        settings.enable("is_pic").unwrap();

        let isa = cranelift_native::builder()
            .unwrap()
            .finish(cranelift::codegen::settings::Flags::new(settings))
            .unwrap();

        let builder = ObjectBuilder::new(
            isa,
            context.package.name.clone(),
            cranelift_module::default_libcall_names(),
        )
        .unwrap();

        let mut module = ObjectModule::new(builder);
        let ptr_type = module.target_config().pointer_type();

        let intrinsics = IntrinsicFunctions {
            malloc: Self::declare_external_function(&mut module, "malloc", &[types::I64], Some(ptr_type))?,
        };

        Ok(Self {
            context,
            module: Some(Arc::new(RwLock::new(module))),
            declared_funcs: IndexMap::new(),
            intrinsics,
            static_data: RwLock::new(HashMap::new()),
        })
    }

    #[track_caller]
    pub(crate) fn module(&self) -> RwLockReadGuard<'_, ObjectModule> {
        self.module.as_ref().unwrap().try_read().unwrap()
    }

    #[track_caller]
    pub(crate) fn module_mut(&self) -> RwLockWriteGuard<'_, ObjectModule> {
        self.module.as_ref().unwrap().try_write().unwrap()
    }

    #[tracing::instrument(level = "TRACE", skip(module), err)]
    fn declare_external_function(
        module: &mut ObjectModule,
        name: &'static str,
        params: &[types::Type],
        ret: Option<types::Type>,
    ) -> Result<cranelift_module::FuncId> {
        let mut sig = module.make_signature();

        for param in params {
            sig.params.push(AbiParam::new(*param));
        }

        if let Some(ret_ty) = ret {
            sig.returns.push(AbiParam::new(ret_ty));
        }

        let func_id = module
            .declare_function(name, cranelift_module::Linkage::Import, &sig)
            .map_error()?;

        Ok(func_id)
    }

    #[tracing::instrument(level = "TRACE", skip_all, fields(func = %func.name))]
    fn declare_function(&mut self, func: &lume_mir::Function) -> Signature {
        let module = self.module();
        let mut sig = module.make_signature();

        for param in &func.signature.parameters {
            let param_ty = self.cl_type_of(param);

            sig.params.push(AbiParam::new(param_ty));
        }

        if func.signature.return_type.kind != lume_mir::TypeKind::Void {
            let ret_ty = self.cl_type_of(&func.signature.return_type);

            sig.returns.push(AbiParam::new(ret_ty));
        }

        sig
    }

    #[tracing::instrument(level = "DEBUG", skip_all, fields(func = %func.name), err)]
    fn define_function(
        &mut self,
        func: &lume_mir::Function,
        ctx: &mut cranelift::codegen::Context,
        builder_ctx: &mut FunctionBuilderContext,
    ) -> Result<()> {
        let declared_func = self.declared_funcs.get(&func.id).unwrap();
        ctx.func.signature = declared_func.sig.clone();

        let builder = FunctionBuilder::new(&mut ctx.func, builder_ctx);
        LowerFunction::new(self, func, builder).define();

        tracing::debug!(name: "lowered_func", name = %func.name, function = %ctx.func);

        let verify_flags = settings::Flags::new(settings::builder());
        if let Err(err) = verify_function(&ctx.func, &verify_flags) {
            let diagnostic = SimpleDiagnostic::new(format!("function verification failed ({})", func.name))
                .add_cause(SimpleDiagnostic::new(err.to_string()));

            return Err(diagnostic.into());
        }

        if let Err(err) = self.module_mut().define_function(declared_func.id, ctx) {
            tracing::error!(name: "verify", "error caused by function:\n{}", ctx.func);

            // Displaying verifier errors directly gives a really useless error, so to
            // actually know the issue, we're using the debug output of the error in the error.
            let diagnostic = SimpleDiagnostic::new(format!("function verification failed ({})", func.name))
                .add_cause(SimpleDiagnostic::new(format!("{err:#?}")));

            return Err(diagnostic.into());
        }

        Ok(())
    }

    pub(crate) fn declare_static_data_ctx(&self, key: &str, ctx: &DataDescription) -> DataId {
        if let Some(global) = self.static_data.read().unwrap().get(key) {
            *global
        } else {
            let len = self.static_data.read().unwrap().len();
            let name = format!("@__SYM_STATIC_{len}");

            let data_id = self
                .module_mut()
                .declare_data(&name, Linkage::Local, false, false)
                .unwrap();

            self.static_data.try_write().unwrap().insert(key.to_owned(), data_id);

            self.module_mut().define_data(data_id, ctx).unwrap();

            data_id
        }
    }

    pub(crate) fn declare_static_data(&self, key: &str, value: &[u8]) -> DataId {
        let mut data_ctx = DataDescription::new();
        data_ctx.set_align(8);
        data_ctx.set_used(true);

        data_ctx.define(value.to_vec().into_boxed_slice());

        self.declare_static_data_ctx(key, &data_ctx)
    }

    pub(crate) fn reference_static_data(&self, key: &str) -> Option<DataId> {
        self.static_data.read().unwrap().get(key).copied()
    }

    pub(crate) fn declare_static_string(&self, value: &str) -> DataId {
        self.declare_static_data(value, value.as_bytes())
    }
}

trait MapModuleResult<T> {
    fn map_error(self) -> T;
}

impl<T> MapModuleResult<error_snippet::Result<T>> for cranelift_module::ModuleResult<T> {
    fn map_error(self) -> error_snippet::Result<T> {
        self.map_err(error_snippet::IntoDiagnostic::into_diagnostic)
    }
}

struct LowerFunction<'ctx> {
    backend: &'ctx CraneliftBackend<'ctx>,
    func: &'ctx lume_mir::Function,

    builder: FunctionBuilder<'ctx>,
    variables: IndexMap<RegisterId, Variable>,
    variable_types: IndexMap<RegisterId, Type>,
    parameters: IndexMap<RegisterId, Value>,
    slots: IndexMap<SlotId, StackSlot>,
    blocks: IndexMap<lume_mir::BasicBlockId, Block>,
}

impl<'ctx> LowerFunction<'ctx> {
    pub fn new(
        backend: &'ctx CraneliftBackend<'ctx>,
        func: &'ctx lume_mir::Function,
        builder: FunctionBuilder<'ctx>,
    ) -> Self {
        Self {
            backend,
            func,
            builder,
            variables: IndexMap::new(),
            variable_types: IndexMap::new(),
            parameters: IndexMap::new(),
            slots: IndexMap::new(),
            blocks: IndexMap::new(),
        }
    }

    pub fn define(mut self) {
        // Allocate all blocks, so they can be referenced by earlier blocks
        for (idx, block) in self.func.blocks.iter().enumerate() {
            if idx == 0 {
                self.cg_block_alloc_entry(block);
            } else {
                self.cg_block_alloc(block);
            }
        }

        for block in &self.func.blocks {
            self.cg_block_in(block);
        }

        self.builder.seal_all_blocks();
        self.builder.finalize();
    }

    pub(crate) fn declared_func(&self, id: lume_mir::FunctionId) -> &DeclaredFunction {
        self.backend.declared_funcs.get(&id).unwrap()
    }

    pub(crate) fn get_func(&mut self, id: cranelift_module::FuncId) -> codegen::ir::FuncRef {
        self.backend.module_mut().declare_func_in_func(id, self.builder.func)
    }

    #[tracing::instrument(level = "TRACE", skip(self))]
    pub(crate) fn seal_block(&mut self, id: lume_mir::BasicBlockId) {
        let cg_block = *self.blocks.get(&id).unwrap();

        self.builder.seal_block(cg_block);
    }

    #[tracing::instrument(level = "TRACE", skip(self))]
    pub(crate) fn declare_var(&mut self, register: RegisterId, ty: Type) -> Variable {
        let var = self.builder.declare_var(ty);

        self.variables.insert(register, var);
        self.variable_types.insert(register, ty);

        tracing::debug!("declare_var {register}[{ty}] = {var}");

        var
    }

    pub(crate) fn retrieve_var(&self, register: RegisterId) -> Variable {
        *self.variables.get(&register).unwrap()
    }

    pub(crate) fn use_var(&mut self, register: RegisterId) -> Value {
        if let Some(param) = self.parameters.get(&register) {
            return *param;
        }

        let var = self.retrieve_var(register);
        self.builder.use_var(var)
    }

    pub(crate) fn load_var(&mut self, register: RegisterId) -> Value {
        let val = self.use_var(register);
        let ty = self.retrieve_var_type(register);

        self.builder.ins().load(ty, MemFlags::new(), val, Offset32::new(0))
    }

    #[tracing::instrument(level = "TRACE", skip(self), fields(func = %self.func.name))]
    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    pub(crate) fn load_field(&mut self, register: RegisterId, field: usize, offset: usize) -> Value {
        let ptr = self.use_var(register);
        let field_ty = self.retrieve_field_type(register, field);

        tracing::debug!(%ptr, %field_ty);

        self.builder.ins().load(field_ty, MemFlags::new(), ptr, offset as i32)
    }

    pub(crate) fn retrieve_var_type(&self, register: RegisterId) -> Type {
        *self.variable_types.get(&register).unwrap()
    }

    pub(crate) fn retrieve_load_type(&self, register: RegisterId) -> Type {
        let reg_ty = self.func.registers.register_ty(register);
        let lume_mir::TypeKind::Pointer { elemental } = &reg_ty.kind else {
            panic!("bug!: attempting to load non-pointer register");
        };

        self.backend.cl_type_of(elemental)
    }

    #[tracing::instrument(level = "TRACE", skip(self), fields(func = %self.func.name))]
    pub(crate) fn retrieve_field_type(&self, register: RegisterId, index: usize) -> Type {
        let reg_ty = self.func.registers.register_ty(register);
        let lume_mir::TypeKind::Pointer { elemental } = &reg_ty.kind else {
            panic!("bug!: attempting to load non-pointer register");
        };

        let lume_mir::TypeKind::Struct { fields } = &elemental.kind else {
            panic!("bug!: attempting to load field from non-struct register");
        };

        let field = &fields[index];
        tracing::debug!(%reg_ty, %field, index);

        self.backend.cl_type_of(field)
    }

    pub(crate) fn retrieve_slot(&self, slot: SlotId) -> StackSlot {
        *self.slots.get(&slot).unwrap()
    }

    pub(crate) fn icmp(&mut self, cmp: IntCC, x: &lume_mir::Operand, y: &lume_mir::Operand) -> Value {
        let x_val = self.cg_operand(x);
        let y_val = self.cg_operand(y);

        self.builder.ins().icmp(cmp, x_val, y_val)
    }

    pub(crate) fn iadd(&mut self, x: &lume_mir::Operand, y: &lume_mir::Operand) -> Value {
        let x_val = self.cg_operand(x);
        let y_val = self.cg_operand(y);

        self.builder.ins().iadd(x_val, y_val)
    }

    pub(crate) fn isub(&mut self, x: &lume_mir::Operand, y: &lume_mir::Operand) -> Value {
        let x_val = self.cg_operand(x);
        let y_val = self.cg_operand(y);

        self.builder.ins().isub(x_val, y_val)
    }

    pub(crate) fn imul(&mut self, x: &lume_mir::Operand, y: &lume_mir::Operand) -> Value {
        let x_val = self.cg_operand(x);
        let y_val = self.cg_operand(y);

        self.builder.ins().imul(x_val, y_val)
    }

    pub(crate) fn idiv(&mut self, x: &lume_mir::Operand, y: &lume_mir::Operand) -> Value {
        let x_val = self.cg_operand(x);
        let y_val = self.cg_operand(y);

        self.builder.ins().sdiv(x_val, y_val)
    }

    pub(crate) fn and(&mut self, x: &lume_mir::Operand, y: &lume_mir::Operand) -> Value {
        let x_val = self.cg_operand(x);
        let y_val = self.cg_operand(y);

        self.builder.ins().band(x_val, y_val)
    }

    pub(crate) fn or(&mut self, x: &lume_mir::Operand, y: &lume_mir::Operand) -> Value {
        let x_val = self.cg_operand(x);
        let y_val = self.cg_operand(y);

        self.builder.ins().bor(x_val, y_val)
    }

    pub(crate) fn xor(&mut self, x: &lume_mir::Operand, y: &lume_mir::Operand) -> Value {
        let x_val = self.cg_operand(x);
        let y_val = self.cg_operand(y);

        self.builder.ins().bxor(x_val, y_val)
    }

    pub(crate) fn icast(&mut self, reg: RegisterId, to: u8) -> Value {
        let lume_mir::TypeKind::Integer { bits: from, signed } = self.func.registers.register_ty(reg).kind else {
            panic!("bug!: attempted to use icast on non-integer register");
        };

        // Cast from larger int to smaller int (ex. i64 -> i32)
        if from > to {
            let reduced_ty = types::Type::int(u16::from(to)).unwrap();
            let value = self.use_var(reg);

            return self.builder.ins().ireduce(reduced_ty, value);
        }

        let extended_ty = types::Type::int(u16::from(to)).unwrap();
        let value = self.use_var(reg);

        if signed {
            self.builder.ins().sextend(extended_ty, value)
        } else {
            self.builder.ins().uextend(extended_ty, value)
        }
    }

    pub(crate) fn fcmp(&mut self, cmp: FloatCC, x: &lume_mir::Operand, y: &lume_mir::Operand) -> Value {
        let x_val = self.cg_operand(x);
        let y_val = self.cg_operand(y);

        self.builder.ins().fcmp(cmp, x_val, y_val)
    }

    pub(crate) fn fadd(&mut self, x: &lume_mir::Operand, y: &lume_mir::Operand) -> Value {
        let x_val = self.cg_operand(x);
        let y_val = self.cg_operand(y);

        self.builder.ins().fadd(x_val, y_val)
    }

    pub(crate) fn fsub(&mut self, x: &lume_mir::Operand, y: &lume_mir::Operand) -> Value {
        let x_val = self.cg_operand(x);
        let y_val = self.cg_operand(y);

        self.builder.ins().fsub(x_val, y_val)
    }

    pub(crate) fn fmul(&mut self, x: &lume_mir::Operand, y: &lume_mir::Operand) -> Value {
        let x_val = self.cg_operand(x);
        let y_val = self.cg_operand(y);

        self.builder.ins().fmul(x_val, y_val)
    }

    pub(crate) fn fdiv(&mut self, x: &lume_mir::Operand, y: &lume_mir::Operand) -> Value {
        let x_val = self.cg_operand(x);
        let y_val = self.cg_operand(y);

        self.builder.ins().fdiv(x_val, y_val)
    }

    pub(crate) fn fcast(&mut self, reg: RegisterId, to: u8) -> Value {
        let lume_mir::TypeKind::Float { bits: from } = self.func.registers.register_ty(reg).kind else {
            panic!("bug!: attempted to use fcast on non-float register");
        };

        let value = self.use_var(reg);
        let cast_ty = match to {
            32 => types::F32,
            64 => types::F64,
            _ => unreachable!(),
        };

        if from < to {
            self.builder.ins().fpromote(cast_ty, value)
        } else {
            self.builder.ins().fdemote(cast_ty, value)
        }
    }

    #[allow(clippy::cast_lossless)]
    pub(crate) fn alloc(&mut self, ty: types::Type) -> Value {
        self.alloca(ty.bytes() as usize)
    }

    #[allow(clippy::cast_lossless, clippy::cast_possible_wrap)]
    pub(crate) fn alloca(&mut self, size: usize) -> Value {
        let malloc_id = self.backend.intrinsics.malloc;
        let malloc = self.get_func(malloc_id);

        let size = self.builder.ins().iconst(types::I64, size as i64);
        let call = self.builder.ins().call(malloc, &[size]);

        self.builder.inst_results(call)[0]
    }

    pub(crate) fn declare_data_in_func(&mut self, data: DataId) -> GlobalValue {
        self.backend.module_mut().declare_data_in_func(data, self.builder.func)
    }

    pub(crate) fn reference_static_data(&mut self, key: &str) -> Option<Value> {
        let data_id = self.backend.reference_static_data(key)?;
        let local_id = self.declare_data_in_func(data_id);

        Some(self.builder.ins().symbol_value(self.backend.cl_ptr_type(), local_id))
    }

    pub(crate) fn reference_static_string(&mut self, value: &str) -> Value {
        let data_id = self.backend.declare_static_string(value);
        let local_id = self.declare_data_in_func(data_id);

        self.builder.ins().symbol_value(self.backend.cl_ptr_type(), local_id)
    }

    pub(crate) fn call(&mut self, func: lume_mir::FunctionId, args: &[lume_mir::Operand]) -> &[Value] {
        let cl_func_id = self.backend.declared_funcs.get(&func).unwrap().id;
        let cl_func_ref = self.get_func(cl_func_id);

        let args = args.iter().map(|arg| self.cg_operand(arg)).collect::<Vec<_>>();
        let call = self.builder.ins().call(cl_func_ref, &args);

        self.builder.inst_results(call)
    }

    pub(crate) fn branch(&mut self, call: &BlockBranchSite) {
        let cl_block = *self.blocks.get(&call.block).unwrap();
        let args = call
            .arg_operands()
            .map(|arg| BlockArg::Value(self.cg_operand(&arg)))
            .collect::<Vec<_>>();

        self.builder.ins().jump(cl_block, args.iter().as_ref());
    }

    pub(crate) fn conditional_branch(
        &mut self,
        cond: Value,
        then_block: &BlockBranchSite,
        else_block: &BlockBranchSite,
    ) {
        let cl_then_block = *self.blocks.get(&then_block.block).unwrap();
        let cl_else_block = *self.blocks.get(&else_block.block).unwrap();

        let then_args = then_block
            .arg_operands()
            .map(|arg| BlockArg::Value(self.cg_operand(&arg)))
            .collect::<Vec<_>>();

        let else_args = else_block
            .arg_operands()
            .map(|arg| BlockArg::Value(self.cg_operand(&arg)))
            .collect::<Vec<_>>();

        self.builder.ins().brif(
            cond,
            cl_then_block,
            then_args.iter().as_ref(),
            cl_else_block,
            else_args.iter().as_ref(),
        );
    }
}
