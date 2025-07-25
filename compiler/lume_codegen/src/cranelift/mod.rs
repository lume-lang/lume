pub(crate) mod inst;
pub(crate) mod ty;
pub(crate) mod value;

use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

use cranelift::{
    codegen::ir::{BlockArg, immediates::Offset32},
    prelude::*,
};
use cranelift_module::Module;
use cranelift_object::{ObjectBuilder, ObjectModule};
use indexmap::IndexMap;
use lume_errors::Result;
use lume_mir::{BlockBranchSite, RegisterId};

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
}

impl<'ctx> Backend<'ctx> for CraneliftBackend<'ctx> {
    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn initialize(&mut self) -> lume_errors::Result<()> {
        Ok(())
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn generate(&mut self) -> lume_errors::Result<CompiledModule> {
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

    #[tracing::instrument(level = "TRACE", skip_all, fields(func = %func.name), err)]
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

        self.module_mut().define_function(declared_func.id, ctx).map_error()?;

        Ok(())
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

    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    pub(crate) fn load_field(&mut self, register: RegisterId, field: usize, offset: usize) -> Value {
        let ptr = self.use_var(register);
        let field_ty = self.retrieve_field_type(register, field);

        self.builder
            .ins()
            .load(field_ty, MemFlags::new(), ptr, Offset32::new(offset as i32))
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

    pub(crate) fn retrieve_field_type(&self, register: RegisterId, index: usize) -> Type {
        let reg_ty = self.func.registers.register_ty(register);
        let lume_mir::TypeKind::Pointer { elemental } = &reg_ty.kind else {
            panic!("bug!: attempting to load non-pointer register");
        };

        let lume_mir::TypeKind::Struct { properties } = &elemental.kind else {
            panic!("bug!: attempting to load field from non-struct register");
        };

        self.backend.cl_type_of(&properties[index])
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

    #[allow(clippy::cast_lossless)]
    pub(crate) fn alloc(&mut self, ty: types::Type) -> Value {
        let malloc_id = self.backend.intrinsics.malloc;
        let malloc = self.get_func(malloc_id);

        let size = self.builder.ins().iconst(types::I64, ty.bytes() as i64);
        let call = self.builder.ins().call(malloc, &[size]);

        self.builder.inst_results(call)[0]
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
