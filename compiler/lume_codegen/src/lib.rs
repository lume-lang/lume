pub mod dwarf;
pub(crate) mod inst;
pub(crate) mod metadata;
pub(crate) mod ty;
pub(crate) mod unwind;
pub(crate) mod value;

use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

use cranelift::codegen::ir::{BlockArg, GlobalValue, StackSlot};
use cranelift::codegen::verify_function;
use cranelift::prelude::*;
use cranelift_codegen::ir::SourceLoc;
use cranelift_codegen::isa::TargetIsa;
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule, ObjectProduct};
use gimli::write::{Address, Writer};
use indexmap::{IndexMap, IndexSet};
use lume_errors::{MapDiagnostic, Result, SimpleDiagnostic};
use lume_mir::{BlockBranchSite, ModuleMap, RegisterId, SlotId};
use lume_session::DebugInfo;
use lume_span::NodeId;
use lume_span::source::Location;
use object::write::Relocation;
use object::{RelocationEncoding, RelocationFlags};

use crate::dwarf::{DebugRelocName, RootDebugContext, WriterRelocate};
use crate::unwind::RootUnwindContext;

pub const GC_ALLOC: &str = "std::mem::GC::alloc";
pub const GC_STEP: &str = "std::mem::GC::step";

#[derive(Debug, Clone)]
struct DeclaredFunction {
    pub id: cranelift_module::FuncId,
    pub sig: Signature,
}

#[derive(Debug, Clone)]
struct FunctionMetadata {
    pub total_size: usize,
    pub stack_locations: Vec<(usize, usize, Vec<usize>)>,
}

#[derive(Debug, Clone)]
struct IntrinsicFunctions {
    pub gc_step: cranelift_module::FuncId,
    pub gc_alloc: cranelift_module::FuncId,
}

/// Compiles the given MIR map and returns the fully-compiled bytecode of the
/// resulting object file.
///
/// # Errors
///
/// Returns `Err` if the compiler returned an error while compiling the MIR.
#[tracing::instrument(level = "DEBUG", skip_all, err)]
pub fn generate<'ctx>(mir: ModuleMap) -> Result<Vec<u8>> {
    let object = CraneliftBackend::new(mir)?.generate()?;

    object.emit().map_diagnostic()
}

pub(crate) struct CraneliftBackend {
    context: ModuleMap,
    module: Option<Rc<RwLock<ObjectModule>>>,

    declared_funcs: IndexMap<NodeId, DeclaredFunction>,
    intrinsics: IntrinsicFunctions,

    static_data: RwLock<HashMap<String, DataId>>,
    location_indices: RwLock<IndexSet<Location>>,
    isa: Arc<dyn TargetIsa>,
    flags: settings::Flags,
}

impl CraneliftBackend {
    pub fn new(context: ModuleMap) -> Result<Self> {
        let mut settings = cranelift::codegen::settings::builder();
        settings.set("preserve_frame_pointers", "true").unwrap();
        settings.set("unwind_info", "true").unwrap();
        settings.set("is_pic", "true").unwrap();

        let flags = settings::Flags::new(settings);
        let isa = cranelift_native::builder().unwrap().finish(flags.clone()).unwrap();
        let builder = ObjectBuilder::new(
            isa.clone(),
            context.package.name.clone(),
            cranelift_module::default_libcall_names(),
        )
        .map_diagnostic()?;

        let mut module = ObjectModule::new(builder);
        let ptr_ty = module.target_config().pointer_type();

        let intrinsics = IntrinsicFunctions {
            gc_step: import_function(&mut module, GC_STEP, &[], None)?,
            gc_alloc: import_function(&mut module, GC_ALLOC, &[ptr_ty, ptr_ty], Some(ptr_ty))?,
        };

        Ok(Self {
            context,
            module: Some(Rc::new(RwLock::new(module))),
            declared_funcs: IndexMap::new(),
            intrinsics,
            flags,
            isa,
            static_data: RwLock::new(HashMap::new()),
            location_indices: RwLock::new(IndexSet::new()),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn generate(&mut self) -> lume_errors::Result<ObjectProduct> {
        let functions = std::mem::take(&mut self.context.functions);

        for func in functions.values() {
            let (func_id, sig) = self.declare_function(func)?;

            self.declared_funcs
                .insert(func.id, DeclaredFunction { id: func_id, sig });
        }

        self.context.functions = functions;
        self.declare_type_metadata();

        let mut ctx = self.module_mut().make_context();
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut function_metadata = HashMap::new();

        let mut debug_ctx = if self.context.options.debug_info > DebugInfo::None {
            Some(RootDebugContext::new(&self.context, self.module().isa()))
        } else {
            None
        };

        let mut unwind_ctx = RootUnwindContext::new(self.isa.clone());

        for func in self.context.functions.values() {
            if func.signature.external {
                continue;
            }

            if let Some(debug_ctx) = debug_ctx.as_mut() {
                debug_ctx.declare_function(func);
            }

            self.define_function(func, &mut ctx, &mut builder_ctx, debug_ctx.as_mut())?;

            unwind_ctx.add_function(func.id, &ctx);

            let compiled_code = ctx.compiled_code().expect("expected context to be compiled");
            let code_len = compiled_code.buffer.total_size() as usize;

            let mut stack_locations = Vec::new();
            for (offset, length, map) in compiled_code.buffer.user_stack_maps() {
                let refs = map.entries().map(|(_, offset)| offset as usize).collect();

                stack_locations.push((*offset as usize, *length as usize, refs));
            }

            function_metadata.insert(func.id, FunctionMetadata {
                total_size: code_len,
                stack_locations,
            });

            self.module().clear_context(&mut ctx);
        }

        if let Some(debug_ctx) = debug_ctx.as_mut() {
            debug_ctx.populate_function_units(self, &function_metadata)?;
        }

        let module: ObjectModule = Rc::into_inner(self.module.take().unwrap())
            .unwrap()
            .into_inner()
            .unwrap();

        let mut object = module.finish();
        self.declare_stack_maps(&mut object, function_metadata)?;

        if let Some(debug_ctx) = debug_ctx.take() {
            debug_ctx.finish(&mut object)?;
        }

        unwind_ctx.write(self, &mut object)?;

        Ok(object)
    }

    #[track_caller]
    pub(crate) fn module(&self) -> RwLockReadGuard<'_, ObjectModule> {
        self.module.as_ref().unwrap().try_read().unwrap()
    }

    #[track_caller]
    pub(crate) fn module_mut(&self) -> RwLockWriteGuard<'_, ObjectModule> {
        self.module.as_ref().unwrap().try_write().unwrap()
    }

    #[tracing::instrument(level = "INFO", skip_all, fields(func = %func.name))]
    fn declare_function(&mut self, func: &lume_mir::Function) -> Result<(cranelift_module::FuncId, Signature)> {
        let sig = self.create_signature_of(&func.signature);

        let linkage = if func.signature.external {
            cranelift_module::Linkage::Import
        } else {
            cranelift_module::Linkage::Export
        };

        let func_id = self
            .module_mut()
            .declare_function(&func.name, linkage, &sig)
            .map_diagnostic()?;

        Ok((func_id, sig))
    }

    #[tracing::instrument(level = "TRACE", skip_all)]
    fn create_signature_of(&self, signature: &lume_mir::Signature) -> Signature {
        let mut sig = self.module().make_signature();

        for param in &signature.parameters {
            let param_ty = self.cl_type_of(&param.ty);

            sig.params.push(AbiParam::new(param_ty));
        }

        if signature.return_type.kind != lume_mir::TypeKind::Void {
            let ret_ty = self.cl_type_of(&signature.return_type);

            sig.returns.push(AbiParam::new(ret_ty));
        }

        sig
    }

    #[tracing::instrument(level = "INFO", skip_all, fields(func = %func.name), err)]
    fn define_function(
        &self,
        func: &lume_mir::Function,
        ctx: &mut cranelift::codegen::Context,
        builder_ctx: &mut FunctionBuilderContext,
        mut debug_ctx: Option<&mut RootDebugContext>,
    ) -> Result<()> {
        let declared_func = self.declared_funcs.get(&func.id).unwrap();
        ctx.func.signature = declared_func.sig.clone();

        let builder = FunctionBuilder::new(&mut ctx.func, builder_ctx);
        LowerFunction::new(self, func, builder).define();

        lume_trace::debug!(name: "lowered_func", name = %func.name, function = %ctx.func);

        {
            // We have to pass the same flags to the verifier function as we used
            // to create the function. Otherwise, the verifier might complain about
            // missing ISA, disabled flags or similar.
            let module = self.module();

            let foi = settings::FlagsOrIsa {
                flags: &self.flags,
                isa: Some(module.isa()),
            };

            if let Err(err) = verify_function(&ctx.func, foi) {
                let diagnostic = SimpleDiagnostic::new(format!("function verification failed ({})", func.name))
                    .add_cause(SimpleDiagnostic::new(err.to_string()));

                return Err(diagnostic.into());
            }
        }

        if let Err(err) = self.module_mut().define_function(declared_func.id, ctx) {
            tracing::error!(name: "verify", "error caused by function:\n{}", ctx.func);

            // Displaying verifier errors directly gives a really useless error, so to
            // actually know the issue, we're using the debug output of the error in the
            // error.
            let diagnostic = SimpleDiagnostic::new(format!("function verification failed ({})", func.name))
                .add_cause(SimpleDiagnostic::new(format!("{err:#?}")));

            return Err(diagnostic.into());
        }

        if let Some(debug_ctx) = debug_ctx.as_mut() {
            debug_ctx.define_function(func.id, &ctx);
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

    pub(crate) fn calculate_source_loc(&self, loc: Location) -> SourceLoc {
        let (idx, _) = self.location_indices.try_write().unwrap().insert_full(loc);

        SourceLoc::new(idx as u32)
    }

    pub(crate) fn lookup_source_loc(&self, loc: SourceLoc) -> Location {
        let map = self.location_indices.try_read().unwrap();

        map.get_index(loc.bits() as usize).unwrap().clone()
    }
}

impl CraneliftBackend {
    /// Write the function stack maps to a symbol within the given object, so we
    /// can read them within the GC at runtime.
    ///
    /// The content of the symbol (simply named `__STACK_MAPS`) is structured
    /// like so:
    /// ```
    /// // Data structure of the `__STACK_MAPS` symbol itself
    /// Symbol:
    ///     // List of stack maps within the program - one per applicable function.
    ///     nfunc       u64
    ///     funcs       [StackMap; nfunc]
    ///
    /// // Data structure for a single function, outlining all the stack
    /// // locations which can contain GC references.
    /// StackMap:
    ///     // Memory address of the function which the stack map is referencing.
    ///     addr        u64
    ///
    ///     // Size of the function (in bytes).
    ///     size        u64
    ///
    ///     // List of stack locations within the function.
    ///     nloc        u64
    ///     locs        [StackLocation; nloc]
    ///
    /// StackLocation:
    ///     // Range in which the stack location is valid, relative to the
    ///     // start of the function.
    ///     start       u64
    ///     size        u64
    ///
    ///     // List of offsets relative to the stack pointer, which contains a
    ///     // pointer to a GC reference.
    ///     noffset     u64
    ///     offsets     [u64; noffset]
    /// ```
    fn declare_stack_maps(
        &self,
        product: &mut ObjectProduct,
        function_metadata: HashMap<NodeId, FunctionMetadata>,
    ) -> Result<()> {
        let endian = match self.isa.endianness() {
            cranelift_codegen::ir::Endianness::Big => gimli::RunTimeEndian::Big,
            cranelift_codegen::ir::Endianness::Little => gimli::RunTimeEndian::Little,
        };

        let mut nfunc = 0_u64;
        let mut stack_maps = WriterRelocate::new(endian);

        for (def, func) in &self.declared_funcs {
            let func_def = self.context.functions.get(def).unwrap();
            if func_def.signature.external {
                continue;
            }

            let Some(metadata) = function_metadata.get(def) else {
                continue;
            };

            let addr = address_for_func(func.id);

            // Write the address range of the function declaration
            stack_maps
                .write_address(addr, self.isa.pointer_bytes())
                .map_diagnostic()?;

            stack_maps.write_u64(metadata.total_size as u64).map_diagnostic()?;

            stack_maps
                .write_u64(metadata.stack_locations.len() as u64)
                .map_diagnostic()?;

            for (start, len, stack_offsets) in &metadata.stack_locations {
                stack_maps.write_u64(*start as u64).map_diagnostic()?;
                stack_maps.write_u64(*len as u64).map_diagnostic()?;

                stack_maps.write_u64(stack_offsets.len() as u64).map_diagnostic()?;

                for stack_offset in stack_offsets {
                    stack_maps.write_u64(*stack_offset as u64).map_diagnostic()?;
                }
            }

            nfunc += 1;
        }

        let section_id = product.object.section_id(object::write::StandardSection::Data);

        let section_offset = product
            .object
            .append_section_data(section_id, &u64::to_ne_bytes(nfunc), 8);

        // Size of the symbol must include the function found (`nfunc`).
        let symbol_size = size_of::<u64>() + stack_maps.writer.slice().len();

        product.object.add_symbol(object::write::Symbol {
            name: b"__STACK_MAPS".to_vec(),
            value: section_offset,
            size: symbol_size as u64,
            kind: object::write::SymbolKind::Data,
            scope: object::write::SymbolScope::Linkage,
            weak: false,
            section: object::write::SymbolSection::Section(section_id),
            flags: object::SymbolFlags::None,
        });

        // Write the rest of the symbol content.
        let content_offset = product
            .object
            .append_section_data(section_id, stack_maps.writer.slice(), 1);

        for reloc in &stack_maps.relocs {
            let symbol = match reloc.name {
                DebugRelocName::Section(_) => unreachable!(),
                DebugRelocName::Symbol(id) => {
                    let id = id.try_into().unwrap();

                    if id & 1 << 31 == 0 {
                        product.function_symbol(FuncId::from_u32(id))
                    } else {
                        product.data_symbol(DataId::from_u32(id & !(1 << 31)))
                    }
                }
            };

            product
                .object
                .add_relocation(section_id, Relocation {
                    offset: content_offset + u64::from(reloc.offset),
                    symbol,
                    flags: RelocationFlags::Generic {
                        kind: reloc.kind,
                        encoding: RelocationEncoding::Generic,
                        size: reloc.size * 8,
                    },
                    addend: reloc.addend,
                })
                .unwrap();
        }

        Ok(())
    }
}

#[tracing::instrument(level = "TRACE", skip(module), err)]
fn import_function<TModule: Module>(
    module: &mut TModule,
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
        .map_diagnostic()?;

    Ok(func_id)
}

pub(crate) fn address_for_func(func_id: FuncId) -> Address {
    let symbol = func_id.as_u32();
    assert!(symbol & 1 << 31 == 0);

    Address::Symbol {
        symbol: symbol as usize,
        addend: 0,
    }
}

struct LowerFunction<'ctx> {
    backend: &'ctx CraneliftBackend,
    func: &'ctx lume_mir::Function,

    builder: FunctionBuilder<'ctx>,
    variables: IndexMap<RegisterId, Variable>,
    variable_types: IndexMap<RegisterId, lume_mir::Type>,
    parameters: IndexMap<RegisterId, Value>,
    slots: IndexMap<SlotId, StackSlot>,
    blocks: IndexMap<lume_mir::BasicBlockId, Block>,
}

impl<'ctx> LowerFunction<'ctx> {
    pub fn new(
        backend: &'ctx CraneliftBackend,
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
        self.set_srcloc(self.func.location.clone());

        // Allocate all blocks, so they can be referenced by earlier blocks
        for (idx, block) in self.func.blocks.values().enumerate() {
            if idx == 0 {
                self.cg_block_alloc_entry(block);
            } else {
                self.cg_block_alloc(block);
            }
        }

        for block in self.func.blocks.values() {
            self.cg_block_in(block);
        }

        self.builder.seal_all_blocks();
        self.builder.finalize();
    }

    pub(crate) fn get_func(&mut self, id: cranelift_module::FuncId) -> codegen::ir::FuncRef {
        self.backend.module_mut().declare_func_in_func(id, self.builder.func)
    }

    #[tracing::instrument(level = "TRACE", skip(self))]
    pub(crate) fn seal_block(&mut self, id: lume_mir::BasicBlockId) {
        let cg_block = *self.blocks.get(&id).unwrap();

        self.builder.seal_block(cg_block);
    }

    /// Inserts a conditional call for the garbage collection to trigger
    /// at the current builder location.
    ///
    /// Whether the garbage collector is actually triggered depends on when
    /// the last invocation occured and whether any memory actually needs to be
    /// collected.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub(crate) fn insert_gc_trigger(&mut self) {
        if self.func.signature.is_dropper {
            return;
        }

        let cl_gc_step = self.get_func(self.backend.intrinsics.gc_step);
        self.builder.ins().call(cl_gc_step, &[]);
    }

    #[tracing::instrument(level = "TRACE", skip(self))]
    pub(crate) fn declare_var(&mut self, register: RegisterId, ty: lume_mir::Type) -> Variable {
        let cg_ty = self.backend.cl_type_of(&ty);
        let var = self.builder.declare_var(cg_ty);

        lume_trace::debug!("declare_var {register}[{ty}] = {var}({cg_ty})");

        self.variables.insert(register, var);
        self.variable_types.insert(register, ty);

        var
    }

    pub(crate) fn retrieve_var(&self, register: RegisterId) -> Variable {
        *self.variables.get(&register).unwrap_or_else(|| {
            panic!(
                "should have register {register} present ({}, {})",
                self.func.name,
                self.func.current_block().id
            )
        })
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
        let ty = self.retrieve_load_type(register);

        lume_trace::debug!("loading {val} from {register}, type {ty}");

        self.builder.ins().load(ty, MemFlags::new(), val, 0)
    }

    #[tracing::instrument(level = "TRACE", skip(self), fields(func = %self.func.name))]
    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    pub(crate) fn load_field(&mut self, register: RegisterId, field: usize, offset: usize, ty: Type) -> Value {
        let ptr = self.use_var(register);

        lume_trace::debug!(%ptr, %ty, %register, field);

        self.builder.ins().load(ty, MemFlags::new(), ptr, offset as i32)
    }

    pub(crate) fn retrieve_load_type(&self, register: RegisterId) -> Type {
        let reg_ty = self.func.registers.register_ty(register);

        if let lume_mir::TypeKind::Union { .. } = &reg_ty.kind {
            return types::I8;
        }

        let lume_mir::TypeKind::Pointer { elemental } = &reg_ty.kind else {
            panic!("bug!: attempting to load non-pointer register");
        };

        if let lume_mir::TypeKind::Union { .. } = &elemental.kind {
            return types::I8;
        }

        self.backend.cl_type_of(elemental)
    }

    #[tracing::instrument(level = "TRACE", skip(self), fields(func = %self.func.name))]
    pub(crate) fn retrieve_field_type(&self, register: RegisterId, index: usize) -> Type {
        let reg_ty = self.func.registers.register_ty(register);

        if let lume_mir::TypeKind::Union { cases } = &reg_ty.kind {
            let case = cases
                .get(index)
                .expect("bug!: attempted to load union field out of bounds");

            return self.backend.cl_type_of(case);
        }

        let lume_mir::TypeKind::Pointer { elemental } = &reg_ty.kind else {
            panic!("bug!: attempting to load non-pointer register");
        };

        if let lume_mir::TypeKind::Union { .. } = &elemental.kind {
            return self.backend.cl_ptr_type();
        }

        let lume_mir::TypeKind::Struct { fields, .. } = &elemental.kind else {
            panic!("bug!: attempting to load field from non-struct register");
        };

        let field = &fields[index];
        lume_trace::debug!(%reg_ty, %field, index);

        self.backend.cl_type_of(field)
    }

    pub(crate) fn retrieve_slot(&self, slot: SlotId) -> StackSlot {
        *self.slots.get(&slot).unwrap()
    }

    pub(crate) fn set_srcloc(&mut self, loc: Location) {
        let src_loc = self.backend.calculate_source_loc(loc);

        self.builder.set_srcloc(src_loc);
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

    #[allow(clippy::cast_lossless, clippy::cast_possible_wrap)]
    pub(crate) fn alloca(&mut self, size: usize, metadata: Option<RegisterId>) -> Value {
        let alloc_id = self.backend.intrinsics.gc_alloc;
        let alloc = self.get_func(alloc_id);

        let metadata_arg = if let Some(metadata) = metadata {
            self.use_var(metadata)
        } else {
            self.builder.ins().iconst(self.backend.cl_ptr_type(), 0)
        };

        let size = self.builder.ins().iconst(types::I64, size as i64);
        let call = self.builder.ins().call(alloc, &[size, metadata_arg]);

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

    pub(crate) fn call(&mut self, func: NodeId, args: &[lume_mir::Operand]) -> &[Value] {
        let cl_func_id = self.backend.declared_funcs.get(&func).unwrap().id;
        let cl_func_ref = self.get_func(cl_func_id);

        let args = args.iter().map(|arg| self.cg_operand(arg)).collect::<Vec<_>>();

        self.insert_gc_trigger();

        let call = self.builder.ins().call(cl_func_ref, &args);
        self.builder.inst_results(call)
    }

    pub(crate) fn indirect_call(
        &mut self,
        ptr: RegisterId,
        sig: lume_mir::Signature,
        args: &[lume_mir::Operand],
    ) -> &[Value] {
        let cl_func_sig = self.backend.create_signature_of(&sig);
        let cl_sig_ref = self.builder.import_signature(cl_func_sig);

        let callee = self.use_var(ptr);
        let args = args.iter().map(|arg| self.cg_operand(arg)).collect::<Vec<_>>();

        self.insert_gc_trigger();

        let call = self.builder.ins().call_indirect(cl_sig_ref, callee, &args);
        self.builder.inst_results(call)
    }

    pub(crate) fn branch(&mut self, call: &BlockBranchSite) {
        let cl_block = *self.blocks.get(&call.block).unwrap();
        let args = call
            .arguments
            .iter()
            .map(|arg| BlockArg::Value(self.cg_operand(arg)))
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
            .arguments
            .iter()
            .map(|arg| BlockArg::Value(self.cg_operand(arg)))
            .collect::<Vec<_>>();

        let else_args = else_block
            .arguments
            .iter()
            .map(|arg| BlockArg::Value(self.cg_operand(arg)))
            .collect::<Vec<_>>();

        self.builder.ins().brif(
            cond,
            cl_then_block,
            then_args.iter().as_ref(),
            cl_else_block,
            else_args.iter().as_ref(),
        );
    }

    pub(crate) fn switch(&mut self, operand: Value, arms: &[(i64, BlockBranchSite)], fallback: &BlockBranchSite) {
        let mut switch = cranelift::frontend::Switch::new();
        let fallback = *self.blocks.get(&fallback.block).unwrap();

        for (index, block) in arms {
            let arm_block = *self.blocks.get(&block.block).unwrap();

            #[allow(clippy::cast_sign_loss)]
            switch.set_entry(u128::from(*index as u64), arm_block);
        }

        switch.emit(&mut self.builder, operand, fallback);
    }
}
