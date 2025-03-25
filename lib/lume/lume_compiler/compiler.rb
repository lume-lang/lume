# frozen_string_literal: true

require 'llvm/core'
require 'llvm/execution_engine'
require 'llvm/linker'
require 'llvm/transforms/scalar'

require 'lume/lume_compiler/ir_visitor'
require 'lume/lume_compiler/builder'

module Lume
  class Compiler # :nodoc:
    MAIN_NAME = 'main'

    def initialize
      # Initialize the LLVM backend
      LLVM.init_jit
    end

    # Compiles all the modules from the given compiler context into LLVM IR, which
    # can be executed or dumped to an object file.
    #
    # @param context [CompilerContext] The compiler context containing the MIR.
    #
    # @return [void]
    def compile!(context)
      # Generate LLVM IR from each of the modules within the context
      context.modules.each { |mod| compile_module!(mod) }
    end

    # Compiles a single module from the given compiler context into LLVM IR, which
    # can be executed or dumped to an object file.
    #
    # @param module [Lume::Module] The module to compile.
    #
    # @return [void]
    def compile_module!(mod)
      # Generate the LLVM IR and add it to the LLVM module
      mod.llvm_module = NodeVisitor.visit_module(mod)
    end

    # Optimizes the module using LLVM's optimization passes.
    #
    # @param context [CompilerContext] The compiler context containing the MIR.
    #
    # @return [void]
    def optimize!(context)
      pass_manager = LLVM::PassManager.new

      context.modules.each do |mod|
        mod.llvm_module.optimize_with!(pass_manager)
      end
    end

    # Dumps the LLVM module to `stdout`.
    #
    # @return [void]
    def dump!
      @module.dump
      @module.verify!
    end

    # Emits the LLVM module as an object file to the given file.
    #
    # @param filename [String] The name of the file to emit the module to.
    #
    # @return [void]
    def emit(filename)
      engine = LLVM::JITCompiler.new(@module)
      target_machine = engine.target_machine

      target_machine.emit(@module, filename, :object)
    end

    # Evaluates / executes the compiled module, as if it were a compiled executable.
    #
    # @param args [Array] The arguments to pass to the module's `main` function.
    #
    # @return [Integer]
    def evaluate(*args)
      engine = LLVM::JITCompiler.new(@module)

      argc = args.length
      argv = nil if args.empty?

      engine.run_function(engine.functions[MAIN_NAME], argc, argv)
    end
  end
end
