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

    attr_reader :module

    def initialize
      # Initialize the LLVM backend
      initialize_codegen!

      @module = LLVM::Module.new('lume')
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
    # @param module [Lume::Parser::Module] The module to compile.
    #
    # @return [void]
    def compile_module!(mod)
      # Generate the LLVM IR and add it to the LLVM module
      llvm_module = NodeVisitor.visit_module(mod)

      # Link the current LLVM module into the main module
      llvm_module.link_into(@module)
    end

    # Optimizes the module using LLVM's optimization passes.
    #
    # @return [void]
    def optimize!
      ensure_initialized!

      pass_manager = LLVM::PassManager.new
      pass_manager.run(@module)

      @optimized = true
    end

    # Dumps the LLVM module to `stdout`.
    #
    # @return [void]
    def dump!
      ensure_initialized!

      @module.dump
      @module.verify!
    end

    # Emits the LLVM module as an object file to the given file.
    #
    # @param filename [String] The name of the file to emit the module to.
    #
    # @return [void]
    def emit(filename)
      ensure_initialized!

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
      ensure_initialized!

      engine = LLVM::JITCompiler.new(@module)

      argc = args.length
      argv = nil if args.empty?

      engine.run_function(engine.functions[MAIN_NAME], argc, argv)
    end

    # Returns whether the compiler has been initialized.
    #
    # @return [Boolean]
    def initialized?
      @initialized
    end

    # Returns whether the compiled LLVM IR has been optimized.
    #
    # @return [Boolean]
    def optimized?
      @optimized
    end

    private

    # initializes the backend components for Lume (such as LLVM, JIT, etc.)
    #
    # @return [void]
    def initialize_codegen!
      ensure_uninitialized!

      LLVM.init_jit

      @initialized = true
      @optimized = false
    end

    def ensure_initialized!
      raise StandardError, 'Compiler has not been initialized, yet' unless initialized?
    end

    def ensure_uninitialized!
      raise StandardError, 'Compiler has already been initialized' if initialized?
    end
  end
end
