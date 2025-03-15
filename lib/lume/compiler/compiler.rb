# frozen_string_literal: true

require 'llvm/core'
require 'llvm/execution_engine'
require 'llvm/transforms/scalar'

require_relative 'ir_visitor'

module Lume
  class Compiler # :nodoc:
    # Compiles the CompilerIR from the given compiler context into LLVM IR, which
    # can be executed or dumped to an object file.
    #
    # @param context [CompilerContext] The compiler context containing the CompilerIR.
    #
    # @return [void]
    def compile!(context)
      # Initialize the LLVM backend
      initialize_codegen!

      # Generate LLVM IR from each of the root nodes within the CompilerIR
      context.ir.nodes.each { |node| @visitor.visit(node) }
    end

    # Finishes the compilation process.
    #
    # @return [void]
    def finalize!
      ensure_initialized!

      @visitor.finalize!
    end

    # Optimizes the module using LLVM's optimization passes.
    #
    # @return [void]
    def optimize!
      ensure_initialized!

      @pass_manager.run(@module)

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

      @target_machine.emit(@module, filename, :object)
    end

    # Evaluates / executes the compiled module, as if it were a compiled executable.
    #
    # @param args [Array] The arguments to pass to the module's `main` function.
    #
    # @return [Integer]
    def evaluate(*args)
      ensure_initialized!

      argc = args.length
      argv = nil if args.empty?

      @engine.run_function(@engine.functions[MAIN_NAME], argc, argv)
    end

    # Finishes the compilation process.
    #
    # @return [void]
    def finish
      @builder.dispose
      @module.dispose

      @initialized = false
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

    MAIN_NAME = 'main'

    # initializes the backend components for Lume (such as LLVM, JIT, etc.)
    #
    # @return [void]
    def initialize_codegen!
      ensure_uninitialized!

      LLVM.init_jit

      @module = LLVM::Module.new('lume')
      @engine = LLVM::JITCompiler.new(@module)
      @pass_manager = LLVM::PassManager.new
      @builder = LLVM::Builder.new
      @target_machine = @engine.target_machine

      @visitor = NodeVisitor.new(@module, @engine, @builder)

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
