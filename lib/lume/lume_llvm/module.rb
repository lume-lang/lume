# frozen_string_literal: true

require 'llvm/core'
require 'llvm/execution_engine'
require 'llvm/linker'
require 'llvm/transforms/scalar'

require 'lume/module'

module Lume
  # Represents a single LLVM module.
  class LLVMModule
    attr_reader :inner

    def initialize(name)
      @inner = LLVM::Module.new(name)
    end

    # Optimizes the LLVM module with the given pass manager.
    #
    # @param pass_manager  [LLVM::PassManager]  The pass manager to optimize the module with.
    #
    # @return [void]
    def optimize_with!(pass_manager)
      pass_manager.run(@inner)
    end

    # Links the LLVM module into the given module.
    #
    # @param mod  [Lume::Module, LLVMModule]  The module to link into.
    #
    # @return [void]
    def link_into!(mod)
      mod = mod.llvm_module if mod.is_a?(Lume::Module)
      mod = mod.inner if mod.is_a?(Lume::LLVMModule)

      @inner.link_into(mod)
    end

    # Dumps the LLVM module to `stdout`.
    #
    # @return [void]
    def dump
      @inner.dump
    end
  end
end
