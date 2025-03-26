# frozen_string_literal: true

require 'llvm/core'
require 'llvm/execution_engine'
require 'llvm/linker'
require 'llvm/transforms/scalar'

require 'lume/module'
require 'lume/lume_llvm/module'

module Lume
  # Linker for combining (linking) LLVM modules into a single executable or library.
  class Linker
    # Links all the LLVM modules from the given compiler context into a single module, which
    # can be executed or dumped to an object file.
    #
    # @param modules  [Array<Lume::Module>] The modules to link.
    #
    # @return [Lume::LLVM::Module]
    def link!(modules)
      main_module = Lume::LLVMModule.new('main')

      # Link each of the modules into the main LLVM module.
      modules.each do |mod|
        result = mod.llvm_module.link_into!(main_module)

        raise "Failed to link module #{mod.name}: #{result.message}" unless result.nil?
      end

      main_module.verify!

      main_module
    end
  end
end
