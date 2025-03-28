# frozen_string_literal: true

module Lume
  class Analyzer
    module Pass
      # Inserts branch instructions to ensure that all blocks end with a terminator instruction.
      #
      # This pass loops over all conditional-, loop- and switch-blocks to ensure that each child block branches
      # to their corresponding merge block.
      class DefineBlockJumps
        include Lume::MIR
        include Lume::MIR::Visitor

        BRANCHING_BLOCK_TYPES = [Conditional, Loop].freeze

        def initialize(logger)
          @logger = logger
        end

        # Performs the analysis pass on the given modules.
        #
        # @param modules [Array<Lume::Module>] The modules to analyze.
        def call(modules)
          modules.each { |mod| accept_ast(mod.mir) }
        end

        private

        # Inserts branch instructions in the given conditional block.
        #
        # @param conditional [Conditional] The conditional block to analyze.
        def accept_conditional(conditional)
          # Inserts branches to the merge label in blocks which don't already branch away.
          conditional.cases.each do |cond_case|
            next if cond_case.branch?

            cond_case.block.expressions << Lume::MIR::Goto.new(conditional.merge_label)
          end
        end
      end
    end
  end
end
