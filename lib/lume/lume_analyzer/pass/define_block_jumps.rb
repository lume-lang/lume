# frozen_string_literal: true

module Lume
  class Analyzer
    module Pass
      # Inserts branch instructions to ensure that all blocks end with a terminator instruction.
      #
      # This pass loops over all conditional-, loop- and switch-blocks to ensure that each child block branches
      # to their corresponding merge block.
      class DefineBlockJumps < VisitorPass
        BRANCHING_BLOCK_TYPES = [Conditional, Loop].freeze

        def initialize(logger)
          super

          @branch_exits = []
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

        # Appends a `continue` instruction to the loop block.
        #
        # @param loop [Loop] The loop block to analyze.
        def accept_loop(loop)
          # If the block already has a branch, we don't need to append a continue instruction.
          return if loop.block.branch?

          continue = Lume::MIR::Continue.new(loop.entry)
          continue.loop = loop

          loop.block.expressions << continue
        end
      end
    end
  end
end
