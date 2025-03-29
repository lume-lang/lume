# frozen_string_literal: true

module Lume
  class Analyzer
    module Pass
      # Warn about code which is unreachable within a block.
      class WarnUnreachableCode < FlatVisitorPass
        BRANCHING_INSTRUCTIONS = [Return, Goto].freeze

        private

        # Visits a module and reports any unused symbols.
        #
        # @param mod [Lume::Module] The module to visit.
        # @param nodes [Array<Lume::Node>] The flattened array of MIR nodes.
        def visit(_mod, nodes)
          unreachable_blocks = find_blocks_with_unreachable_code(nodes)

          unreachable_blocks.each do |block|
            @logger.report(Lume::Analyzer::Errors::UnreachableCode.new(block))
          end
        end

        # Finds all the blocks with unreachable code and returns them.
        #
        # @param ast [Array<Node>] The flattened AST to analyze.
        #
        # @return [Array<Block>] The blocks with unreachable code.
        def find_blocks_with_unreachable_code(ast)
          blocks = ast.select { |node| node.is_a?(Block) }
          unreachable_blocks = []

          blocks.each do |block|
            # If the block doesn't contain any branches, skip it.
            next unless block.any?(BRANCHING_INSTRUCTIONS)

            # Find the first node within the block which is a branching instruction.
            first_branch_idx = block.expressions.find_index { |expr| BRANCHING_INSTRUCTIONS.include?(expr.class) }

            # If none are found or if it's the last one in the block, skip to the next block.
            next if first_branch_idx.nil? || first_branch_idx >= block.expressions.length - 1

            unreachable_blocks << block
          end

          unreachable_blocks
        end
      end
    end
  end
end
