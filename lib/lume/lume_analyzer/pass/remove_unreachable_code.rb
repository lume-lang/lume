# frozen_string_literal: true

module Lume
  class Analyzer
    module Pass
      # Remove code which is unreachable within a block.
      #
      # LLVM does not accept instructions after terminators - i.e. after a return, branch, or similar instruction. We
      # therefore need to remove any instructions that follow a terminator instruction. This pass also raises a warning
      # about unreachable code.
      class RemoveUnreachableCode
        include Lume::MIR

        BRANCHING_INSTRUCTIONS = [Return, Goto].freeze

        def initialize(logger)
          @logger = logger
        end

        # Performs the analysis pass on the given modules.
        #
        # @param modules [Array<Lume::Module>] The modules to analyze.
        def call(modules)
          modules.each { |mod| visit_module(mod) }
        end

        private

        # Visits a module and reports any unused symbols.
        #
        # @param mod [Lume::Module] The module to visit.
        def visit_module(mod)
          flat_ast = FlatVisitor.flatten(mod.mir)

          unreachable_blocks = find_blocks_with_unreachable_code(flat_ast)

          unreachable_blocks.each do |block|
            purge_unreachable_statements(block)
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

        # Purges unreachable statements from the given block and raises warnings about them.
        #
        # @param block [Lume::MIR::Block] The block to purge from.
        def purge_unreachable_statements(block)
          first_branch_idx = block.expressions.find_index { |expr| BRANCHING_INSTRUCTIONS.include?(expr.class) }

          first_unreachable_node = block.expressions[first_branch_idx + 1]
          unreachable_code_error = Lume::Analyzer::Errors::UnreachableCode.new(first_unreachable_node)
          @logger.report(unreachable_code_error)

          # Remove all the statements after the first branching instruction.
          block.expressions.slice!(first_branch_idx + 1..)
        end
      end
    end
  end
end
