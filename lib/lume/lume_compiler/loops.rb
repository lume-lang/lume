# frozen_string_literal: true

require 'lume/lume_mir/mir'

module Lume
  class NodeVisitor # :nodoc:
    protected

    # Visits a loop expression node in the AST and generates LLVM IR.
    #
    # @param expression [Loop] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_loop_expression(expression)
      visit_many(expression.entry, expression.exit)

      case expression
      when InfiniteLoop then visit_infinite_loop_expression(expression)
      when IteratorLoop then visit_iterator_loop_expression(expression)
      when PredicateLoop then visit_predicate_loop_expression(expression)
      else raise NotImplementedError, "Unsupported loop type: #{expression.class}"
      end
    end

    # Visits an infinite loop expression node in the AST and generates LLVM IR.
    #
    # @param expression [InfiniteLoop] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_infinite_loop_expression(expression)
      # Enter the loop from the current block
      @builder.branch(expression.block.label.ir)

      @builder.in_block(expression.block.label.ir) do
        # Lower all the MIR expressions into the LLVM block
        visit(expression.block)
      end

      # At the very end, branch to the exit block, so statements can follow after the loop.
      @builder.branch_exit(expression.exit.ir)
    end
  end
end
