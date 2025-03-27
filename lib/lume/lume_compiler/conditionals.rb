# frozen_string_literal: true

require 'lume/lume_mir/mir'

module Lume
  class NodeVisitor # :nodoc:
    protected

    # Generates LLVM IR for a conditional expression.
    #
    # @param expression [Conditional] The conditional expression to generate LLVM IR for.
    #
    # @return [LLVM::Instruction] The generated LLVM IR instruction.
    def handle_conditional(expression)
      # Create LLVM blocks for all the labels within the conditional.
      visit_many(expression.cases.flat_map { |c| [c.label, c.block.label] }, expression.merge_label)

      # Create the statements which perform the comparison logic.
      create_conditional_statements(expression)

      # Afterwards, create the branches which the conditional statements can branch to
      create_conditional_branches(expression)

      # At the very end, branch to the merge block, so statements can follow after the conditional.
      @builder.branch_exit = expression.merge_label.ir
    end

    private

    # Creates LLVM IR statements for a conditional expression.
    #
    # @param expression   [Conditional]             The conditional expression to create branches for.
    #
    # @return [void]
    def create_conditional_statements(expression)
      expression.cases.each do |cond_case|
        # If the condition is unset, it's the `else` block, which shouldn't have a conditional statement.
        next if cond_case.condition.nil?

        block = cond_case.label&.ir
        true_block = cond_case.block.label.ir
        false_block = cond_case.next.ir

        create_conditional_statement(cond_case.condition, block, true_block, false_block)
      end
    end

    # Creates an LLVM IR statement for a conditional expression.
    #
    # @param condition    [Lume::MIR::Expression] The condition to evaluate.
    # @param block        [LLVM::BasicBlock, nil] The block to contain the statement in.
    # @param true_block   [LLVM::BasicBlock]      The block to jump to if the condition is true.
    # @param false_block  [LLVM::BasicBlock]      The block to jump to if the condition is false.
    #
    # @return [void]
    def create_conditional_statement(condition, block, true_block, false_block)
      @builder.in_block(block) do
        # Lower the MIR condition expression into an LLVM value
        condition = visit(condition)

        @builder.conditional_block(condition, true_block, false_block)
      end
    end

    # Creates branches in LLVM to store the given expression blocks.
    #
    # @param expression   [Conditional]             The conditional expression to create branches for.
    #
    # @return [void]
    def create_conditional_branches(expression)
      expression.cases.each do |cond_case|
        # Within the block, compile all expressions and place them in the LLVM block
        @builder.in_block(cond_case.block.label.ir) { visit(cond_case.block) }
      end
    end
  end
end
