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
      statements = create_statement_blocks(expression)
      blocks = create_conditional_blocks(expression)

      # Create the statements which perform the comparison logic.
      create_conditional_statements(expression, statements, blocks)

      # Afterwards, create the branches which the conditional statements can branch to
      create_conditional_branches(expression, blocks)

      # At the very end, branch to the merge block, so statements can follow after the conditional.
      @builder.branch_exit(blocks.last)
    end

    private

    # Creates the LLVM IR blocks for a conditional expression. The returned array will contain:
    #  - A single block for the `then` branch,
    #  - an array of zero-or-more blocks for the `else-if` branches,
    #  - a single block for the `else` branch,
    #  - a single block for the merge point.
    #
    # @param expression [Conditional] The conditional expression to create blocks for.
    #
    # @return [Array<LLVM::BasicBlock>] The generated LLVM IR blocks.
    def create_conditional_blocks(expression)
      # Create a block for the `then` branch
      then_block = @builder.block('branch_then')

      # Create a list of blocks for the `else if` branches
      else_if_blocks = expression.else_if.map { |_| @builder.block('branch_else_if') }

      # Create a block for the `else` branch
      else_block = @builder.block('branch_else')

      # Create a block for the merge point
      merge_block = @builder.block('branch_merge')

      [then_block, else_if_blocks, else_block, merge_block]
    end

    # Creates the LLVM IR blocks for conditional statement instructions.
    #
    # @param expression [Conditional] The conditional expression to create blocks for.
    #
    # @return [Array<LLVM::BasicBlock>] The generated LLVM IR blocks.
    def create_statement_blocks(expression)
      expression.else_if.map { |_| @builder.block('next') }
    end

    # Creates LLVM IR statements for a conditional expression.
    #
    # @param expression   [Conditional]             The conditional expression to create branches for.
    # @param blocks       [Array<LLVM::BasicBlock>] The blocks for the conditional expression.
    #
    # @return [void]
    def create_conditional_statements(expression, statements, blocks)
      then_block, else_if_blocks, else_block, = blocks

      # Create the first statement, which handles the `then` branch.
      # If the condition is true, jump to the `then` block. Otherwise, attempt to jump to the next `else if` block
      # if it exists. If not, jump to the `else` block.
      create_conditional_statement(expression.condition, nil, then_block, statements.first || else_block)

      # Creates the `else if` statements.
      # If the condition is true, jump to the `then` block of the current `else if` statement.
      # Otherwise, attempt to jump to the next `else if` block if it exists. If not, jump to the `else` block.
      expression.else_if.each_with_index do |else_if, index|
        true_block = else_if_blocks[index]

        false_block = if index >= expression.else_if.length - 1
          else_block
        else
          statements[index + 1]
        end

        create_conditional_statement(else_if.condition, statements[index], true_block, false_block)
      end
    end

    # Creates an LLVM IR statement for a conditional expression.
    #
    # @param condition    [Lume::MIR::Expression] The condition to evaluate.
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
    # @param blocks       [Array<LLVM::BasicBlock>] The blocks for the conditional expression.
    #
    # @return [void]
    def create_conditional_branches(expression, blocks)
      then_block, else_if_blocks, else_block, merge_block = blocks

      # Create branch for the `then` block
      create_conditional_branch(expression.then, then_block, merge_block)

      # Create branch for the `else if` block
      else_if_blocks.each_with_index do |else_if_block, index|
        create_conditional_branch(expression.else_if[index].then, else_if_block, merge_block)
      end

      # Create branch for the `else` block
      create_conditional_branch(expression.else, else_block, merge_block)
    end

    # Creates a branch in LLVM to store the given expression block.
    #
    # @param expressions  [Lume::MIR::Block]  The expression block to lower into the LLVM block.
    # @param block        [LLVM::BasicBlock]  The block to jump to if the condition is false.
    # @param merge_block  [LLVM::BasicBlock]  The block to jump to after the conditional branch.
    #
    # @return [void]
    def create_conditional_branch(expressions, block, merge_block)
      # Within the block, compile all expressions.
      @builder.in_block(block) do
        # Lower all the MIR expressions into the LLVM block
        visit(expressions)

        # If the block returns a value, don't insert a branch statement.
        # If the branch statement is present, the return statement might be overwritten by the merge block.
        @builder.branch(merge_block) unless expressions.return?
      end
    end
  end
end
