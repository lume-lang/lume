# frozen_string_literal: true

require 'lume/lume_mir/mir'
require 'lume/lume_mir/loops'
require 'lume/lume_mir/types'
require 'lume/lume_mir/values'

module Lume
  module Lowering
    class Generator # :nodoc:
      protected

      CONDITIONAL_THEN_LABEL = :'#conditional_then'

      CONDITIONAL_ELSE_LABEL = :'#conditional_else'

      CONDITIONAL_EXIT_LABEL = :'#conditional_exit'

      # Visits a conditional expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::Conditional] The expression to visit.
      #
      # @return [Lume::MIR::Conditional]
      def generate_conditional(expression)
        case expression
        when Lume::Syntax::IfConditional then generate_if_conditional(expression)
        when Lume::Syntax::UnlessConditional then generate_unless_conditional(expression)
        when Lume::Syntax::ElseIfConditional then generate_else_if_conditional(expression)
        else raise "Unsupported conditional type: #{expression.class}"
        end
      end

      # Visits an `if` conditional expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::IfConditional] The expression to visit.
      #
      # @return [Lume::MIR::Conditional]
      def generate_if_conditional(expression)
        condition = generate_node(expression.condition)
        then_block = generate_block(expression.then)
        else_if_block = generate_nodes(expression.else_if)
        else_block = generate_block(expression.else)

        conditional = Lume::MIR::Conditional.new(
          condition: condition,
          then_block: then_block,
          else_if: else_if_block,
          else_block: else_block
        )

        generate_root_conditional_block(conditional)

        conditional
      end

      # Visits an `unless` conditional expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::UnlessConditional] The expression to visit.
      #
      # @return [Lume::MIR::Conditional]
      def generate_unless_conditional(expression)
        condition = generate_node(expression.condition)
        then_block = generate_block(expression.then)
        else_block = generate_block(expression.else)

        conditional = Lume::MIR::Conditional.new(
          condition: Lume::MIR::Negation.new(condition),
          then_block: then_block,
          else_block: else_block
        )

        generate_root_conditional_block(conditional)

        conditional
      end

      # Visits an `else if` conditional expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::ElseIfConditional] The expression to visit.
      #
      # @return [Lume::MIR::Conditional]
      def generate_else_if_conditional(expression)
        condition = generate_node(expression.condition)
        then_block = generate_block(expression.then)

        Lume::MIR::Conditional.new(
          condition: condition,
          then_block: then_block
        )
      end

      private

      # Applies conditional-specific logic to the given conditional block.
      #
      # @param conditional [Lume::MIR::Conditional] The conditional to generate the block from.
      #
      # @return [void]
      def generate_root_conditional_block(conditional)
        merge_label = Lume::MIR::Label.new(CONDITIONAL_EXIT_LABEL)

        # Handle the labels on the root blocks (`then` and `else`)
        generate_child_conditional_block(conditional, merge_label)

        # Handle the labels on the auxiliary blocks (`else if`)
        conditional.else_if.each do |else_if|
          generate_child_conditional_block(else_if, merge_label)
        end
      end

      # Applies conditional-specific logic to the given conditional block.
      #
      # @param conditional [Lume::MIR::Conditional] The conditional to generate the block from.
      #
      # @return [void]
      def generate_child_conditional_block(conditional, merge_label)
        # Add a new label after the conditional statement, so statements can follow after it.
        conditional.merge_label = merge_label

        # Add a new label for the `then` block of a conditional statement.
        # This block contains the statements to execute if the condition succeeds.
        conditional.then_label = Lume::MIR::Label.new(CONDITIONAL_THEN_LABEL)

        # Add a new label for the `else` block of a conditional statement.
        # This block contains the statements to execute if the condition fails.
        #
        # If the 'else' block is empty, we can skip it and jump directly to the merge label.
        conditional.else_label = if conditional.else.empty?
          conditional.merge_label
        else
          Lume::MIR::Label.new(CONDITIONAL_ELSE_LABEL)
        end
      end
    end
  end
end
