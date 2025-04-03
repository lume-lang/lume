# frozen_string_literal: true

require 'lume/lume_mir/mir'
require 'lume/lume_mir/loops'
require 'lume/lume_mir/types'
require 'lume/lume_mir/values'

module Lume
  module Lowering
    class Generator # :nodoc:
      protected

      # Visits a conditional expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::Conditional] The expression to visit.
      #
      # @return [Lume::MIR::Conditional]
      def generate_conditional(expression)
        conditional = case expression
        when Lume::Syntax::IfConditional then generate_if_conditional(expression)
        when Lume::Syntax::UnlessConditional then generate_unless_conditional(expression)
        else raise "Unsupported conditional type: #{expression.class}"
        end

        # Set the `next` pointers for the conditional cases.
        link_conditional_cases(conditional)

        conditional
      end

      # Visits an `if` conditional expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::IfConditional] The expression to visit.
      #
      # @return [Lume::MIR::Conditional]
      def generate_if_conditional(expression)
        conditional = Lume::MIR::Conditional.new

        # Add the root condition
        conditional.cases << generate_conditional_case(expression.condition, expression.then)

        # Add the `else if` blocks, if any.
        expression.else_if.each do |else_if|
          conditional.cases << generate_conditional_case(else_if.condition, else_if.then)
        end

        # Add the `else` block, if it exists.
        conditional.cases << generate_conditional_case(nil, expression.else) unless expression.else.empty?

        conditional
      end

      # Visits an `unless` conditional expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::UnlessConditional] The expression to visit.
      #
      # @return [Lume::MIR::Conditional]
      def generate_unless_conditional(expression)
        conditional = Lume::MIR::Conditional.new

        conditional.cases << generate_conditional_case(expression.condition, expression.then)
        conditional.cases << generate_conditional_case(nil, expression.else) unless expression.else.empty?

        first_condition = conditional.cases.first
        first_condition.condition = Lume::MIR::Negation.new(first_condition.condition)

        conditional
      end

      private

      # Creates a conditional case from the given condition and block.
      #
      # @param condition  [Lume::Syntax::Expression] The condition to visit.
      # @param block      [Lume::Syntax::Block] The block to visit.
      #
      # @return [Lume::MIR::Conditional]
      def generate_conditional_case(condition, block)
        condition = generate_node(condition)
        block = generate_block(block)

        Lume::MIR::ConditionalCase.new(condition, block)
      end

      # Links the cases of the given conditional together with the `next` properties.
      #
      # @param conditional [Lume::MIR::Conditional] The conditional to link.
      #
      # @return [void]
      def link_conditional_cases(conditional)
        conditional.cases.each_with_index do |cond_case, index|
          cond_case.next = conditional.cases[index + 1]
        end
      end
    end
  end
end
