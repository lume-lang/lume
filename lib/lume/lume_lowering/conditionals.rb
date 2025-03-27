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

        Lume::MIR::Conditional.new(
          condition: condition,
          then_block: then_block,
          else_if: else_if_block,
          else_block: else_block
        )
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

        Lume::MIR::Conditional.new(
          condition: Lume::MIR::Negation.new(condition),
          then_block: then_block,
          else_block: else_block
        )
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
    end
  end
end
