# frozen_string_literal: true

require 'lume/lume_mir/mir'
require 'lume/lume_mir/loops'
require 'lume/lume_mir/types'
require 'lume/lume_mir/values'

module Lume
  module Lowering
    class Generator # :nodoc:
      protected

      CONDITIONAL_MERGE_LABEL = :'#if_merge'

      CONDITIONAL_STATEMENT_LABEL = :'#if_condition'

      CONDITIONAL_BRANCH_LABEL = :'#if_branch'

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

        # Add labels to the conditional blocks, so LLVM can create proper blocks for them.
        add_conditional_labels(conditional)

        # Set the `next` pointers for the conditional cases.
        link_conditional_cases(conditional)

        # Insert branches into unbranching cases, so they can branch to the merge block.
        insert_merging_branches(conditional)

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

      # Adds labels to the given conditional and child cases.
      #
      # @param conditional [Lume::MIR::Conditional] The conditional to add labels to.
      #
      # @return [void]
      def add_conditional_labels(conditional)
        conditional.merge_label = Lume::MIR::Label.new(CONDITIONAL_MERGE_LABEL)

        # Handle the labels on the child cases
        conditional.cases.each_with_index do |cond_case, index|
          # Add a label to the statement itself, so it can be called by other conditions.
          # The first case - i.e. the `then` block - doesn't need a label, since it should never be branched to.
          # The `else` block also doesn't need a label, since it doesn't contain a condition statement.
          if index.positive? && !cond_case.condition.nil?
            cond_case.label = Lume::MIR::Label.new("#{CONDITIONAL_STATEMENT_LABEL}_#{index}")
          end

          # Add a label to the block itself, so it can be branched to if the condition succeeds.
          cond_case.block.label = Lume::MIR::Label.new("#{CONDITIONAL_BRANCH_LABEL}_#{index}")
        end
      end

      # Links the cases of the given conditional together with the `next` properties.
      #
      # @param conditional [Lume::MIR::Conditional] The conditional to link.
      #
      # @return [void]
      def link_conditional_cases(conditional)
        conditional.cases.each_with_index do |cond_case, index|
          next_case = conditional.cases[index + 1]

          # If there are no more cases, link to the merge label.
          if next_case.nil?
            cond_case.next = conditional.merge_label
            next
          end

          cond_case.next = next_case.branch_label
        end
      end

      # Inserts branches to the merge label in blocks which don't already branch away.
      #
      # @param conditional [Lume::MIR::Conditional] The conditional to generate the block from.
      #
      # @return [void]
      def insert_merging_branches(conditional)
        conditional.cases.each do |cond_case|
          next if cond_case.branch?

          cond_case.block.expressions << Lume::MIR::Goto.new(conditional.merge_label)
        end
      end
    end
  end
end
