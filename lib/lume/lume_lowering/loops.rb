# frozen_string_literal: true

require 'lume/lume_mir/mir'
require 'lume/lume_mir/loops'
require 'lume/lume_mir/types'
require 'lume/lume_mir/values'

module Lume
  module Lowering
    class Generator # :nodoc:
      protected

      LOOP_ENTRY_LABEL = :'#loop_entry'

      LOOP_BODY_LABEL = :'#loop_body'

      LOOP_EXIT_LABEL = :'#loop_exit'

      # Visits a loop expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::Loop] The expression to visit.
      #
      # @return [Lume::MIR::Loop]
      def generate_loop(expression)
        loop = case expression
        when Lume::Syntax::InfiniteLoop then generate_infinite_loop(expression)
        when Lume::Syntax::IteratorLoop then generate_iterator_loop(expression)
        when Lume::Syntax::WhileLoop then generate_predicate_loop(expression)
        else raise "Unsupported loop type: #{expression.class}"
        end

        # Create labels for the entry and exit of the loop.
        create_loop_labels(loop)

        # Append a `continue` instruction, so the loop will actually.. loop.
        append_continue_instruction(loop)

        loop
      end

      # Visits an infinite loop expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::InfiniteLoop] The expression to visit.
      #
      # @return [Lume::MIR::InfiniteLoop]
      def generate_infinite_loop(expression)
        block = generate_block(expression.block)

        Lume::MIR::InfiniteLoop.new(block)
      end

      # Visits an iterator loop expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::IteratorLoop] The expression to visit.
      #
      # @return [Lume::MIR::IteratorLoop]
      def generate_iterator_loop(expression)
        pattern = Lume::MIR::VariableDeclaration.new(expression.pattern)
        collection = Lume::MIR::Variable.new(expression.collection)
        block = generate_block(expression.block)

        Lume::MIR::IteratorLoop.new(pattern, collection, block)
      end

      # Visits a predicate loop expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::WhileLoop] The expression to visit.
      #
      # @return [Lume::MIR::PredicateLoop]
      def generate_predicate_loop(expression)
        # Insert a conditional to break out of the loop, if the predicate is not satisfied.
        break_statement = Lume::Syntax::Break.new

        # This effectively replaces the original block with the new conditional block. For example, a loop like:
        #
        #   while x {
        #     y
        #   }
        #
        # is transformed into:
        #
        #   while x {
        #     if x {
        #       y
        #     } else {
        #       break
        #     }
        #   }
        predicate_conditional = Lume::Syntax::IfConditional.new(
          condition: expression.predicate,
          then_block: expression.block,
          else_block: [break_statement]
        )

        # Replace the original block with the new conditional block.
        expression.block = [predicate_conditional]

        # Lower the new block and create an "infinite" loop from it.
        block = generate_block(expression.block)

        Lume::MIR::InfiniteLoop.new(block)
      end

      # Visits a loop break expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::Break] The expression to visit.
      #
      # @return [Lume::MIR::Break]
      def generate_break(_expression)
        Lume::MIR::Break.new
      end

      # Visits a loop continue expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::Continue] The expression to visit.
      #
      # @return [Lume::MIR::Continue]
      def generate_continue(_expression)
        Lume::MIR::Continue.new
      end

      # Applies loop-specific logic to the given loop block.
      #
      # @param loop [Lume::MIR::Loop] The loop to generate the block from.
      #
      # @return [void]
      def create_loop_labels(loop)
        # Set a body label to the very start of the loop body.
        loop.block.label = Lume::MIR::Label.new(LOOP_BODY_LABEL)

        # Set an exit label to the very end of the loop body.
        loop.exit = Lume::MIR::Label.new(LOOP_EXIT_LABEL)
      end

      # Appends a `continue` instruction to the loop block.
      #
      # @param loop [Lume::MIR::Loop] The loop to generate the block from.
      #
      # @return [void]
      def append_continue_instruction(loop)
        # If the block already has a branch, we don't need to append a continue instruction.
        return if loop.block.branch?

        loop.block.expressions << Lume::MIR::Continue.new(loop)
      end
    end
  end
end
