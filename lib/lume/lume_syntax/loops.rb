# frozen_string_literal: true

require 'lume/location'
require 'lume/lume_syntax/hir'

module Lume
  module Syntax
    # Represents an abstract loop expression.
    class Loop < Expression
      attr_accessor :block

      def initialize(block)
        super()

        @block = block
      end

      def ==(other)
        other.is_a?(self.class) && @block == other.block
      end
    end

    # Represents an infinite loop.
    #
    #   'loop' '{'
    #     block
    #   '}'
    class InfiniteLoop < Loop
    end

    # Represents an iterator loop.
    #
    #   'for' pattern 'in' collection '{'
    #     block
    #   '}'
    class IteratorLoop < Loop
      attr_accessor :pattern, :collection

      def initialize(pattern, collection, block)
        super(block)

        @pattern = pattern
        @collection = collection
      end

      def ==(other)
        super && @pattern == other.pattern && @collection == other.collection
      end
    end

    # Represents a predicate loop.
    #
    #   'while' predicate '{'
    #     block
    #   '}'
    class WhileLoop < Loop
      attr_accessor :predicate

      def initialize(predicate, block)
        super(block)

        @predicate = predicate
      end

      def ==(other)
        super && @predicate == other.predicate
      end
    end
  end
end
