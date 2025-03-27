# frozen_string_literal: true

require 'lume/lume_mir/mir'

module Lume
  module MIR
    # Represents an abstract loop expression.
    class Loop < Expression
      attr_accessor :block, :entry, :exit

      def initialize(block)
        super()

        @block = block
      end

      def accept_children(visitor)
        @block.expressions.each { |block| visitor.accept(block) }
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

      def accept_children(visitor)
        visitor.accept(@pattern)
        visitor.accept(@collection)

        super
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
    class PredicateLoop < Loop
      attr_accessor :predicate

      def initialize(predicate, block)
        super(block)

        @predicate = predicate
      end

      def accept_children(visitor)
        visitor.accept(@predicate)

        super
      end

      def ==(other)
        super && @predicate == other.predicate
      end
    end

    # Represents a loop break expression.
    #
    #   'break'
    class Break < Goto
      attr_accessor :loop

      def ==(other)
        other.is_a?(Break)
      end
    end

    # Represents a loop continue expression.
    #
    #   'continue'
    class Continue < Goto
      attr_accessor :loop

      def ==(other)
        other.is_a?(Continue)
      end
    end
  end
end
