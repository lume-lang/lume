# frozen_string_literal: true

require 'lume/lume_ir/ir'

module Lume
  module IR
    # Represents an abstract number literal.
    class NumberLiteral < Literal
      # Determines whether the node can be cast to the given type.
      #
      # @param to [Class] Type to check
      #
      # @return [bool]
      def castable_to?(to)
        to.is_a?(NumberLiteral) && to.bytesize >= bytesize
      end

      # Determines whether the node can contain the given value.
      #
      # @param value [Object] Value to check
      #
      # @return [bool]
      def self.can_contain?(_value)
        raise 'NumberLiteral::can_contain? must be implemented by subclasses.'
      end
    end

    # Represents an abstract integer literal.
    class IntegerLiteral < NumberLiteral
    end

    # Represents a signed, 1-byte integer literal.
    class ByteLiteral < NumberLiteral
      def bytesize
        1
      end

      def castable_to?(to)
        super && !to.is_a?(UnsignedByteLiteral)
      end

      def self.can_contain?(value)
        value.is_a?(Integer) && value >= -128 && value <= 127
      end

      def self.signed?
        true
      end
    end

    # Represents an unsigned, 1-byte integer literal.
    class UnsignedByteLiteral < NumberLiteral
      def bytesize
        1
      end

      def castable_to?(to)
        super && !to.is_a?(ByteLiteral)
      end

      def self.can_contain?(value)
        value.is_a?(Integer) && value >= 0 && value <= 255
      end

      def self.signed?
        false
      end
    end

    # Represents a signed, 2-byte integer literal.
    class ShortLiteral < NumberLiteral
      def bytesize
        2
      end

      def castable_to?(to)
        super && !to.is_a?(UnsignedShortLiteral)
      end

      def self.can_contain?(value)
        value.is_a?(Integer) && value >= -32_768 && value <= 32_767
      end

      def self.signed?
        true
      end
    end

    # Represents an unsigned, 2-byte integer literal.
    class UnsignedShortLiteral < NumberLiteral
      def bytesize
        2
      end

      def castable_to?(to)
        super && !to.is_a?(ShortLiteral)
      end

      def self.can_contain?(value)
        value.is_a?(Integer) && value >= 0 && value <= 65_535
      end

      def self.signed?
        false
      end
    end

    # Represents a signed, 4-byte integer literal.
    class WordLiteral < NumberLiteral
      def bytesize
        4
      end

      def castable_to?(to)
        super && !to.is_a?(UnsignedWordLiteral)
      end

      def self.can_contain?(value)
        value.is_a?(Integer) && value >= -2_147_483_648 && value <= 2_147_483_647
      end

      def self.signed?
        true
      end
    end

    # Represents an unsigned, 4-byte integer literal.
    class UnsignedWordLiteral < NumberLiteral
      def bytesize
        4
      end

      def castable_to?(to)
        super && !to.is_a?(WordLiteral)
      end

      def self.can_contain?(value)
        value.is_a?(Integer) && value >= 0 && value <= 4_294_967_295
      end

      def self.signed?
        false
      end
    end

    # Represents a signed, 8-byte integer literal.
    class LongLiteral < NumberLiteral
      def bytesize
        8
      end

      def castable_to?(to)
        super && !to.is_a?(UnsignedLongLiteral)
      end

      def self.can_contain?(value)
        value.is_a?(Integer) && value >= -9_223_372_036_854_775_808 && value <= 9_223_372_036_854_775_807
      end

      def self.signed?
        true
      end
    end

    # Represents an unsigned, 8-byte integer literal.
    class UnsignedLongLiteral < NumberLiteral
      def bytesize
        8
      end

      def castable_to?(to)
        super && !to.is_a?(LongLiteral)
      end

      def self.can_contain?(value)
        value.is_a?(Integer) && value >= 0 && value <= 18_446_744_073_709_551_615
      end

      def self.signed?
        false
      end
    end

    # Represents an abstract floating-point literal.
    class RealLiteral < NumberLiteral
      def self.precision_digits
        raise NotImplementedError, 'RealLiteral#precision_digits must be implemented in subclasses.'
      end

      def self.can_contain?(value)
        value = value.to_f if value.is_a?(Integer)

        return false unless value.is_a?(Float)

        value.round(precision_digits - 1) == value
      end
    end

    # Represents a single-precision floating-point literal.
    class FloatLiteral < RealLiteral
      PRECISION_DIGITS = 7

      def bytesize
        4
      end

      def self.precision_digits
        PRECISION_DIGITS
      end
    end

    # Represents a double-precision floating-point literal.
    class DoubleLiteral < RealLiteral
      PRECISION_DIGITS = 15

      def bytesize
        8
      end

      def self.precision_digits
        PRECISION_DIGITS
      end
    end

    # Represents a string literal.
    class StringLiteral < Literal
      def bytesize
        1.size
      end

      def self.can_contain?(value)
        value.is_a?(String)
      end
    end

    # Represents a boolean literal.
    class BooleanLiteral < Literal
      def bytesize
        1
      end

      def self.can_contain?(value)
        ['true', 'false', true, false].include?(value)
      end
    end

    # Represents a nil literal.
    class NilLiteral < Literal
      def initialize(value = nil)
        super
      end

      def bytesize
        1.size
      end

      def self.can_contain?(value)
        value.nil?
      end
    end
  end
end
