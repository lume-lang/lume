# frozen_string_literal: true

require 'lume/location'
require 'lume/lume_syntax/ast'

module Lume
  module Syntax
    # Represents an abstract number literal.
    class NumberLiteral < Literal
      def self.can_contain?(value)
        raise NotImplementedError, 'Subclasses must implement the `can_contain?` method'
      end

      def to_ir
        raise NotImplementedError, 'Subclasses must implement the `to_ir` method'
      end
    end

    # Represents an abstract integer literal.
    class IntegerLiteral < NumberLiteral
    end

    # Represents a signed, 1-byte integer literal.
    class ByteLiteral < NumberLiteral
      def self.can_contain?(value)
        value.is_a?(Integer) && value >= -128 && value <= 127
      end

      def to_ir
        ::LLVM::Int8.from_i(value, true)
      end
    end

    # Represents an unsigned, 1-byte integer literal.
    class UnsignedByteLiteral < NumberLiteral
      def self.can_contain?(value)
        value.is_a?(Integer) && value >= 0 && value <= 255
      end

      def to_ir
        ::LLVM::Int8.from_i(value, false)
      end
    end

    # Represents a signed, 2-byte integer literal.
    class ShortLiteral < NumberLiteral
      def self.can_contain?(value)
        value.is_a?(Integer) && value >= -32_768 && value <= 32_767
      end

      def to_ir
        ::LLVM::Int16.from_i(value, true)
      end
    end

    # Represents an unsigned, 2-byte integer literal.
    class UnsignedShortLiteral < NumberLiteral
      def self.can_contain?(value)
        value.is_a?(Integer) && value >= 0 && value <= 65_535
      end

      def to_ir
        ::LLVM::Int16.from_i(value, false)
      end
    end

    # Represents a signed, 4-byte integer literal.
    class WordLiteral < NumberLiteral
      def self.can_contain?(value)
        value.is_a?(Integer) && value >= -2_147_483_648 && value <= 2_147_483_647
      end

      def to_ir
        ::LLVM::Int32.from_i(value, true)
      end
    end

    # Represents an unsigned, 4-byte integer literal.
    class UnsignedWordLiteral < NumberLiteral
      def self.can_contain?(value)
        value.is_a?(Integer) && value >= 0 && value <= 4_294_967_295
      end

      def to_ir
        ::LLVM::Int32.from_i(value, false)
      end
    end

    # Represents a signed, 8-byte integer literal.
    class LongLiteral < NumberLiteral
      def self.can_contain?(value)
        value.is_a?(Integer) && value >= -9_223_372_036_854_775_808 && value <= 9_223_372_036_854_775_807
      end

      def to_ir
        ::LLVM::Int64.from_i(value, true)
      end
    end

    # Represents an unsigned, 8-byte integer literal.
    class UnsignedLongLiteral < NumberLiteral
      def self.can_contain?(value)
        value.is_a?(Integer) && value >= 0 && value <= 18_446_744_073_709_551_615
      end

      def to_ir
        ::LLVM::Int32.from_i(value, false)
      end
    end

    # Represents an abstract floating-point literal.
    class RealLiteral < NumberLiteral
    end

    # Represents a single-precision floating-point literal.
    class FloatLiteral < RealLiteral
      PRECISION_DIGITS = 7

      def self.can_contain?(value)
        return false unless value.is_a?(Float) || value.is_a?(Integer)

        value.round(PRECISION_DIGITS - 1) == value
      end

      def to_ir
        ::LLVM.Float(value)
      end
    end

    # Represents a double-precision floating-point literal.
    class DoubleLiteral < RealLiteral
      PRECISION_DIGITS = 15

      def self.can_contain?(value)
        return false unless value.is_a?(Float) || value.is_a?(Integer)

        value.round(PRECISION_DIGITS - 1) == value
      end

      def to_ir
        ::LLVM.Double(value)
      end
    end

    # Represents a string literal.
    class StringLiteral < Literal
      def self.can_contain?(value)
        value.is_a?(String)
      end
    end

    # Represents a boolean literal.
    class BooleanLiteral < Literal
      def self.can_contain?(value)
        %w[true false].include?(value) || value == true || value == false
      end
    end

    # Represents a null literal.
    class NullLiteral < Literal
      def initialize
        super(nil)
      end

      def self.can_contain?(value)
        value.nil?
      end
    end
  end
end
