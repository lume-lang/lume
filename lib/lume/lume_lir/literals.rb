# frozen_string_literal: true

module Lume
  module LIR
    # Represents a single constant integer literal.
    class IntLiteral < Instruction
      attr_accessor :bits, :value

      def initialize(bits:, value:)
        super()

        @bits = bits
        @value = value
      end
    end

    # Represents a single constant float literal.
    class FloatLiteral < Instruction
      attr_accessor :bits, :value

      def initialize(bits:, value:)
        super()

        @bits = bits
        @value = value
      end
    end

    # Represents a single constant boolean literal.
    class BooleanLiteral < Instruction
      attr_accessor :value

      def initialize(value:)
        super()

        @value = value
      end
    end

    # Represents a single constant null literal.
    class NullLiteral < Instruction
    end
  end
end
