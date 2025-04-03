# frozen_string_literal: true

module Lume
  module LIR
    # Represents a type definition.
    class Type < Instruction
    end

    # Defines an integer type.
    class IntType < Type
      attr_accessor :bits

      def initialize(bits:)
        super()

        @bits = bits
      end
    end

    # Defines a float type.
    class FloatType < Type
      attr_accessor :bits

      def initialize(bits:)
        super()

        @bits = bits
      end
    end

    # Represents a boolean type.
    class BooleanType < Type
    end

    # Represents a pointer type.
    class PointerType < Type
      attr_accessor :element_type

      def initialize(element_type)
        super()

        @element_type = element_type
      end
    end

    # Represents an array type.
    class ArrayType < Type
      attr_accessor :element_type

      def initialize(element_type)
        super()

        @element_type = element_type
      end
    end

    # Represents a void type.
    class VoidType < Type
    end
  end
end
