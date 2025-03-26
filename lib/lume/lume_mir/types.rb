# frozen_string_literal: true

require 'lume/lume_mir/mir'

module Lume
  module MIR
    # Defines the name of all floating-point types.
    FLOATING_TYPES = %w[Float Double].freeze

    # Defines the name of the string type.
    STRING_TYPE = 'String'

    # Defines the name of the pointer type.
    POINTER_TYPE = 'Pointer'

    # Defines the name of the boolean type.
    BOOLEAN_TYPE = 'Boolean'

    # Defines the name of all signed integer types.
    SIGNED_INTEGER_TYPES = %w[Int8 Int16 Int32 Int64].freeze

    # Defines the name of all unsigned integer types.
    UNSIGNED_INTEGER_TYPES = %w[UInt8 UInt16 UInt32 UInt64].freeze

    # Defines the name of all integer types, both signed and unsigned.
    INTEGER_TYPES = SIGNED_INTEGER_TYPES + UNSIGNED_INTEGER_TYPES

    # Defines a mapping of number types to their bit widths.
    NUMBER_WIDTHS = {
      'Int8' => 8,
      'Int16' => 16,
      'Int32' => 32,
      'Int64' => 64,
      'UInt8' => 8,
      'UInt16' => 16,
      'UInt32' => 32,
      'UInt64' => 64,
      'Float' => 32,
      'Double' => 64
    }.freeze

    # Represents an abstract type node.
    class Type < Node
      def ==(other)
        other.is_a?(self.class)
      end

      # Determines the size of the type in bytes.
      #
      # @return [Integer] Size of the type in bytes
      def bytesize
        raise "Type::bytesize must be implemented by subclasses. Implementation in #{self.class.name} is missing."
      end

      # Returns a string representation of the type.
      #
      # @return [String] String representation of the type
      def to_s
        raise "Type::to_s must be implemented by subclasses. Implementation in #{self.class.name} is missing."
      end
    end

    # Represents a void type.
    class Void < Type
      def bytesize
        0
      end

      def to_s
        'void'
      end
    end

    # Represents a named type.
    class NamedType < Type
      attr_accessor :name, :reference

      printer_ignore :reference

      def initialize(name)
        super()

        @name = name
        @expression_type = self
      end

      # Determines whether the scalar is an integer type.
      #
      # @return [Boolean]
      def integer?
        INTEGER_TYPES.include?(name)
      end

      # Determines whether the scalar is a floating-point type.
      #
      # @return [Boolean]
      def floating?
        FLOATING_TYPES.include?(name)
      end

      # Determines whether the scalar is a string type.
      #
      # @return [Boolean]
      def string?
        name == STRING_TYPE
      end

      # Determines whether the scalar is a pointer type.
      #
      # @return [Boolean]
      def pointer?
        name == POINTER_TYPE
      end

      # Determines whether the scalar is a boolean type.
      #
      # @return [Boolean]
      def boolean?
        name == BOOLEAN_TYPE
      end

      # On numeric scalar types, returns the width in bits.
      #
      # @return [Integer, nil]
      def width
        NUMBER_WIDTHS[name]
      end

      # On numeric scalar types, returns whether the scalar is signed.
      #
      # @return [Boolean]
      def signed?
        SIGNED_INTEGER_TYPES.include?(name)
      end

      # Determines whether the scalar is a builtin type.
      #
      # @return [Boolean]
      def builtin?
        @reference.builtin?
      end

      def ==(other)
        other.is_a?(self.class) && other.name == name
      end

      def to_s
        name
      end
    end

    # Represents a pointer type.
    class Pointer < Type
      attr_accessor :of

      def initialize(of)
        super()

        @of = of
      end

      def ==(other)
        other.is_a?(self.class) && other.of == of
      end

      def bytesize
        # `Integer#size` returns the size of the integer in bytes.
        # Since the size of the pointer depends on the integer size to determine it.
        # All integers are 64-bits on x64 and 32-bits on x86.
        1.size
      end

      def to_s
        "*#{of}"
      end
    end

    # Represents a union type.
    class Union < Type
      attr_accessor :types

      def initialize(types)
        super()

        @types = types
      end

      def ==(other)
        other.is_a?(self.class) && other.types == types
      end

      def bytesize
        types.max { |a, b| a.bytesize <=> b.bytesize }
      end

      def to_s
        "[#{types.join(', ')}]"
      end
    end
  end
end
