# frozen_string_literal: true

require 'lume/lume_ir/ir'

module Lume
  module IR
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

    # Represents an abstract scalar type.
    class Scalar < Type
      attr_accessor :name

      def initialize(name)
        super()

        @name = name
      end

      # Determines whether the scalar is an integer type.
      #
      # @return [Boolean]
      def integer?
        name.start_with?('Int') || name.start_with?('UInt')
      end

      # Determines whether the scalar is a floating-point type.
      #
      # @return [Boolean]
      def floating?
        name.start_with?('Float') || name.start_with?('Double')
      end

      # Determines whether the scalar is a string type.
      #
      # @return [Boolean]
      def string?
        name == 'String'
      end

      # Determines whether the scalar is a boolean type.
      #
      # @return [Boolean]
      def boolean?
        name == 'Boolean'
      end

      # On numeric scalar types, returns the width in bits.
      #
      # @return [Integer, nil]
      def width
        return nil unless integer? || floating?

        case name
        when 'Int8', 'UInt8' then 8
        when 'Int16', 'UInt16' then 16
        when 'Int32', 'UInt32', 'Float' then 32
        when 'Int64', 'UInt64', 'Double' then 64
        end
      end

      # On numeric scalar types, returns whether the scalar is signed.
      #
      # @return [Boolean]
      def signed?
        return false unless integer?

        case name
        when 'Int8', 'Int16', 'Int32', 'Int64' then true
        when 'UInt8', 'UInt16', 'UInt32', 'UInt64' then false
        end
      end

      def ==(other)
        other.is_a?(self.class) && other.name == name
      end

      def to_s
        name
      end
    end

    # Represents a named type.
    class NamedType < Scalar
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
