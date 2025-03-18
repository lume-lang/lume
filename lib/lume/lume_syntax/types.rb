# frozen_string_literal: true

require 'lume/location'
require 'lume/lume_syntax/ast'

module Lume
  module Syntax
    # Represents an abstract type node.
    class Type < Node
      def ==(other)
        other.is_a?(self.class)
      end
    end

    # Represents a void type.
    #
    #   'void'
    class Void < Type
    end

    # Represents a null type.
    #
    #   'null'
    class Null < Type
    end

    # Represents an abstract named type.
    #
    #   name
    class NamedType < Type
      attr_accessor :name

      def initialize(name)
        super()

        @name = name
      end

      # Determines whether the type is an integer type.
      #
      # @return [Boolean]
      def integer?
        name.start_with?('Int') || name.start_with?('UInt')
      end

      # Determines whether the type is a floating-point type.
      #
      # @return [Boolean]
      def floating?
        name.start_with?('Float') || name.start_with?('Double')
      end

      # Determines whether the type is a string type.
      #
      # @return [Boolean]
      def string?
        name == 'String'
      end

      # Determines whether the type is a boolean type.
      #
      # @return [Boolean]
      def boolean?
        name == 'Boolean'
      end

      def ==(other)
        other.is_a?(self.class) && other.name == name
      end
    end

    # Represents a pointer type.
    #
    #   '*' of
    class Pointer < Type
      attr_accessor :of

      def initialize(of)
        super()

        @of = of
      end

      def ==(other)
        other.is_a?(self.class) && other.of == of
      end
    end

    # Represents a union type, where the type is one of the defined subtypes.
    #
    #   types '|' [ types* ]
    class Union < Type
      attr_accessor :types

      def initialize(types = [])
        super()

        @types = types
      end

      def ==(other)
        other.is_a?(self.class) && other.types == types
      end

      # Merges the nested unions within the union type into itself.
      def merge_nested_unions
        # Find all the unions nested within the current union
        unions = types.select { |type| type.is_a?(Union) }

        # Append all the nested union types into the current union
        unions.each { |union| types.concat(union.types) }

        # Remove all the nested unions from the current union
        types.delete_if { |type| type.is_a?(Union) }
      end
    end

    # Represents an array type, where the type is a list of zero-or-more elements.
    #
    #   '[' inner ']'
    class ArrayType < Type
      attr_accessor :inner

      def initialize(inner)
        super()

        @inner = inner
      end

      def ==(other)
        other.is_a?(self.class) && other.inner == inner
      end
    end
  end
end
