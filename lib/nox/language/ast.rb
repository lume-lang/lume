# frozen_string_literal: true

module Nox
  module Language
    # Represents the abstract syntax tree.
    class AST
      attr_accessor :nodes

      def initialize(nodes = [])
        @nodes = nodes
      end
    end

    class Node # :nodoc:
      attr_accessor :parent, :location

      def call(name, *args)
        Call.new(self, name, *args)
      end
    end

    # Represents an abstract expression.
    class Expression < Node
      attr_accessor :expression_type
    end

    # Represents an abstract literal.
    class Literal < Node
      attr_accessor :value

      def initialize(value)
        super()

        @value = value
      end

      def ==(other)
        other.is_a?(self.class) && @value == other.value
      end
    end

    # Represents a single argument.
    #
    #   [name ':'] value
    class Argument < Node
      attr_accessor :name, :value

      def accept_children(visitor)
        visitor.accept(@value)
      end

      def ==(other)
        other.is_a?(Argument) && @name == other.name && @value == other.value
      end
    end

    # Represents a single argument definition.
    #
    #   name ':' type
    class ArgumentDefinition < Node
      attr_accessor :name, :type

      def accept_children(visitor)
        visitor.accept(@type)
      end

      def ==(other)
        other.is_a?(ArgumentDefinition) && @name == other.name && @type == other.type
      end
    end

    # Represents an assignment expression.
    #
    #   target '=' value
    class Assignment < Expression
      attr_accessor :target, :value

      def initialize(target, value)
        super()

        @target = target
        @value = value
      end

      def accept_children(visitor)
        visitor.accept(@target)
        visitor.accept(@value)
      end

      def ==(other)
        other.is_a?(Assignment) && @target == other.target && @value == other.value
      end
    end

    # Represents a function- or method-invocation.
    #
    #   [ target '.' ] action '(' arguments [ ',' arguments ]* ')'
    class Call < Expression
      attr_accessor :target, :action, :arguments

      def initialize(target, action, *args)
        super()

        @target = target
        @action = action
        @arguments = args
      end

      def accept_children(visitor)
        visitor.accept(@target) if @target.is_a?(Node)
        visitor.accept(@action) if @action.is_a?(Node)

        @arguments.each { |arg| visitor.accept(arg) }
      end

      def ==(other)
        other.is_a?(self.class) && @target == other.target && @action == other.action && @arguments == other.arguments
      end

      def first_arg
        return @arguments.first if @arguments.any?

        nil
      end
    end

    # Represents a struct definition.
    #
    #   'struct' name
    #     expressions
    #   'end'
    class StructDefinition < Expression
      attr_accessor :name, :expressions

      def accept_children(visitor)
        @expressions.each { |ex| visitor.accept(ex) }
      end
    end

    # Represents a class definition.
    #
    #   'class' name
    #     expressions
    #   'end'
    class ClassDefinition < Expression
      attr_accessor :name, :expressions

      def accept_children(visitor)
        @expressions.each { |ex| visitor.accept(ex) }
      end
    end

    # Represents a method definition.
    #
    #   'def' [ target '.' ] name '(' ')' ':' return
    #     expressions
    #   'end'
    # |
    #   'def' [ target '.' ] name '(' arguments [ ',' arguments ]* ')' ':' return
    #     expressions
    #   'end'
    class MethodDefinition < Expression
      attr_accessor :name, :arguments, :return, :expressions

      def accept_children(visitor)
        @expressions.each { |ex| visitor.accept(ex) }

        visitor.accept(@return)
      end
    end

    # Represents a variable declaration.
    #
    #   name ':' type [ '=' value ]
    class VariableDeclaration < Expression
      attr_accessor :name, :type, :value

      def initialize
        super

        @value = NilLiteral.new
      end

      def accept_children(visitor)
        visitor.accept(@type)
        visitor.accept(@value)
      end

      def ==(other)
        other.is_a?(self.class) && @name == other.name && @type == other.type && @value == other.value
      end
    end

    # Represents a variable reference.
    #
    #   name
    class VariableReference < Expression
      attr_accessor :name

      def initialize(name)
        super()

        @name = name
      end

      def ==(other)
        other.is_a?(self.class) && @name == other.name
      end
    end

    # Represents an return expression.
    #
    #   'return' value
    class Return < Expression
      attr_accessor :value

      def accept_children(visitor)
        visitor.accept(@value)
      end
    end

    # Represents an abstract conditional expression.
    class Conditional < Expression
      attr_accessor :condition, :then

      def initialize
        super

        @condition = nil
        @then = []
      end

      def accept_children(visitor)
        @then.each { |ex| visitor.accept(ex) }
      end
    end

    # Represents an `if` conditional expression.
    #
    #   'if' condition
    #     expressions
    #   'end'
    # |
    #   'if' condition
    #     then
    #   'else'
    #     else
    #   'end'
    # |
    #   'if' condition
    #     then
    #   'else if' else_if.key
    #     else_if.value
    #   'else'
    #     else
    #   'end'
    class IfConditional < Conditional
      attr_accessor :else, :else_if

      def initialize
        super

        @else = []
        @else_if = []
      end

      def accept_children(visitor)
        super

        @else.each { |ex| visitor.accept(ex) }
        @else_if.each { |ex| visitor.accept(ex) }
      end
    end

    # Represents an `else if` conditional expression.
    #
    #   'else if' condition
    #     expression
    #   'end'
    class ElseIfConditional < Conditional
    end

    # Represents an `unless` conditional expression.
    #
    #   'unless' condition
    #     expressions
    #   'end'
    # |
    #   'unless' condition
    #     then
    #   'else'
    #     else
    #   'end'
    class UnlessConditional < Conditional
      attr_accessor :else

      def initialize
        super

        @else = []
      end

      def accept_children(visitor)
        super

        @else.each { |ex| visitor.accept(ex) }
      end
    end

    # Represents an abstract number literal.
    class NumberLiteral < Literal
      def self.can_contain?(value)
        raise NotImplementedError, 'Subclasses must implement the `can_contain?` method'
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
    end

    # Represents an unsigned, 1-byte integer literal.
    class UnsignedByteLiteral < NumberLiteral
      def self.can_contain?(value)
        value.is_a?(Integer) && value >= 0 && value <= 255
      end
    end

    # Represents a signed, 2-byte integer literal.
    class ShortLiteral < NumberLiteral
      def self.can_contain?(value)
        value.is_a?(Integer) && value >= -32_768 && value <= 32_767
      end
    end

    # Represents an unsigned, 2-byte integer literal.
    class UnsignedShortLiteral < NumberLiteral
      def self.can_contain?(value)
        value.is_a?(Integer) && value >= 0 && value <= 65_535
      end
    end

    # Represents a signed, 4-byte integer literal.
    class WordLiteral < NumberLiteral
      def self.can_contain?(value)
        value.is_a?(Integer) && value >= -2_147_483_648 && value <= 2_147_483_647
      end
    end

    # Represents an unsigned, 4-byte integer literal.
    class UnsignedWordLiteral < NumberLiteral
      def self.can_contain?(value)
        value.is_a?(Integer) && value >= 0 && value <= 4_294_967_295
      end
    end

    # Represents a signed, 8-byte integer literal.
    class LongLiteral < NumberLiteral
      def self.can_contain?(value)
        value.is_a?(Integer) && value >= -9_223_372_036_854_775_808 && value <= 9_223_372_036_854_775_807
      end
    end

    # Represents an unsigned, 8-byte integer literal.
    class UnsignedLongLiteral < NumberLiteral
      def self.can_contain?(value)
        value.is_a?(Integer) && value >= 0 && value <= 18_446_744_073_709_551_615
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
    end

    # Represents a double-precision floating-point literal.
    class DoubleLiteral < RealLiteral
      PRECISION_DIGITS = 15

      def self.can_contain?(value)
        return false unless value.is_a?(Float) || value.is_a?(Integer)

        value.round(PRECISION_DIGITS - 1) == value
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

    # Represents a nil literal.
    class NilLiteral < Literal
      def initialize
        super(nil)
      end

      def self.can_contain?(value)
        value.nil?
      end
    end

    # Represents an abstract type node.
    class Type < Node
    end

    # Represents a void type.
    class Void < Type
    end

    # Represents an abstract scalar type.
    class Scalar < Type
      attr_accessor :name

      def initialize(name)
        super()

        @name = name
      end
    end

    # Represents a void type.
    class Void < Type
    end
  end
end
