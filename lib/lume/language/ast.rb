# frozen_string_literal: true

module Lume
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

      def call(name, ...)
        Call.new(self, name, ...)
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

    # Represents a single parameter.
    #
    #   name ':' type
    class Parameter < Node
      attr_accessor :name, :type

      def initialize(name, type)
        super()

        @name = name
        @type = type
      end

      def accept_children(visitor)
        visitor.accept(@type)
      end

      def ==(other)
        other.is_a?(Parameter) && @name == other.name && @type == other.type
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

    # Represents an object initialization expression.
    #
    #   'new' class '(' arguments [ ',' arguments ]* ')'
    class New < Expression
      attr_accessor :class_name, :arguments

      def initialize(class_name, *arguments)
        super()

        @class_name = class_name
        @arguments = *arguments
      end

      def accept_children(visitor)
        @arguments.each { |arg| visitor.accept(arg) }
      end

      def ==(other)
        other.is_a?(self.class) && @class_name == other.class_name && @arguments == other.arguments
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
    #   'fn' [ target '.' ] name '(' ')' ':' return
    #     expressions
    #   'end'
    # |
    #   'fn' [ target '.' ] name '(' parameters [ ',' parameters ]* ')' ':' return
    #     expressions
    #   'end'
    class MethodDefinition < Expression
      attr_accessor :name, :parameters, :return, :expressions

      def initialize(name, parameters, return_value, expressions)
        super()

        @name = name
        @parameters = parameters
        @return = return_value
        @expressions = expressions
      end

      def accept_children(visitor)
        @parameters.each { |ex| visitor.accept(ex) }
        @expressions.each { |ex| visitor.accept(ex) }

        visitor.accept(@return)
      end

      def ==(other)
        return false unless other.is_a?(self.class)

        @name == other.name && @parameters == other.parameters && @return == other.return && @expressions == other.expressions
      end
    end

    # Represents a variable declaration.
    #
    #   [ 'let' | 'const' ] name ':' type [ '=' value ]
    class VariableDeclaration < Expression
      attr_accessor :name, :type, :value, :const

      def initialize(name, type, value = nil, const: false)
        super()

        @name = name
        @type = type
        @value = value || NilLiteral.new
        @const = const
      end

      def accept_children(visitor)
        visitor.accept(@type)
        visitor.accept(@value)
      end

      def ==(other)
        other.is_a?(self.class) && @name == other.name && @type == other.type && @value == other.value && @const == other.const
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

    # Represents an abstract access operator.
    class Access < Expression
      attr_accessor :target, :property

      def initialize(target, property)
        super()

        @target = target
        @property = property
      end

      def ==(other)
        other.is_a?(self.class) && @target == other.target && @property == other.property
      end
    end

    # Represents a member access expression on a target object.
    #
    #   target '.' property
    class MemberAccess < Access
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

    # Represents a nil literal.
    class NilLiteral < Literal
      def initialize
        super(nil)
      end

      def self.can_contain?(value)
        value.nil?
      end
    end

    # Represents a unary expression.
    class Unary < Expression
      attr_accessor :operator, :right

      def initialize(operator, right)
        super()

        @operator = operator
        @right = right
      end
    end

    # Represents an abstract type node.
    class Type < Node
      def ==(other)
        other.is_a?(self.class)
      end
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

      # Determines whether the scalar is a boolean type.
      #
      # @return [Boolean]
      def boolean?
        name == 'Boolean'
      end

      def ==(other)
        other.is_a?(self.class) && other.name == name
      end
    end
  end
end
