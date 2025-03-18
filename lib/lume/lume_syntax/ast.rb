# frozen_string_literal: true

require 'lume/location'

module Lume
  module Syntax
    # Represents the abstract syntax tree.
    class AST
      attr_accessor :nodes

      def initialize(nodes = [])
        @nodes = nodes
      end

      # Gets all the import statements within the AST.
      #
      # @return [Array<Import>] The import statements within the AST.
      def imports
        @nodes.select { |node| node.is_a?(Import) }
      end
    end

    class Node # :nodoc:
      attr_accessor :parent, :location

      def call(name, ...)
        Call.new(self, name, ...)
      end

      def arg(name: nil)
        Argument.new(name, self)
      end
    end

    # Represents an abstract expression.
    class Expression < Node
      attr_accessor :expression_type, :comment
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

    # Represents a single argument.
    #
    #   [ name ':' ] value
    class Argument < Node
      attr_accessor :name, :value

      def initialize(name, value)
        super()

        @name = name
        @value = value
      end

      def accept_children(visitor)
        visitor.accept(@value)
      end

      def ==(other)
        other.is_a?(Argument) && @name == other.name && @value == other.value
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

    # Represents a class definition.
    #
    #   'class' name '{'
    #     expressions
    #   '}'
    class ClassDefinition < Expression
      attr_accessor :name, :expressions

      def initialize(name, expressions)
        super()

        @name = name
        @expressions = expressions
      end

      def ==(other)
        other.is_a?(self.class) && @name == other.name && @expressions == other.expressions
      end
    end

    # Represents a comment block.
    class Comment < Node
      attr_accessor :content

      def initialize(content)
        super()

        @content = content
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
    #   'if' condition '{'
    #     expressions
    #   '}'
    # |
    #   'if' condition '{'
    #     then
    #   '}' 'else' '{'
    #     else
    #   '}'
    # |
    #   'if' condition '{'
    #     then
    #   '}' 'else if' else_if '{'
    #     else_if.value
    #   '}' 'else' '{'
    #     else
    #   '}'
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
    #   'else if' condition '{'
    #     expression
    #   '}'
    class ElseIfConditional < Conditional
    end

    # Represents an `unless` conditional expression.
    #
    #   'unless' condition '{'
    #     expressions
    #   '}'
    # |
    #   'unless' condition '{'
    #     then
    #   '}' 'else' '{'
    #     else
    #   '}'
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

    # Represents an import expression.
    #
    #   'import' library
    class Import < Expression
      attr_accessor :library

      def initialize(library)
        super()

        @library = library
      end

      def ==(other)
        other.is_a?(Import) && @library == other.library
      end
    end

    # Represents an abstract member definition.
    class Member < Expression
      attr_accessor :visibility
    end

    # Represents a member access expression on a target object.
    #
    #   target '.' property
    class MemberAccess < Access
    end

    # Represents a method definition.
    #
    #   visibility* 'fn' name '(' ')' '->' return '{'
    #     expressions
    #   '}'
    # |
    #   visibility* 'fn' name '(' parameters [ ',' parameters ]* ')' '->' return '{'
    #     expressions
    #   '}'
    class MethodDefinition < Member
      attr_accessor :name, :parameters, :return, :expressions, :external

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

        @name == other.name &&
          @parameters == other.parameters &&
          @return == other.return &&
          @expressions == other.expressions
      end

      def external?
        @external
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

    # Represents a property declaration.
    #
    #   name [ ':' type | '=' default ]
    class Property < Member
      attr_accessor :name, :type, :default

      def initialize(name, type: nil, default: nil)
        super()

        @name = name
        @type = type
        @default = default || NilLiteral.new
      end

      def accept_children(visitor)
        visitor.accept(@type) unless @type.nil?
        visitor.accept(@default) unless @default.nil?
      end

      def ==(other)
        return false unless other.is_a?(self.class)

        @name == other.name && @type == other.type && @default == other.default
      end
    end

    # Represents an return expression.
    #
    #   'return' value
    class Return < Expression
      attr_accessor :value

      def initialize(value)
        super()

        @value = value
      end

      def accept_children(visitor)
        visitor.accept(@value)
      end
    end

    # Represents a type definition.
    #
    #   'type' name '=' type
    class TypeDefinition < Expression
      attr_accessor :name, :type

      def initialize(name, type)
        super()

        @name = name
        @type = type
      end

      def ==(other)
        other.is_a?(self.class) && @name == other.name && @type == other.type
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

    # Represents a variable declaration.
    #
    #   [ 'let' | 'const' ] name ':' type [ '=' value ]
    class VariableDeclaration < Expression
      attr_accessor :name, :type, :value

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
        other.is_a?(self.class) && @name == other.name && @type == other.type && @value == other.value && @const == other.const?
      end

      def const?
        @const
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

    # Represents a single visibility modifier for a class, method or property.
    #
    #   'public' | 'private' | 'static'
    class Visibility < Expression
      attr_accessor :name

      def initialize(name)
        super()

        @name = name
      end

      def ==(other)
        other.is_a?(self.class) && @name == other.name
      end
    end
  end
end
