# frozen_string_literal: true

require 'lume/lume_ir_visitor/flat_visitor'

module Lume
  module IR
    # Represents the abstract syntax tree.
    class AST
      attr_accessor :nodes, :sources

      def initialize
        @nodes = []
        @sources = {}
      end

      # Finds the first root node of the given type.
      #
      # @param type [Class] The type of node to find
      #
      # @return [Node] The first node of the given type, or `nil` if not found
      def find_first(type)
        @nodes.find { |node| node.is_a?(type) }
      end

      # Finds all root nodes of the given type.
      #
      # @param type [Class] The type of node to find
      #
      # @return [Array<Node>] The root nodes of the given type.
      def find_all(type)
        @nodes.select { |node| node.is_a?(type) }
      end
    end

    # Represents a node within an abstract syntax tree.
    class Node
      attr_accessor :expression_type, :location

      # Finds all the nodes of the given type within the current node.
      #
      # @param type [Class] The type of node to find.
      #
      # @return [Array<Node>] An array of nodes of the given type.
      def find_all(type)
        nodes = FlatVisitor.flatten(self)
        nodes.select { |node| node.is_a?(type) }
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

      def arg(name:)
        Argument.new(name, self)
      end

      # Determines the size of the node in bytes.
      #
      # @return [Integer] the size of the node in bytes
      def bytesize
        raise 'Literal::bytesize must be implemented by subclasses.'
      end
    end

    # Represents a single argument.
    #
    #   name ':' value
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

      # Casts the argument value to the given type.
      #
      # @param to [Type] Type to cast to
      def cast_to(to)
        @value = Cast.new(@value, to)
      end
    end

    # Represents a single parameter.
    #
    #   name ':' type
    class Parameter < Node
      attr_writer :default
      attr_accessor :name, :type

      def initialize(name, type, default: nil)
        super()

        @name = name
        @type = type
        @default = default
      end

      def accept_children(visitor)
        visitor.accept(@type)
      end

      def ==(other)
        other.is_a?(Parameter) && @name == other.name && @type == other.type
      end

      def default?
        !@default.nil?
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

      # Casts the RHS to the given type.
      #
      # @param to [Type] Type to cast to
      def cast_to(to)
        @value = Cast.new(@value, to)
      end
    end

    # Represents a function- or method-invocation.
    #
    #   [ target '.' ] action '(' arguments [ ',' arguments ]* ')'
    class Call < Expression
      attr_accessor :action, :arguments, :reference

      def initialize(action, *args)
        super()

        @action = action
        @arguments = args
        @reference = nil
      end

      def accept_children(visitor)
        visitor.accept(@action) if @action.is_a?(Node)

        @arguments.each { |arg| visitor.accept(arg) }
      end

      def ==(other)
        other.is_a?(self.class) && @action == other.action && @arguments == other.arguments
      end

      # Gets the amount of arguments passed.
      #
      # @return [Integer] Number of arguments passed
      def argument_count
        @arguments.length
      end

      # Gets the full name of the call.
      #
      # @return [String] Full name of the call
      def full_name
        reference.full_name
      end
    end

    # Represents a function invocation.
    #
    #   action '(' arguments [ ',' arguments ]* ')'
    class FunctionCall < Call; end

    # Represents a method invocation.
    #
    #   instance '.' action '(' arguments [ ',' arguments ]* ')'
    class MethodCall < Call
      attr_accessor :instance

      def initialize(instance, action, *)
        super(action, *)

        @instance = instance
      end

      def accept_children(visitor)
        visitor.accept(@instance) if @instance.is_a?(Node)

        super
      end

      def ==(other)
        super && @instance == other.instance
      end

      # Determines whether the method call is a static call.
      #
      # This method determines whether the method call is static by checking the `instance` attribute:
      #   - If the `instance` is `nil`, the method call is static.
      #   - If the `instance` is a `ClassDefinition`, the method call is static.
      #   - If the `instance` refers to some other type, it's not static.
      #
      # @return [Boolean]
      def static?
        return true if @instance.nil?

        # If the instance has a ClassDefinition reference, it's static.
        instance.respond_to?(:reference) && @instance.reference.is_a?(ClassDefinition)
      end

      # Gets a reference to the class instance being accessed.
      #
      # @return [ClassDefinition]
      def class_instance_name
        return instance.reference.name if instance.respond_to?(:reference) && instance.reference.is_a?(ClassDefinition)

        instance.expression_type.name
      end
    end

    # Represents an object initialization expression.
    #
    #   'new' class '(' arguments [ ',' arguments ]* ')'
    class New < Expression
      attr_accessor :class_def, :arguments

      def initialize(class_def, *arguments)
        super()

        @class_def = class_def
        @arguments = *arguments
      end

      def accept_children(visitor)
        @arguments.each { |arg| visitor.accept(arg) }
      end

      def ==(other)
        other.is_a?(self.class) && @class_def == other.class_def && @arguments == other.arguments
      end

      # Determines the full name of the method.
      #
      # @return [String] Full name of the method.
      def full_name
        "#{@class_def.name}::#{@class_def.name}"
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

      # Casts the returned value to the given type.
      #
      # @param to [Type] Type to cast to
      def cast_to(to)
        @value = Cast.new(@value, to)
      end
    end

    # Represents a class definition.
    #
    #   'class' name
    #     expressions
    #   'end'
    class ClassDefinition < Expression
      attr_accessor :name, :expressions

      def initialize(name, expressions)
        super()

        @name = name
        @expressions = expressions
      end

      def accept_children(visitor)
        @expressions.each { |ex| visitor.accept(ex) }
      end

      # Merges the current class definition with another one.
      #
      # @param other [ClassDefinition] The other class definition to merge with.
      #
      # @return [void]
      def merge_with(other)
        @expressions.concat(other.expressions)
      end

      # Gets the methods defined on the class definition.
      #
      # @return [Array<MethodDefinition>] The methods defined on the class definition.
      def methods
        @expressions.select { |ex| ex.is_a?(MethodDefinition) }
      end

      # Gets the method defined on the class definition with the given name.
      #
      # @param name [String] The name of the method to get.
      #
      # @return [nil|MethodDefinition] The method defined on the class definition with the given name.
      def method(name)
        methods.find { |ex| ex.name == name }
      end

      # Gets the constructor method on the class definition.
      #
      # @return [nil|MethodDefinition] The constructor method on the class definition, if any. Otherwise, `nil`.
      def constructor
        method(@name)
      end

      # Creates a new dummy constructor, if an explicit constructor wasn't defined.
      #
      # @return [MethodDefinition] The dummy constructor method.
      def dummy_constructor
        # If a constructor is already defined, return it instead.
        return constructor unless constructor.nil?

        constructor = MethodDefinition.new(self, @name, [], Void.new, [])

        # Put the constructor at the beginning of the class definition
        expressions.prepend(constructor)

        constructor
      end

      # Determines the size of the class definition in bytes.
      #
      # @return [Integer] The size of the class definition in bytes.
      def bytesize
        size = 0

        properties = @expressions.select { |ex| ex.is_a?(Property) }
        size += properties.reduce(0) { |acc, prop| acc + prop.type.bytesize }

        size
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
        return true if other.is_a?(String) && @name.casecmp?(other)

        other.is_a?(self.class) && @name == other.name
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

      def accept_children(visitor)
        visitor.accept(@type)
      end

      def ==(other)
        other.is_a?(self.class) && @name == other.name && @type == other.type
      end
    end

    # Represents a property definition in a class.
    #
    #   visibility* name ( ':' type [ '=' default ] | [':' type] '=' default )
    class Property < Expression
      attr_accessor :name, :type, :default, :visibility

      def initialize(name, type, default, visibility)
        super()

        @name = name
        @type = type
        @default = default || NilLiteral.new
        @visibility = visibility
      end

      def accept_children(visitor)
        visitor.accept(@visibility) unless @visibility.nil?
        visitor.accept(@type) unless @type.nil?
        visitor.accept(@default) unless @default.nil?
      end

      def ==(other)
        return false unless other.is_a?(self.class)

        @name == other.name && @type == other.type && @default == other.default && @visibility == other.visibility
      end

      # Casts the default value to the given type.
      #
      # @param to [Type] Type to cast to
      def cast_to(to)
        @default = Cast.new(@default, to) unless @default.nil?
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

      def accept_children(visitor)
        visitor.accept(@target) unless @target.nil?
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

    # Represents a function definition.
    #
    #   'fn' name '(' parameters [ ',' parameters ]* ')' ':' return
    #     expressions
    #   'end'
    class FunctionDefinition < Expression
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

        @name == other.name &&
          @parameters == other.parameters &&
          @return == other.return &&
          @expressions == other.expressions
      end

      # Gets the range of required parameter counts, going from the least amount of parameters
      # required to the maximum.
      #
      # For example, given the function:
      #
      #   fn test(a: int, b: string)
      #     # ...
      #   end
      #
      # `parameter_count` would return `2..2`, since the function always requires `2` parameters.
      #
      # This function:
      #
      #   fn test(a: int, b: string = 1)
      #     # ...
      #   end
      #
      # `parameter_count` would return `1..2`, since the function requires at least `1` parameter, but can
      # take `2` parameters, if provided.
      #
      # @return [Range] Range of parameters required.
      def parameter_count
        max = @parameters.length
        min = max - @parameters.count(&:default?)

        min..max
      end
    end

    # Represents a method definition.
    #
    #   visibility* 'fn' name '(' parameters [ ',' parameters ]* ')' ':' return
    #     expressions
    #   'end'
    class MethodDefinition < FunctionDefinition
      attr_accessor :class_def, :visibility

      def initialize(class_def, name, parameters, return_value, expressions)
        super(name, parameters, return_value, expressions)

        @class_def = class_def
      end

      def ==(other)
        super && @class_def == other.class_def && @visibility == other.visibility
      end

      def accept_children(visitor)
        super

        visitor.accept(@visibility) if @visibility.nil?
      end

      # Determines if the method is a constructor.
      #
      # @return [Boolean] `true` if the method is a constructor, `false` otherwise.
      def constructor?
        @name == @class_def.name
      end

      # Determines if the method is static.
      #
      # @return [Boolean] `true` if the method is static, `false` otherwise.
      def static?
        @visibility.any? { |v| v == 'static' }
      end

      # Determines the full name of the method.
      #
      # @return [String] Full name of the method.
      def full_name
        "#{@class_def.name}::#{@name}"
      end
    end

    # Represents an abstract allocation expression.
    class Allocation < Expression
      attr_accessor :type, :size

      def initialize(type, size)
        super()

        @type = type
        @size = size
      end

      def ==(other)
        other.is_a?(self.class) && @type == other.type && @size == other.size
      end
    end

    # Represents a allocation expression, which allocates memory on the heap.
    class HeapAllocation < Allocation
    end

    # Represents a allocation expression, which allocates memory on the stack.
    class StackAllocation < Allocation
    end

    # Represents a cast expression, which casts a value to a different type.
    class Cast < Expression
      attr_accessor :value, :type

      def initialize(value, type)
        super()

        @value = value
        @type = type
      end

      def accept_children(visitor)
        visitor.accept(@value)
        visitor.accept(@type)
      end

      def ==(other)
        other.is_a?(self.class) && @value == other.value && @type == other.type
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
        other.is_a?(self.class) &&
          @name == other.name &&
          @type == other.type &&
          @value == other.value &&
          @const == other.const
      end

      def const?
        @const
      end

      # Casts the variable value to the given type.
      #
      # @param to [Type] Type to cast to
      def cast_to(to)
        @value = Cast.new(@value, to) unless @value.nil?
      end
    end

    # Represents a variable reference.
    #
    #   name
    class Variable < Expression
      attr_accessor :name, :reference

      def initialize(name)
        super()

        @name = name
        @reference = nil
      end

      def accept_children(visitor)
        visitor.accept(@reference) unless @reference.nil?
      end

      def ==(other)
        other.is_a?(self.class) && @name == other.name && @reference == other.reference
      end
    end

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
