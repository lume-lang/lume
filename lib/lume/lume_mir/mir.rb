# frozen_string_literal: true

require 'lume/lume_mir_visitor/flat_visitor'

module Lume
  module MIR
    # Represents the abstract syntax tree.
    #
    # This AST is part of the MIR (Middle-Level Intermediate Representation). The MIR is the second pass around
    # analyzing the source code of source file(s). It is produced after "lowering" the High-Level Intermediate
    # Representation (HIR) into a representation that is easier to analyze and manipulate. This representation is
    # meant to be closer to what LLVM will see before linking the final binary.
    #
    # None of the nodes within the AST are responsible for any semantic analysis or type checking - they are only
    # responsible for representing the source code. For this, it's the perfect format to handle name resolution,
    # "de-sugaring" (converting special syntax into a more standard form) and type checking.
    #
    # MIR is also more referential than HIR. For example, instead of variable nodes referencing a variable by name,
    # they reference the variable declaration node itself. This is important for the analysis stage, since all
    # nodes are expected to be altered during the analysis process. If there are duplicate nodes within the tree
    # which refer to the same node, but is actually a different object, the analyzer would only analyze and
    # update one of them, which could lead to incorrect analysis results.
    class AST
      attr_accessor :nodes, :sources

      printer_ignore :sources

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

      # Finds all nodes of the given type, recursively.
      #
      # @param type [Class] The type of node to find
      #
      # @return [Array<Node>] The root nodes of the given type.
      def find_all_recursively(type)
        @nodes.flat_map { |node| node.find_all(type) }
      end
    end

    # Represents a node within an abstract syntax tree.
    class Node
      attr_accessor :expression_type, :location

      printer_ignore :expression_type, :location, :ir

      # Defines the LLVM IR representation of the node.
      #
      # The value of `ir` is defined in the `CODEGEN` stage and will remain `nil` until then.
      attr_accessor :ir

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

    # Represents a class definition.
    #
    #   'class' name '{'
    #     expressions
    #   '}'
    class ClassDefinition < Expression
      attr_accessor :name, :expressions, :builtin

      def initialize(name, expressions, builtin: false)
        super()

        @name = name
        @expressions = expressions
        @builtin = builtin
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

      # Gets a new named type, which references this class definition.
      def type
        NamedType.new(name).tap do |type|
          type.reference = self
        end
      end

      # Determines whether the class definition is a built-in type.
      #
      # @return [Boolean] `true` if the class definition is a built-in type, `false` otherwise.
      def builtin?
        @builtin
      end
    end

    # Represents a conditional expression.
    #
    #   'if' condition '{'
    #     then
    #   '}'
    #   [ 'else if' else_if '{' else_if '}' ]*
    #   [ 'else' else '{' else '}' ]
    class Conditional < Expression
      attr_accessor :condition, :then, :else_if, :else

      def initialize(condition: nil, then_block: [], else_if: [], else_block: [])
        super()

        @condition = condition
        @then = then_block
        @else_if = else_if
        @else = else_block
      end

      def accept_children(visitor)
        visitor.accept(@condition)

        @then.each { |block| visitor.accept(block) }
        @else_if.each { |block| visitor.accept(block) }
        @else.each { |block| visitor.accept(block) }
      end

      def ==(other)
        other.is_a?(self.class) &&
          @condition == other.condition &&
          @then == other.then &&
          @else_if == other.else_if &&
          @else == other.else
      end
    end

    # Represents a function invocation.
    #
    #   action '(' arguments [ ',' arguments ]* ')'
    class FunctionCall < Call; end

    # Represents a function declaration.
    #
    #   'fn' name '(' parameters [ ',' parameters ]* ')' ':' return
    # |
    #   'fn' 'external' name '(' parameters [ ',' parameters ]* ')' ':' return
    class FunctionDeclaration < Expression
      attr_accessor :name, :parameters, :return

      def initialize(name, parameters, return_value)
        super()

        @name = name
        @parameters = parameters
        @return = return_value
      end

      def ==(other)
        return false unless other.is_a?(self.class)

        @name == other.name &&
          @parameters == other.parameters &&
          @return == other.return
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

      # Determines the full name of the function.
      #
      # @return [String] Full name of the function.
      def full_name
        @name
      end
    end

    # Represents a function definition.
    #
    #   'fn' name '(' parameters [ ',' parameters ]* ')' '->' return '{'
    #     expressions
    #   '}'
    # |
    #   'fn' 'external' name '(' parameters [ ',' parameters ]* ')' '->' return
    class FunctionDefinition < FunctionDeclaration
      attr_accessor :expressions, :external

      def initialize(name, parameters, return_value, expressions)
        super(name, parameters, return_value)

        @expressions = expressions
      end

      def accept_children(visitor)
        @parameters.each { |ex| visitor.accept(ex) }
        @expressions.each { |ex| visitor.accept(ex) }

        visitor.accept(@return)
      end

      def ==(other)
        super && @expressions == other.expressions
      end

      # Determines whether the function is external.
      #
      # @return [Boolean]
      def external?
        @external
      end
    end

    # Represents a allocation expression, which allocates memory on the heap.
    class HeapAllocation < Allocation
    end

    # Represents a member access expression on a target object.
    #
    #   target '.' property
    class MemberAccess < Access
    end

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
        # If the instance has a ClassDefinition reference, it's static.
        @instance.nil? || @instance.is_a?(ClassDefinition) || @instance.reference.is_a?(ClassDefinition)
      end

      # Determines whether the referenced method is intrinsic.
      #
      # @return [Boolean] `true` if the method is intrinsic, `false` otherwise.
      def intrinsic?
        return false if @reference.nil?

        @reference.intrinsic?
      end

      # Gets the name of the class instance being accessed.
      #
      # @return [String]
      def class_instance_name
        # If the instance refers to a class definition, return the name of the class.
        return instance.name if instance.is_a?(ClassDefinition)

        # Otherwise, return the name of the expression type.
        instance.expression_type.name
      end
    end

    # Represents a method definition.
    #
    #   visibility* 'fn' name '(' parameters [ ',' parameters ]* ')' '->' return '{'
    #     expressions
    #   '}'
    # |
    #   visibility* 'fn' 'external' name '(' parameters [ ',' parameters ]* ')' '->' return
    class MethodDefinition < FunctionDefinition
      attr_accessor :class_def, :visibility

      def initialize(class_def, name, parameters, return_value, expressions)
        super(name, parameters, return_value, expressions)

        @class_def = class_def
        @visibility = []
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

      # Determines whether the method is intrinsic.
      #
      # Intrinsic methods are implemented by the compiler.
      #
      # @return [Boolean] `true` if the method is intrinsic, `false` otherwise.
      def intrinsic?
        return false if @class_def.nil?

        @class_def.builtin? && external?
      end

      # Determines the full name of the method.
      #
      # @return [String] Full name of the method.
      def full_name
        "#{@class_def.name}::#{@name}"
      end
    end

    # Represents a negation for a sub-expression.
    #
    #   '!' expression
    class Negation < Expression
      attr_accessor :expression

      def initialize(expression)
        super()

        @expression = expression
      end

      def accept_children(visitor)
        visitor.accept(@expression)
      end

      def ==(other)
        other.is_a?(self.class) && @expression == other.expression
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

    # Represents a property definition in a class.
    #
    #   visibility* name ( ':' type [ '=' default ] | [':' type] '=' default )
    class Property < Expression
      attr_accessor :name, :type, :default, :visibility

      def initialize(name, type, default, visibility)
        super()

        @name = name
        @type = type
        @default = default || NullLiteral.new
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

    # Represents a allocation expression, which allocates memory on the stack.
    class StackAllocation < Allocation
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

    # Represents a variable declaration.
    #
    #   [ 'let' | 'const' ] name ':' type [ '=' value ]
    class VariableDeclaration < Expression
      attr_accessor :name, :type, :value, :const

      def initialize(name, type, value = nil, const: false)
        super()

        @name = name
        @type = type
        @value = value || NullLiteral.new
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
  end
end
