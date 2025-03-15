# frozen_string_literal: true

require 'lume/lume_analyzer/errors'
require 'lume/lume_analyzer/symbol_table'

module Lume
  class Analyzer # :nodoc:
    private

    # Passes the AST through the main visitor, which handles the expansion of result
    # types of expressions within it.
    #
    # @param ast [Lume::IR::AST]  The AST to be analyzed.
    #
    # @see MainVisitor
    def visit_main(ast)
      visitor = MainVisitor.new
      visitor.visit(ast)
    end

    # The main visitor class, which is responsible for descending the AST and
    # expanding the result types of expressions within it.
    #
    # In essence, when the compiler sees an statement like this:
    #
    #   const a = 10 + 20
    #
    # It recursively expands the type of each expression within it, so that `10` becomes `Int32`, `20` becomes `Int32`,
    # resulting in `a` being of type `Int32`. The final type of the statement, `a`, would then be `Int32`.
    #
    # This is important for the type checker, as it requires all expressions to have a known type. Otherwise, it cannot
    # perform type checking and may produce incorrect results.
    class MainVisitor
      include Lume::Analyzer::Errors
      include Lume::IR
      include Lume::IR::Visitor

      SCALAR_TYPES = %w[
        String
        Int8
        UInt8
        Int16
        UInt16
        Int32
        UInt32
        Int64
        UInt64
        Float
        Double
        Boolean
      ].freeze

      LITERAL_TYPE_MAP = {
        StringLiteral => 'String',
        ByteLiteral => 'Int8',
        UnsignedByteLiteral => 'UInt8',
        ShortLiteral => 'Int16',
        UnsignedShortLiteral => 'UInt16',
        WordLiteral => 'Int32',
        UnsignedWordLiteral => 'UInt32',
        LongLiteral => 'Int64',
        UnsignedLongLiteral => 'UInt64',
        FloatLiteral => 'Float',
        DoubleLiteral => 'Double',
        BooleanLiteral => 'Boolean',
        NilLiteral => 'Nil'
      }.freeze

      def initialize
        @symbols = SymbolTable.new
      end

      # Accepts an AST and runs it through the visitor.
      #
      # @param ast [Lume::IR::AST] The AST to be analyzed.
      def visit(ast)
        prediscover(ast)
        accept_ast(ast)
      end

      private

      # Visits an argument expression and resolves it's argument types.
      #
      # @param expression [Argument] The argument expression to be visited.
      def accept_argument(expression)
        expression.expression_type = scalar_of(expression.value.expression_type)
      end

      # Visits a class definition and registers it in the symbol table.
      #
      # @param expression [ClassDefinition] The class definition to be visited.
      def accept_class_definition(expression)
        @symbols.define(expression)
      end

      # Visits a function invocation expression and resolves it's type from the symbol table.
      #
      # @param expression [FunctionCall] The function invocation expression to be visited.
      def accept_function_call(expression)
        # Find the function definition in the global symbol table
        expression.reference = @symbols.retrieve(expression.action, type: FunctionDefinition)

        # If no function with the given name was found, raise an error.
        raise UndefinedSymbol.new(expression, name: expression.action) if expression.reference.nil?

        # Label all the arguments with their parameter names
        map_argument_names(expression.arguments, expression.reference.parameters)

        # Resolve the expression type from the function return type
        expression.expression_type = expression.reference.return
      end

      # Visits a function definition and registers its arguments within the symbol table.
      #
      # @param _ [FunctionDefinition] The function definition to be visited.
      def before_function_definition(_)
        @symbols.push_boundary
      end

      # Visits a function definition and pops its arguments off the symbol table.
      #
      # @param _ [FunctionDefinition] The function definition to be visited.
      def accept_function_definition(_)
        @symbols.pop_boundary
      end

      # Visits a heap allocation expression.
      #
      # @param expression [HeapAllocation] The expression to be visited.
      def accept_heap_allocation(expression)
        expression.expression_type = scalar_of(expression.type)
      end

      # Visits an literal expression and resolves it's expression type.
      #
      # @param expression [Literal] The literal expression to be visited.
      def accept_literal(expression)
        expression.expression_type = scalar_of(LITERAL_TYPE_MAP[expression.class])
      end

      # Visits a method invocation expression and resolves it's type from the symbol table.
      #
      # @param expression [MethodCall] The method invocation expression to be visited.
      def accept_method_call(expression)
        class_name = expression.class_instance_name
        class_def = @symbols.retrieve(class_name, type: ClassDefinition)

        expression.reference = class_def.method(expression.action)
        expression.expression_type = expression.reference.return
      end

      # Visits a method definition and pushes its arguments onto the symbol table.
      #
      # @param _ [MethodDefinition] The method definition to be visited.
      def before_method_definition(_)
        @symbols.push_boundary
      end

      # Visits a method definition and pops its arguments off the symbol table.
      #
      # @param _ [MethodDefinition] The method definition to be visited.
      def accept_method_definition(_)
        @symbols.pop_boundary
      end

      # Visits an object initialization expression.
      #
      # @param expression [New] The expression to be visited.
      def accept_new(expression)
        class_name = NamedType.new(expression.class_def.name)
        expression.expression_type = scalar_of(class_name)

        constructor = expression.class_def.constructor

        # If no constructor was explicitly defined, there's nothing to map against.
        return if constructor.nil?

        # Label all the arguments with their parameter names
        map_argument_names(expression.arguments, constructor.parameters)
      end

      # Visits a function parameter expression and resolves it's argument types.
      #
      # @param expression [Parameter] The function parameter expression to be visited.
      def accept_parameter(expression)
        @symbols.define(expression)
      end

      # Visits a return expression and resolves it's type from the symbol table.
      #
      # @param expression [Return] The return expression to be visited.
      def accept_return(expression)
        expression.expression_type = expression.value.expression_type
      end

      # Visits a scalar literal and resolves it's type from the symbol table.
      #
      # @param expression [Scalar] The scalar literal to be visited.
      def accept_scalar(expression)
        expression.expression_type = expression
      end

      # Visits a union literal and resolves it's type from the symbol table.
      #
      # @param expression [Union] The scalar literal to be visited.
      def accept_union(expression)
        expression.types.each { |type| accept(type) }

        expression.expression_type = expression
      end

      # Visits a variable reference expression and resolves it's type from the symbol table.
      #
      # @param expression [Variable] The variable reference expression to be visited.
      def accept_variable(expression)
        expression.reference = @symbols.retrieve(expression.name)

        # If no variable with the given name was found, raise an error.
        raise UndefinedSymbol.new(expression) if expression.reference.nil?

        expression.expression_type = expression_type(expression.reference)
      end

      # Visits a variable declaration expression and adds it to the symbol table.
      #
      # @param expression [VariableDeclaration] The variable declaration expression to be visited.
      def accept_variable_declaration(expression)
        expression.type ||= expression.value.expression_type
        expression.expression_type = expression.type

        @symbols.define(expression)
      end

      # Prediscovers all class- and function-definitions within the AST and adds them to the symbol table.
      #
      # @param ast [Lume::IR::AST] The AST to be analyzed.
      def prediscover(ast)
        discover_class_definitions(ast)
        discover_function_definitions(ast)
      end

      # Discovers all class definitions within the AST and adds them to the symbol table.
      #
      # @param ast [Lume::IR::AST] The AST to be analyzed.
      def discover_class_definitions(ast)
        class_definitions = ast.nodes.select { |node| node.is_a?(ClassDefinition) }

        class_definitions.each do |class_definition|
          @symbols.define(class_definition)
        end
      end

      # Discovers all function definitions within the AST and adds them to the symbol table.
      #
      # @param ast [AST] The AST to be analyzed.
      def discover_function_definitions(ast)
        function_definitions = ast.nodes.select { |node| node.is_a?(FunctionDefinition) }

        function_definitions.each do |definition|
          @symbols.define(definition, type: definition.return)
        end
      end

      # Gets or creates a scalar type with the given name.
      #
      # @param name [String] The name of the scalar type.
      #
      # @return [Scalar] The scalar type.
      def scalar_of(name)
        if name.is_a?(String)
          scalar = Scalar.new(name)
        elsif name.is_a?(Scalar) || name.is_a?(NamedType)
          scalar = name
        else
          raise TypeError, "Expected String, Scalar, or NamedType, got #{name.class}"
        end

        scalar.expression_type = scalar
        scalar
      end

      # Maps the names of all the arguments to their corresponding parameter names.
      #
      # @param arguments [Array<Argument>]    The arguments to be mapped.
      # @param parameters [Array<Parameter>]  The parameters to be mapped.
      #
      # @return [void]
      def map_argument_names(arguments, parameters)
        arguments.each_with_index do |arg, index|
          arg.name = parameters[index]&.name
        end
      end

      # Determines the expression type of the given expression.
      #
      # @param expression [Expression]  The expression to determine the type of.
      #
      # @return [Type]
      def expression_type(expression)
        # Types are their own expression types.
        return expression if expression.is_a?(Type)

        # Variable declarations resolve to their own type.
        return expression_type(expression.type) if expression.is_a?(VariableDeclaration)

        # Parameters resolve to their own type.
        return expression_type(expression.type) if expression.is_a?(Parameter)

        # Function definitions resolve to their return type.
        return expression_type(expression.return) if expression.is_a?(FunctionDefinition)

        nil
      end
    end
  end
end
