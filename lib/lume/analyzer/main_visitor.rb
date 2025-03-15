# frozen_string_literal: true

module Lume
  class Analyzer # :nodoc:
    private

    # Passes the AST through the main visitor, which handles the expansion of result
    # types of expressions within it.
    #
    # @param ast [Lume::Analyzer::IR::AST]  The AST to be analyzed.
    #
    # @see MainVisitor
    def visit_main(ast)
      visitor = MainVisitor.new
      visitor.visit(ast)
    end
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
    include Lume::Analyzer::IR
    include Lume::Analyzer::Visitor

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
      @functions = {}
      @symbols = []
      @expression_types = {}
    end

    # Accepts an AST and runs it through the visitor.
    #
    # @param ast [Lume::Analyzer::IR::AST] The AST to be analyzed.
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

    # Visits a function invocation expression and resolves it's type from the symbol table.
    #
    # @param expression [FunctionCall] The function invocation expression to be visited.
    def accept_function_call(expression)
      # Find the function definition in the global symbol table
      expression.reference = @functions[expression.action]

      # Label all the arguments with their parameter names
      map_argument_names(expression.arguments, expression.reference.parameters)

      # Resolve the expression type from the function return type
      expression.expression_type = expression.reference.return
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
      class_def = retrieve_symbol(class_name)

      expression.reference = class_def.method(expression.action)
      expression.expression_type = expression.reference.return
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
      add_symbol(expression.name, expression)
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
      expression.reference = retrieve_symbol(expression.name)
      expression.expression_type = expression_type(expression.reference)
    end

    # Visits a variable declaration expression and adds it to the symbol table.
    #
    # @param expression [VariableDeclaration] The variable declaration expression to be visited.
    def accept_variable_declaration(expression)
      expression.type ||= expression.value.expression_type
      expression.expression_type = expression.type

      add_symbol(expression.name, expression)
    end

    # Prediscovers all class- and function-definitions within the AST and adds them to the symbol table.
    #
    # @param ast [Lume::Analyzer::IR::AST] The AST to be analyzed.
    def prediscover(ast)
      discover_class_definitions(ast)
      discover_function_definitions(ast)
    end

    # Discovers all class definitions within the AST and adds them to the symbol table.
    #
    # @param ast [Lume::Analyzer::IR::AST] The AST to be analyzed.
    def discover_class_definitions(ast)
      class_definitions = ast.nodes.select { |node| node.is_a?(ClassDefinition) }

      class_definitions.each do |class_definition|
        add_symbol(class_definition.name, class_definition)
      end
    end

    # Discovers all function definitions within the AST and adds them to the symbol table.
    #
    # @param ast [Lume::Analyzer::IR::AST] The AST to be analyzed.
    def discover_function_definitions(ast)
      function_definitions = ast.nodes.select { |node| node.is_a?(FunctionDefinition) }

      function_definitions.each do |definition|
        add_symbol(definition.name, definition.return)

        @functions[definition.name] = definition
      end
    end

    # Gets or creates a scalar type with the given name.
    #
    # @param name [String] The name of the scalar type.
    #
    # @return [Scalar] The scalar type.
    def scalar_of(name)
      if name.is_a?(String)
        scalar = @expression_types[name] ||= Scalar.new(name)
      elsif name.is_a?(Scalar) || name.is_a?(NamedType)
        scalar = @expression_types[name] ||= name
      else
        raise TypeError, "Expected String, Scalar, or NamedType, got #{name.class}"
      end

      scalar.expression_type = scalar
      scalar
    end

    # Pushes a new symbol scope with the given symbols into the symbol table.
    #
    # This is usually called when a function is invoked or block scope starts.
    #
    # @param symbols [Hash] The symbols to be defined in the new scope. Defaults to an empty hash.
    def push_frame(symbols = {})
      @symbols.push(symbols)
    end

    # Pops the current symbol scope from the symbol table.
    #
    # This is usually called when a function or block scope ends.
    def pop_frame
      @symbols.pop
    end

    # Appends a new symbol to the current symbol scope.
    #
    # This is usually called when a new variable is introduced within an existing block scope.
    #
    # @param name         [String]  The name of the symbol to be added.
    # @param type         [Type]    The type of the symbol to be added.
    def add_symbol(name, type)
      # If the symbol table is empty, push a new frame before adding the symbol.
      push_frame if @symbols.empty?

      # Push the declaration into the last symbol frame, keyed by its name.
      @symbols[-1][name] = type
    end

    # Retrieves a symbol from the current symbol scope, with the given name.
    #
    # @param name   [String]  The name of the symbol to retrieve.
    #
    # @return [Type]
    def retrieve_symbol(name)
      @symbols.reverse_each do |frame|
        return frame[name] if frame.key?(name)
      end

      nil
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
