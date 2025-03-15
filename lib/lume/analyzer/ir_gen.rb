# frozen_string_literal: true

require_relative 'ir'

module Lume
  class IRGen # :nodoc:
    def initialize
      @classes = {}
      @class_stack = []
    end

    # Generates CompilerIR from the given abstract syntax tree (AST).
    #
    # @param ast [Lume::Language::AST] The abstract syntax tree to generate IR from.
    #
    # @return [Lume::Analyzer::IR] The generated compiler IR.
    def generate(ast)
      # Pre-register all the class definitions within the AST, so we
      # can reference them later without having to re-traverse the tree.
      iterate_class_nodes(ast)

      ir = Lume::Analyzer::IR::AST.new
      ir.nodes = generate_ir_nodes(ast.nodes)

      ir
    end

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

    TYPE_ALIAS_MAP = {
      'string' => 'String',
      'byte' => 'UInt8',
      'sbyte' => 'Int8',
      'short' => 'Int16',
      'ushort' => 'UInt16',
      'int' => 'Int32',
      'integer' => 'Int32',
      'uint' => 'UInt32',
      'long' => 'Int64',
      'ulong' => 'UInt64',
      'float' => 'Float',
      'double' => 'Double',
      'bool' => 'Boolean',
      'boolean' => 'Boolean'
    }.freeze

    LITERALS_MAP = {
      Lume::Language::ByteLiteral => Lume::Analyzer::IR::ByteLiteral,
      Lume::Language::UnsignedByteLiteral => Lume::Analyzer::IR::UnsignedByteLiteral,
      Lume::Language::ShortLiteral => Lume::Analyzer::IR::ShortLiteral,
      Lume::Language::UnsignedShortLiteral => Lume::Analyzer::IR::UnsignedShortLiteral,
      Lume::Language::WordLiteral => Lume::Analyzer::IR::WordLiteral,
      Lume::Language::UnsignedWordLiteral => Lume::Analyzer::IR::UnsignedWordLiteral,
      Lume::Language::LongLiteral => Lume::Analyzer::IR::LongLiteral,
      Lume::Language::UnsignedLongLiteral => Lume::Analyzer::IR::UnsignedLongLiteral,
      Lume::Language::FloatLiteral => Lume::Analyzer::IR::FloatLiteral,
      Lume::Language::DoubleLiteral => Lume::Analyzer::IR::DoubleLiteral,
      Lume::Language::StringLiteral => Lume::Analyzer::IR::StringLiteral,
      Lume::Language::BooleanLiteral => Lume::Analyzer::IR::BooleanLiteral,
      Lume::Language::NilLiteral => Lume::Analyzer::IR::NilLiteral
    }.freeze

    private

    # Iterates over all the class definition nodes in the given AST and registers them in the generator.
    #
    # @param ast [Lume::Language::AST] The abstract syntax tree to iterate over.
    def iterate_class_nodes(ast)
      class_definitions = ast.nodes.select { |node| node.is_a?(Lume::Language::ClassDefinition) }

      class_definitions.each do |class_def|
        @classes[class_def.name] = generate_ir_node(class_def)
      end
    end

    def generate_ir_nodes(nodes)
      nodes.map { |node| generate_ir_node(node) }
    end

    def generate_ir_node(node)
      ir = generate_ir_statement(node)

      # Copy the location information from the node into the new IR node.
      ir.location = node.location if node.is_a?(Lume::Language::Node) && !node.nil?

      ir
    end

    # Visits a statement node in the AST and generates LLVM IR.
    #
    # @param node [Lume::Language::Node] The node to visit.
    #
    # @return [Lume::Analyzer::IR::Node]
    def generate_ir_statement(node)
      case node
      when Lume::Language::Expression then generate_ir_expression(node)
      when Lume::Language::Literal then generate_ir_literal(node)
      when Lume::Language::Parameter then generate_ir_parameter(node)
      when Lume::Language::Argument then generate_ir_node(node.value)
      when Lume::Language::Type then generate_ir_type(node)
      when Lume::Language::Token then generate_ir_variable(node.value)
      when String then generate_ir_variable(node)
      when nil then nil
      else
        raise "Unsupported node type: #{node.class}"
      end
    end

    # Visits an expression node in the AST and generates LLVM IR.
    #
    # @param expression [Lume::Language::Expression] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::Expression]
    def generate_ir_expression(expression)
      case expression
      when Lume::Language::Assignment then generate_ir_assignment(expression)
      when Lume::Language::VariableDeclaration then generate_ir_variable_declaration(expression)
      when Lume::Language::VariableReference then generate_ir_variable_reference(expression)
      when Lume::Language::ClassDefinition then generate_ir_class_definition(expression)
      when Lume::Language::MethodDefinition then generate_ir_method_definition(expression)
      when Lume::Language::TypeDefinition then generate_ir_type_definition(expression)
      when Lume::Language::Call then generate_ir_call(expression)
      when Lume::Language::Return then generate_ir_return(expression)
      when Lume::Language::New then generate_ir_new(expression)
      when Lume::Language::Property then generate_ir_property(expression)
      when Lume::Language::MemberAccess then generate_ir_member_access(expression)
      when Lume::Language::Visibility then generate_ir_visibility(expression)
      else
        raise "Unsupported expression type: #{expression.class}"
      end
    end

    # Visits a variable declaration expression node in the AST and generates LLVM IR.
    #
    # @param expression [Lume::Language::VariableDeclaration] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::VariableDeclaration]
    def generate_ir_variable_declaration(expression)
      Lume::Analyzer::IR::VariableDeclaration.new(
        expression.name,
        generate_ir_node(expression.type),
        generate_ir_node(expression.value),
        const: expression.const?
      )
    end

    # Visits a variable reference expression node in the AST and generates LLVM IR.
    #
    # @param expression [Lume::Language::VariableReference] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::Variable]
    def generate_ir_variable_reference(expression)
      Lume::Analyzer::IR::Variable.new(expression.name)
    end

    # Visits a class definition expression node in the AST and generates LLVM IR.
    #
    # @param expression [Lume::Language::ClassDefinition] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::ClassDefinition]
    def generate_ir_class_definition(expression)
      # If the class is already defined, return it as-is, since we need referential integrity.
      return @classes[expression.name] if @classes[expression.name]

      class_def = Lume::Analyzer::IR::ClassDefinition.new(expression.name, [])

      with_class(class_def) do
        class_def.expressions = generate_ir_nodes(expression.expressions)
      end

      # If the class has a constructor, wrap it in the required prologue and epilogue.
      # If no constructor is defined, create a dummy constructor and wrap it.
      constructor = class_def.constructor || class_def.dummy_constructor
      wrap_constructor_definition(constructor)

      class_def
    end

    # Visits a function definition expression node in the AST and generates LLVM IR.
    #
    # @param expression [Lume::Language::MethodDefinition] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::FunctionDefinition]
    def generate_ir_function_definition(expression)
      name = expression.name
      parameters = generate_ir_nodes(expression.parameters)
      expressions = generate_ir_nodes(expression.expressions)
      return_type = generate_ir_node(expression.return)

      Lume::Analyzer::IR::FunctionDefinition.new(name, parameters, return_type, expressions)
    end

    # Visits a method definition expression node in the AST and generates LLVM IR.
    #
    # @param expression [Lume::Language::MethodDefinition] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::MethodDefinition]
    def generate_ir_method_definition(expression)
      # If the class stack is empty, we're not within a class scope.
      # By that definition, the following expression is a function definition, not a method definition.
      return generate_ir_function_definition(expression) if @class_stack.empty?

      class_def = @class_stack[-1]
      raise "Invalid class node: #{expression.name}" if class_def.nil?

      name = expression.name
      visibility = generate_ir_nodes(expression.visibility)
      parameters = generate_ir_nodes(expression.parameters)
      expressions = generate_ir_nodes(expression.expressions)
      return_type = generate_ir_node(expression.return)

      method = Lume::Analyzer::IR::MethodDefinition.new(class_def, name, parameters, return_type, expressions)
      method.visibility = visibility

      method
    end

    # Wraps a constructor method definition expression node in the required prologue and epilogue.
    #
    # @param expression [Lume::Analyzer::IR::MethodDefinition] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::MethodDefinition]
    #
    # @see #insert_constructor_prologue
    # @see #insert_constructor_epilogue
    def wrap_constructor_definition(expression)
      # Insert the instance allocation expressions into the beginning of the method.
      # The allocation expression is required for class-instances, as they are allocated on the heap.
      #
      # It also initializes the instance's fields to their default values, if any are defined.
      insert_constructor_prologue(expression)

      # The epilogue simply returns the `self` variable, which is created in the prologue.
      insert_constructor_epilogue(expression)

      # Set the return type of the constructor method, since it's often left out.
      expression.return = Lume::Analyzer::IR::NamedType.new(expression.class_def.name)

      expression
    end

    # Inserts the prologue for a constructor method definition expression node.
    #
    # @param expression [Lume::Analyzer::IR::MethodDefinition] The expression to insert the prologue into.
    #
    # @return [void]
    def insert_constructor_prologue(expression)
      # Prepend a new expression to create the 'self' variable.
      # This is functionally identical to writing the following at the start of the constructor:
      #
      #   const self: [class_name] = lume_malloc(bytesize)
      #
      # Where `[class_name]` is the name of the class being constructed.

      # Create a new variable declaration for 'self'
      instance_type = Lume::Analyzer::IR::NamedType.new(expression.class_def.name)
      instance = Lume::Analyzer::IR::VariableDeclaration.new('self', instance_type, const: true)

      # Invoke the allocator to allocate memory for the instance.
      instance.value = Lume::Analyzer::IR::HeapAllocation.new(instance_type, expression.class_def.bytesize)

      expression.expressions.unshift(instance)
    end

    # Inserts the epilogue for a constructor method definition expression node.
    #
    # @param expression [Lume::Analyzer::IR::MethodDefinition] The expression to insert the epilogue into.
    #
    # @return [void]
    def insert_constructor_epilogue(expression)
      # Appends a new expression to return the 'self' variable from the constructor.
      # This is functionally identical to writing the following at the end of the constructor:
      #
      #   return self

      instance_variable = Lume::Analyzer::IR::Variable.new('self')
      return_expression = Lume::Analyzer::IR::Return.new(instance_variable)

      expression.expressions.push(return_expression)
    end

    # Visits a type definition expression node in the AST and generates LLVM IR.
    #
    # @param expression [Lume::Language::TypeDefinition] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::TypeDefinition]
    def generate_ir_type_definition(expression)
      name = expression.name
      type = generate_ir_node(expression.type)

      Lume::Analyzer::IR::TypeDefinition.new(name, type)
    end

    # Visits a invocation expression node in the AST and generates LLVM IR.
    #
    # @param expression [Lume::Language::Call] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::Call]
    def generate_ir_call(expression)
      name = expression.action

      # Map all the arguments to their IR representations.
      arguments = generate_ir_nodes(expression.arguments).map do |arg|
        Lume::Analyzer::IR::Argument.new(nil, arg)
      end

      # If no target is defined, it's a function call
      return Lume::Analyzer::IR::FunctionCall.new(name, *arguments) if expression.target.nil?

      # Otherwise, handle it as a method call
      Lume::Analyzer::IR::MethodCall.new(generate_ir_node(expression.target), name, *arguments)
    end

    # Visits a return expression node in the AST and generates LLVM IR.
    #
    # @param expression [Lume::Language::Return] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::Return]
    def generate_ir_return(expression)
      value = generate_ir_node(expression.value)

      Lume::Analyzer::IR::Return.new(value)
    end

    # Visits an object initialization expression node in the AST and generates LLVM IR.
    #
    # @param expression [Lume::Language::New] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::MethodCall]
    def generate_ir_new(expression)
      class_def = @classes[expression.class_name]
      raise "Invalid class: #{expression.class_name}" if class_def.nil?

      # Map all the arguments to their IR representations.
      arguments = generate_ir_nodes(expression.arguments).map do |arg|
        Lume::Analyzer::IR::Argument.new(nil, arg)
      end

      Lume::Analyzer::IR::New.new(class_def, *arguments)
    end

    # Visits a property definition expression node in the AST and generates LLVM IR.
    #
    # @param expression [Lume::Language::Property] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::Property]
    def generate_ir_property(expression)
      name = expression.name
      type = generate_ir_node(expression.type)
      default = generate_ir_node(expression.default)
      visibility = generate_ir_nodes(expression.visibility)

      Lume::Analyzer::IR::Property.new(name, type, default, visibility)
    end

    # Visits a member access expression node in the AST and generates LLVM IR.
    #
    # @param expression [Lume::Language::MemberAccess] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::MemberAccess]
    def generate_ir_member_access(expression)
      target = generate_ir_node(expression.target)
      property = expression.property

      Lume::Analyzer::IR::MemberAccess.new(target, property)
    end

    # Visits a visibility expression node in the AST and generates LLVM IR.
    #
    # @param expression [Lume::Language::Visibility] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::Visibility]
    def generate_ir_visibility(expression)
      Lume::Analyzer::IR::Visibility.new(expression.name)
    end

    # Visits a literal expression node in the AST and generates LLVM IR.
    #
    # @param literal [Lume::Language::Literal] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::Literal]
    def generate_ir_literal(literal)
      raise 'Invalid literal type' unless LITERALS_MAP.key?(literal.class)

      LITERALS_MAP[literal.class].new(literal.value)
    end

    # Visits a parameter expression node in the AST and generates LLVM IR.
    #
    # @param parameter [Lume::Language::Parameter] The expression to visit.
    #
    # @return [Lume::Analyzer::IR::Parameter]
    def generate_ir_parameter(parameter)
      type = generate_ir_node(parameter.type)

      Lume::Analyzer::IR::Parameter.new(parameter.name, type)
    end

    # Visits a type node in the AST and generates LLVM IR.
    #
    # @param type [Lume::Language::Type] The type to visit.
    #
    # @return [Lume::Analyzer::IR::Type]
    def generate_ir_type(type)
      case type
      when Lume::Language::Void then generate_ir_void_type
      when Lume::Language::NamedType then generate_ir_named_type(type)
      when Lume::Language::Pointer then generate_ir_pointer_type(type)
      when Lume::Language::Union then generate_ir_union_type(type)
      else
        raise "Unsupported type: #{type.class}"
      end
    end

    # Visits a void type node in the AST and generates LLVM IR.
    #
    # @return [Lume::Analyzer::IR::Void]
    def generate_ir_void_type
      Lume::Analyzer::IR::Void.new
    end

    # Visits a named type node in the AST and generates LLVM IR.
    #
    # @param type [Lume::Language::NamedType] The type to visit.
    #
    # @return [Lume::Analyzer::IR::Scalar]
    def generate_ir_named_type(node)
      # The named type is a built-in alias, resolve it first.
      node.name = TYPE_ALIAS_MAP[node.name] || node.name

      # If the type name is a scalar type, return a Scalar
      return Lume::Analyzer::IR::Scalar.new(node.name) if SCALAR_TYPES.include?(node.name)

      # Otherwise, return a named type
      Lume::Analyzer::IR::NamedType.new(node.name)
    end

    # Visits a pointer type node in the AST and generates LLVM IR.
    #
    # @param type [Lume::Language::Pointer] The type to visit.
    #
    # @return [Lume::Analyzer::IR::Pointer]
    def generate_ir_pointer_type(node)
      Lume::Analyzer::IR::Pointer.new(node.of)
    end

    # Visits a union type node in the AST and generates LLVM IR.
    #
    # @param type [Lume::Language::Union] The type to visit.
    #
    # @return [Lume::Analyzer::IR::Union]
    def generate_ir_union_type(type)
      ir_types = generate_ir_nodes(type.types)

      Lume::Analyzer::IR::Union.new(ir_types)
    end

    # Visits a variable reference node in the AST and generates LLVM IR.
    #
    # @param variable [String] The variable reference to visit.
    #
    # @return [Lume::Analyzer::IR::Variable]
    def generate_ir_variable(variable)
      Lume::Analyzer::IR::Variable.new(variable)
    end

    # Executes the block while the given class definition is on the stack.
    #
    # @param expression [Lume::Language::ClassDefinition] The class definition to push onto the stack.
    #
    # @yield The block to execute.
    def with_class(expression)
      @class_stack.push(expression)
      yield
    ensure
      @class_stack.pop
    end
  end
end
