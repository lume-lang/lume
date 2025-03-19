# frozen_string_literal: true

require 'lume/lume_mir/mir'
require 'lume/lume_mir/types'
require 'lume/lume_mir/values'

module Lume
  module Lowering
    # Generator which lowers all modules from HIR (High-Level Intermediate Representation) to MIR
    # (Middle-Level Intermediate Representation), which can then be further lowered to LLVM IR.
    class Generator
      def initialize
        @classes = {}
        @class_stack = []
      end

      # Generates MIR from the given HIR abstract syntax tree (AST).
      #
      # @param ast [Lume::Syntax::AST] The abstract syntax tree to generate MIR from.
      #
      # @return [Lume::MIR::AST] The generated MIR.
      def generate(ast)
        ir = Lume::MIR::AST.new

        # Pre-register all the class definitions within the AST, so we
        # can reference them later without having to re-traverse the tree.
        iterate_class_nodes(ast)

        # Generate the rest of the IR nodes.
        ir.nodes.concat(generate_ir_nodes(ast.nodes))

        # Pre-declare all the functions definitions within the IR, so we
        # can call them without them being defined yet.
        declare_function_definitions(ir)

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
        Lume::Syntax::ByteLiteral => Lume::MIR::ByteLiteral,
        Lume::Syntax::UnsignedByteLiteral => Lume::MIR::UnsignedByteLiteral,
        Lume::Syntax::ShortLiteral => Lume::MIR::ShortLiteral,
        Lume::Syntax::UnsignedShortLiteral => Lume::MIR::UnsignedShortLiteral,
        Lume::Syntax::WordLiteral => Lume::MIR::WordLiteral,
        Lume::Syntax::UnsignedWordLiteral => Lume::MIR::UnsignedWordLiteral,
        Lume::Syntax::LongLiteral => Lume::MIR::LongLiteral,
        Lume::Syntax::UnsignedLongLiteral => Lume::MIR::UnsignedLongLiteral,
        Lume::Syntax::FloatLiteral => Lume::MIR::FloatLiteral,
        Lume::Syntax::DoubleLiteral => Lume::MIR::DoubleLiteral,
        Lume::Syntax::StringLiteral => Lume::MIR::StringLiteral,
        Lume::Syntax::BooleanLiteral => Lume::MIR::BooleanLiteral,
        Lume::Syntax::NullLiteral => Lume::MIR::NullLiteral
      }.freeze

      private

      # Iterates over all the class definition nodes in the given AST and registers them in the generator.
      #
      # @param ast [Lume::Syntax::AST] The abstract syntax tree to iterate over.
      def iterate_class_nodes(ast)
        class_definitions = ast.nodes.select { |node| node.is_a?(Lume::Syntax::ClassDefinition) }

        class_definitions.each do |class_def|
          @classes[class_def.name] = generate_ir_node(class_def)
        end
      end

      # Iterates over all the function definition nodes in the given IR and forward-declares them.
      #
      # @param ir [Lume::MIR::AST] The abstract syntax tree to iterate over.
      #
      # @return [void]
      def declare_function_definitions(ir)
        # Select all the function definitions within the IR.
        definitions = ir.find_all_recursively(Lume::MIR::FunctionDefinition)

        declarations = definitions.map do |definition|
          Lume::MIR::FunctionDeclaration.new(definition.full_name, definition.parameters, definition.return)
        end

        ir.nodes.insert(0, *declarations)
      end

      def generate_ir_nodes(nodes)
        nodes.map { |node| generate_ir_node(node) }
      end

      def generate_ir_node(node)
        ir = generate_ir_statement(node)

        # Copy the location information from the node into the new IR node.
        ir.location = node.location if node.is_a?(Lume::Syntax::Node) && !node.nil?

        ir
      end

      # Visits a statement node in the AST and generates LLVM IR.
      #
      # @param node [Lume::Syntax::Node] The node to visit.
      #
      # @return [Lume::MIR::Node]
      def generate_ir_statement(node)
        case node
        when Lume::Syntax::Expression then generate_ir_expression(node)
        when Lume::Syntax::Literal then generate_ir_literal(node)
        when Lume::Syntax::Parameter then generate_ir_parameter(node)
        when Lume::Syntax::Argument then generate_ir_node(node.value)
        when Lume::Syntax::Type then generate_ir_type(node)
        when Lume::Syntax::Token then generate_ir_variable(node.value)
        when String then generate_ir_variable(node)
        when nil then nil
        else
          raise "Unsupported node type: #{node.class}"
        end
      end

      # Visits an expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::Expression] The expression to visit.
      #
      # @return [Lume::MIR::Expression]
      def generate_ir_expression(expression)
        ir = case expression
        when Lume::Syntax::Assignment then generate_ir_assignment(expression)
        when Lume::Syntax::VariableDeclaration then generate_ir_variable_declaration(expression)
        when Lume::Syntax::VariableReference then generate_ir_variable_reference(expression)
        when Lume::Syntax::ClassDefinition then generate_ir_class_definition(expression)
        when Lume::Syntax::MethodDefinition then generate_ir_method_definition(expression)
        when Lume::Syntax::TypeDefinition then generate_ir_type_definition(expression)
        when Lume::Syntax::Call then generate_ir_call(expression)
        when Lume::Syntax::Return then generate_ir_return(expression)
        when Lume::Syntax::New then generate_ir_new(expression)
        when Lume::Syntax::Property then generate_ir_property(expression)
        when Lume::Syntax::MemberAccess then generate_ir_member_access(expression)
        when Lume::Syntax::Visibility then generate_ir_visibility(expression)
        else
          raise "Unsupported expression type: #{expression.class}"
        end

        # Copy the comment, if it exists.
        ir.comment = expression.comment&.content

        ir
      end

      # Visits an assignment expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::Assignment] The expression to visit.
      #
      # @return [Lume::MIR::Assignment]
      def generate_ir_assignment(expression)
        Lume::MIR::Assignment.new(
          generate_ir_node(expression.target),
          generate_ir_node(expression.value)
        )
      end

      # Visits a variable declaration expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::VariableDeclaration] The expression to visit.
      #
      # @return [Lume::MIR::VariableDeclaration]
      def generate_ir_variable_declaration(expression)
        Lume::MIR::VariableDeclaration.new(
          expression.name,
          generate_ir_node(expression.type),
          generate_ir_node(expression.value),
          const: expression.const?
        )
      end

      # Visits a variable reference expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::VariableReference] The expression to visit.
      #
      # @return [Lume::MIR::Variable]
      def generate_ir_variable_reference(expression)
        Lume::MIR::Variable.new(expression.name)
      end

      # Visits a class definition expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::ClassDefinition] The expression to visit.
      #
      # @return [Lume::MIR::ClassDefinition]
      def generate_ir_class_definition(expression)
        # If the class is already defined, return it as-is, since we need referential integrity.
        return @classes[expression.name] if @classes[expression.name]

        class_def = Lume::MIR::ClassDefinition.new(expression.name, [])

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
      # @param expression [Lume::Syntax::MethodDefinition] The expression to visit.
      #
      # @return [Lume::MIR::FunctionDefinition]
      def generate_ir_function_definition(expression)
        name = expression.name
        parameters = generate_ir_nodes(expression.parameters)
        expressions = generate_ir_nodes(expression.expressions)
        return_type = generate_ir_node(expression.return)

        function = Lume::MIR::FunctionDefinition.new(name, parameters, return_type, expressions)
        function.external = expression.external

        function
      end

      # Visits a function declaration expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::MethodDefinition] The expression to visit.
      #
      # @return [Lume::MIR::FunctionDeclaration]
      def generate_ir_function_declaration(expression)
        name = expression.name
        parameters = generate_ir_nodes(expression.parameters)
        return_type = generate_ir_node(expression.return)

        Lume::MIR::FunctionDeclaration.new(name, parameters, return_type)
      end

      # Visits a method definition expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::MethodDefinition] The expression to visit.
      #
      # @return [Lume::MIR::MethodDefinition]
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

        method = Lume::MIR::MethodDefinition.new(class_def, name, parameters, return_type, expressions)
        method.visibility = visibility
        method.external = expression.external

        method
      end

      # Wraps a constructor method definition expression node in the required prologue and epilogue.
      #
      # @param expression [Lume::MIR::MethodDefinition] The expression to visit.
      #
      # @return [Lume::MIR::MethodDefinition]
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
        expression.return = Lume::MIR::NamedType.new(expression.class_def.name)

        expression
      end

      # Inserts the prologue for a constructor method definition expression node.
      #
      # @param expression [Lume::MIR::MethodDefinition] The expression to insert the prologue into.
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
        instance_type = Lume::MIR::NamedType.new(expression.class_def.name)
        instance = Lume::MIR::VariableDeclaration.new('self', instance_type, const: true)

        # Invoke the allocator to allocate memory for the instance.
        instance.value = Lume::MIR::HeapAllocation.new(instance_type, expression.class_def.bytesize)

        expression.expressions.unshift(instance)
      end

      # Inserts the epilogue for a constructor method definition expression node.
      #
      # @param expression [Lume::MIR::MethodDefinition] The expression to insert the epilogue into.
      #
      # @return [void]
      def insert_constructor_epilogue(expression)
        # Appends a new expression to return the 'self' variable from the constructor.
        # This is functionally identical to writing the following at the end of the constructor:
        #
        #   return self

        instance_variable = Lume::MIR::Variable.new('self')
        return_expression = Lume::MIR::Return.new(instance_variable)

        expression.expressions.push(return_expression)
      end

      # Visits a type definition expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::TypeDefinition] The expression to visit.
      #
      # @return [Lume::MIR::TypeDefinition]
      def generate_ir_type_definition(expression)
        name = expression.name
        type = generate_ir_node(expression.type)

        Lume::MIR::TypeDefinition.new(name, type)
      end

      # Visits a invocation expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::Call] The expression to visit.
      #
      # @return [Lume::MIR::Call]
      def generate_ir_call(expression)
        name = expression.action

        # Map all the arguments to their IR representations.
        arguments = generate_ir_nodes(expression.arguments).map do |arg|
          Lume::MIR::Argument.new(nil, arg)
        end

        # If no target is defined, it's a function call
        return Lume::MIR::FunctionCall.new(name, *arguments) if expression.target.nil?

        # Otherwise, handle it as a method call
        Lume::MIR::MethodCall.new(generate_ir_node(expression.target), name, *arguments)
      end

      # Visits a return expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::Return] The expression to visit.
      #
      # @return [Lume::MIR::Return]
      def generate_ir_return(expression)
        value = generate_ir_node(expression.value)

        Lume::MIR::Return.new(value)
      end

      # Visits an object initialization expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::New] The expression to visit.
      #
      # @return [Lume::MIR::MethodCall]
      def generate_ir_new(expression)
        class_def = @classes[expression.class_name]
        raise "Invalid class: #{expression.class_name}" if class_def.nil?

        # Map all the arguments to their IR representations.
        arguments = generate_ir_nodes(expression.arguments).map do |arg|
          Lume::MIR::Argument.new(nil, arg)
        end

        Lume::MIR::New.new(class_def, *arguments)
      end

      # Visits a property definition expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::Property] The expression to visit.
      #
      # @return [Lume::MIR::Property]
      def generate_ir_property(expression)
        name = expression.name
        type = generate_ir_node(expression.type)
        default = generate_ir_node(expression.default)
        visibility = generate_ir_nodes(expression.visibility)

        Lume::MIR::Property.new(name, type, default, visibility)
      end

      # Visits a member access expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::MemberAccess] The expression to visit.
      #
      # @return [Lume::MIR::MemberAccess]
      def generate_ir_member_access(expression)
        target = generate_ir_node(expression.target)
        property = expression.property

        Lume::MIR::MemberAccess.new(target, property)
      end

      # Visits a visibility expression node in the AST and generates LLVM IR.
      #
      # @param expression [Lume::Syntax::Visibility] The expression to visit.
      #
      # @return [Lume::MIR::Visibility]
      def generate_ir_visibility(expression)
        Lume::MIR::Visibility.new(expression.name)
      end

      # Visits a literal expression node in the AST and generates LLVM IR.
      #
      # @param literal [Lume::Syntax::Literal] The expression to visit.
      #
      # @return [Lume::MIR::Literal]
      def generate_ir_literal(literal)
        raise 'Invalid literal type' unless LITERALS_MAP.key?(literal.class)

        LITERALS_MAP[literal.class].new(literal.value)
      end

      # Visits a parameter expression node in the AST and generates LLVM IR.
      #
      # @param parameter [Lume::Syntax::Parameter] The expression to visit.
      #
      # @return [Lume::MIR::Parameter]
      def generate_ir_parameter(parameter)
        type = generate_ir_node(parameter.type)

        Lume::MIR::Parameter.new(parameter.name, type)
      end

      # Visits a type node in the AST and generates LLVM IR.
      #
      # @param type [Lume::Syntax::Type] The type to visit.
      #
      # @return [Lume::MIR::Type]
      def generate_ir_type(type)
        case type
        when Lume::Syntax::Void then generate_ir_void_type
        when Lume::Syntax::NamedType then generate_ir_named_type(type)
        when Lume::Syntax::Pointer then generate_ir_pointer_type(type)
        when Lume::Syntax::Union then generate_ir_union_type(type)
        else
          raise "Unsupported type: #{type.class}"
        end
      end

      # Visits a void type node in the AST and generates LLVM IR.
      #
      # @return [Lume::MIR::Void]
      def generate_ir_void_type
        Lume::MIR::Void.new
      end

      # Visits a named type node in the AST and generates LLVM IR.
      #
      # @param type [Lume::Syntax::NamedType] The type to visit.
      #
      # @return [Lume::MIR::Scalar]
      def generate_ir_named_type(node)
        # The named type is a built-in alias, resolve it first.
        node.name = TYPE_ALIAS_MAP[node.name] || node.name

        # If the type name is a scalar type, return a Scalar
        return Lume::MIR::Scalar.new(node.name) if SCALAR_TYPES.include?(node.name)

        # Otherwise, return a named type
        Lume::MIR::NamedType.new(node.name)
      end

      # Visits a pointer type node in the AST and generates LLVM IR.
      #
      # @param type [Lume::Syntax::Pointer] The type to visit.
      #
      # @return [Lume::MIR::Pointer]
      def generate_ir_pointer_type(node)
        Lume::MIR::Pointer.new(node.of)
      end

      # Visits a union type node in the AST and generates LLVM IR.
      #
      # @param type [Lume::Syntax::Union] The type to visit.
      #
      # @return [Lume::MIR::Union]
      def generate_ir_union_type(type)
        ir_types = generate_ir_nodes(type.types)

        Lume::MIR::Union.new(ir_types)
      end

      # Visits a variable reference node in the AST and generates LLVM IR.
      #
      # @param variable [String] The variable reference to visit.
      #
      # @return [Lume::MIR::Variable]
      def generate_ir_variable(variable)
        Lume::MIR::Variable.new(variable)
      end

      # Executes the block while the given class definition is on the stack.
      #
      # @param expression [Lume::Syntax::ClassDefinition] The class definition to push onto the stack.
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
end
