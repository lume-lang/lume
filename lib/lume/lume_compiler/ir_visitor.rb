# frozen_string_literal: true

require 'lume/lume_ir/ir'

module Lume
  # Visitor for nodes in the AST, used to generate LLVM IR from the CompilerIR.
  class NodeVisitor
    include Lume::IR

    LIBC_FUNCTIONS = %w[
      printf
    ].freeze

    def initialize(mod, engine, builder)
      @module = mod
      @engine = engine
      @builder = builder

      @functions = {}
      @block_stack = []

      # Register all the required LibC library functions
      register_libc

      push_main_func
    end

    # Finishes the compilation process.
    def finalize!
      # Ensure that the main function returns an integer value.
      # If needed, it can be overwritten by the generated code.
      in_builder_block do
        ret(LLVM.Int(0))
      end
    end

    # Retrieves the main function definition.
    #
    # @return [LLVM::Function] The main function definition.
    def main
      @main_func
    end

    # Visits a node in the AST and generates LLVM IR.
    #
    # @param node [Node] The node to visit.
    #
    # @return [void]
    def visit(node)
      case node
      when Expression then visit_expression(node)
      when Literal then visit_literal(node)
      when Argument then visit(node.value)
      when Type then visit_type(node)
      else
        raise "Unsupported node type: #{node.class}"
      end
    end

    private

    # Visits an expression node in the AST and generates LLVM IR.
    #
    # @param expression [Expression] The expression to visit.
    #
    # @return [void]
    def visit_expression(expression)
      case expression
      when Assignment then visit_assignment_expression(expression)
      when VariableDeclaration then visit_variable_declaration(expression)
      when Variable then visit_variable_reference(expression)
      when ClassDefinition then visit_class_definition(expression)
      when MethodDefinition then visit_method_definition(expression)
      when FunctionDefinition then visit_function_definition(expression)
      when FunctionDeclaration then visit_function_declaration(expression)
      when FunctionCall then visit_function_call_expression(expression)
      when MethodCall then visit_method_call_expression(expression)
      when Return then visit_return_expression(expression)
      when New then visit_new_expression(expression)
      when Cast then visit_cast_expression(expression)
      when Allocation then visit_allocation_expression(expression)
      else
        raise "Unsupported expression type: #{expression.class}"
      end
    end

    # Visits an assignment expression in the AST and generates LLVM IR.
    #
    # @param expression [Assignment] The expression to visit.
    #
    # @return [void]
    def visit_assignment_expression(expression)
      target = visit(expression.target)
      value = visit(expression.value)

      assign_var(target, value)
    end

    # Visits an operator expression node in the AST and generates LLVM IR.
    #
    # @param expression [OperatorExpression] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_operator_expression(expression)
      case expression
      when Addition then visit_addition_expression(expression)
      else
        raise "Unsupported operator expression type: #{expression.class}"
      end
    end

    # Visits an addition expression node in the AST and generates LLVM IR.
    #
    # @param expression [Addition] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_addition_expression(expression)
      left = visit(expression.left)
      right = visit(expression.right)

      return @builder.add(left, right, 'addtmp') if integral?(left) && integral?(right)

      left = @builder.i2fp(left, typeof(left)) if integral?(left)
      right = @builder.i2fp(right, typeof(right)) if integral?(right)

      @builder.fadd(left, right, 'addtmp')
    end

    # Visits a variable declaration expression node in the AST and generates LLVM IR.
    #
    # @param expression [VariableDeclaration] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_variable_declaration(expression)
      name = expression.name
      value = visit(expression.value)
      type = visit(expression.type)

      expression.ir = declare_var(name, value, type: type)
    end

    # Visits a variable reference expression node in the AST and generates LLVM IR.
    #
    # @param expression [VariableReference] The expression to visit.
    #
    # @return [LLVM::Value]
    def visit_variable_reference(expression)
      variable = expression.reference.ir

      raise "Undefined variable: '#{expression.name}'" if variable.nil?

      variable
    end

    # Visits a class definition expression node in the AST and generates LLVM IR.
    #
    # @param expression [ClassDefinition] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_class_definition(expression)
      methods = expression.expressions.select { |exp| exp.is_a?(MethodDefinition) }

      methods.each { |method| visit_method_definition(method) }
    end

    # Visits a function definition expression node in the AST and generates LLVM IR.
    #
    # @param expression [FunctionDefinition] The expression to visit.
    def visit_function_definition(expression)
      # External methods should already have been defined in the LLVM module.
      return if expression.external?

      method_name = expression.name
      parameter_types = expression.parameters.map(&:type).map { |type| visit(type) }
      return_type = visit(expression.return)

      define_function(method_name, parameter_types, return_type) do |_, *args|
        wrap_parameters(expression.parameters, args)

        expression.expressions.each { |expr| visit(expr) }
      end
    end

    # Visits a method definition expression node in the AST and generates LLVM IR.
    #
    # @param expression [MethodDefinition] The expression to visit.
    def visit_method_definition(expression)
      # External methods should already have been defined in the LLVM module.
      return if expression.external?

      method_name = expression.full_name
      parameter_types = expression.parameters.map(&:type).map { |type| visit(type) }
      return_type = visit(expression.return)

      define_function(method_name, parameter_types, return_type) do |_, *args|
        wrap_parameters(expression.parameters, args)

        expression.expressions.each { |expr| visit(expr) }
      end
    end

    # Visits a function call expression node in the AST and generates LLVM IR.
    #
    # @param expression [FunctionCall] The expression to visit.
    #
    # @return [LLVM::Value]
    def visit_function_call_expression(expression)
      in_builder_block { invoke(expression.full_name, expression.arguments) }
    end

    # Visits a function declaration expression node in the AST and generates LLVM IR.
    #
    # @param expression [FunctionDeclaration] The expression to visit.
    #
    # @return [LLVM::Value]
    def visit_function_declaration(expression)
      function_name = expression.full_name
      parameter_types = expression.parameters.map(&:type).map { |type| visit(type) }
      return_type = visit(expression.return)

      declare_function(function_name, parameter_types, return_type)
    end

    # Visits a method call expression node in the AST and generates LLVM IR.
    #
    # @param expression [MethodCall] The expression to visit.
    #
    # @return [LLVM::Value]
    def visit_method_call_expression(expression)
      in_builder_block { invoke(expression.full_name, expression.arguments) }
    end

    # Visits a return expression node in the AST and generates LLVM IR.
    #
    # @param expression [Return] The expression to visit.
    #
    # @return [LLVM::Value]
    def visit_return_expression(expression)
      in_builder_block { ret(expression.value) }
    end

    # Visits a `new` expression node in the AST and generates LLVM IR.
    #
    # @param expression [New] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_new_expression(expression)
      in_builder_block { invoke(expression.full_name, expression.arguments) }
    end

    # Visits a cast expression node in the AST and generates LLVM IR.
    #
    # @param expression [Cast] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_cast_expression(expression)
      raise NotImplementedError, 'Cannot cast non-literal values' unless expression.value.is_a?(Literal)
      raise NotImplementedError, 'Cannot cast to non-scalar types' unless expression.type.is_a?(Scalar)

      # If the target type is an integer type, handle it separately
      return visit_integer_cast_expression(expression) if expression.type.integer?

      raise NotImplementedError, "Cannot cast to #{expression.type}"
    end

    # Visits an integer cast expression node in the AST and generates LLVM IR.
    #
    # @param expression [Cast] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_integer_cast_expression(expression)
      in_builder_block { cast(expression.value, expression.type) }
    end

    # Visits an allocation expression node in the AST and generates LLVM IR.
    #
    # @param expression [Allocation] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_allocation_expression(expression)
      case expression
      when HeapAllocation then visit_heap_allocation_expression(expression)
      else
        raise "Unsupported allocation expression type: #{expression.class}"
      end
    end

    # Visits a heap allocation expression node in the AST and generates LLVM IR.
    #
    # @param expression [HeapAllocation] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_heap_allocation_expression(expression)
      heap_allocate(expression.size)
    end

    # Visits a literal node in the AST and generates LLVM IR.
    #
    # @param literal [Literal] The literal to visit.
    #
    # @return [void]
    def visit_literal(literal)
      case literal
      when NumberLiteral then visit_number_literal(literal)
      when BooleanLiteral then visit_boolean_literal(literal)
      when StringLiteral then visit_string_literal(literal)
      else
        raise "Unsupported literal type: #{literal.class}"
      end
    end

    # Visits a number literal node in the AST and generates LLVM IR.
    #
    # @param literal [NumberLiteral] The literal to visit.
    #
    # @return [LLVM::Value]
    def visit_number_literal(literal)
      LLVM.i(literal.bytesize * 8, literal.value)
    end

    # Visits a boolean literal node in the AST and generates LLVM IR.
    #
    # @param literal [BooleanLiteral] The literal to visit.
    #
    # @return [LLVM::Value]
    def visit_boolean_literal(literal)
      LLVM::Int1.from_i(literal.value == true ? 1 : 0)
    end

    # Visits a string literal node in the AST and generates LLVM IR.
    #
    # @param literal [StringLiteral] The literal to visit.
    #
    # @return [LLVM::Value]
    def visit_string_literal(literal)
      constant = LLVM::ConstantArray.string(literal.value)

      global_string = @module.globals.add(constant.type, literal.value)
      global_string.initializer = constant
      global_string.linkage = :private

      global_string
    end

    # Visits a type node in the AST and generates LLVM IR.
    #
    # @param type [Type] The type to visit.
    #
    # @return [void]
    def visit_type(type)
      case type
      when Void then LLVM.Void
      when Pointer then LLVM.Pointer(visit(type.of))
      when NamedType then visit_named_type(type)
      when Scalar then visit_scalar_type(type)
      else
        raise "Unsupported type: #{type.class}"
      end
    end

    # Visits a scalar type node in the AST and generates LLVM IR.
    #
    # @param type [Scalar] The type to visit.
    #
    # @return [LLVM::Type]
    def visit_scalar_type(type)
      return visit_integer_scalar_type(type) if type.integer?

      return visit_integer_float_type(type) if type.floating?

      return LLVM.Pointer(LLVM::Int8) if type.string?

      return LLVM::Int1 if type.boolean?

      raise "Unsupported scalar type: #{type.name}"
    end

    # Visits a named type node in the AST and generates LLVM IR.
    #
    # @param type [NamedType] The type to visit.
    #
    # @return [LLVM::Type]
    def visit_named_type(_type)
      LLVM::Pointer(LLVM.Void)
    end

    # Visits an integer scalar type node in the AST and generates LLVM IR.
    #
    # @param type [Scalar] The type to visit.
    #
    # @return [void]
    def visit_integer_scalar_type(type)
      case type.name
      when 'Int8', 'UInt8' then LLVM::Int8
      when 'Int16', 'UInt16' then LLVM::Int16
      when 'Int32', 'UInt32' then LLVM::Int32
      when 'Int64', 'UInt64' then LLVM::Int64
      else
        raise "Unsupported integer scalar type: #{type.name}"
      end
    end

    # Visits a float scalar type node in the AST and generates LLVM IR.
    #
    # @param type [Scalar] The type to visit.
    #
    # @return [void]
    def visit_float_scalar_type(type)
      case type.name
      when 'Float' then LLVM::Float32
      when 'Double' then LLVM::Float64
      else
        raise "Unsupported float scalar type: #{type.name}"
      end
    end

    # Pushes a main entrypoint function onto the function stack.
    #
    # This method ensures that a main function exists, as LLVM requires a main function to be present.
    #
    # @return [LLVM::Function]
    def push_main_func
      argument_types = [LLVM::Int, LLVM::Pointer(LLVM::Pointer(LLVM::Int8))]
      return_type = LLVM::Int

      @main_func = define_function(Compiler::MAIN_NAME, argument_types, return_type)
    end

    # Declares a new variable and initializes it with a value.
    #
    # @param name   [String]            The name of the variable.
    # @param value  [LLVM::Instruction] The value of the variable.
    # @param type   [LLVM::Type]        The type of the variable.
    def declare_var(name, value, type: nil)
      type ||= typeof(value)

      variable = allocate(type, name)
      assign_var(variable, value)

      variable
    end

    # Allocates a new variable on the stack.
    #
    # @param type   [LLVM::Type]  The type of the variable.
    # @param name   [String]      The name of the variable.
    #
    # @return       [LLVM::Instruction] The allocated variable.
    def allocate(type, name = '')
      in_builder_block { @builder.alloca(type, name) }
    end

    # Allocates a block of memory on the heap.
    #
    # @param count  [Integer]  Amount of bytes to allocate.
    #
    # @return       [LLVM::Value] Pointer to the allocated memory block.
    def heap_allocate(count)
      malloc = @functions['malloc']
      size_arg = LLVM.i(64, count)

      in_builder_block { @builder.call(malloc, size_arg) }
    end

    # Casts a literal integer value to a different type.
    #
    # @param value  [Literal] The value to cast.
    # @param type   [LLVM::Type]  The type to cast to.
    #
    # @return       [LLVM::Instruction] The allocated variable.
    def cast(value, type)
      signed = type.signed?

      value = visit(value)
      type = visit(type)

      signed ? cast_sext(value, type) : cast_zext(value, type)
    end

    # Sign-extend casts an integer value to a different type.
    #
    # @param value  [LLVM::Value] The value to cast.
    # @param type   [LLVM::Type]  The type to cast to.
    #
    # @return       [LLVM::Instruction] The allocated variable.
    def cast_sext(value, type)
      in_builder_block { @builder.sext(value, type) }
    end

    # Zero-extend casts an integer value to a different type.
    #
    # @param value  [LLVM::Value] The value to cast.
    # @param type   [LLVM::Type]  The type to cast to.
    #
    # @return       [LLVM::Instruction] The cast value.
    def cast_zext(value, type)
      in_builder_block { @builder.zext(value, type) }
    end

    # Assigns the value of an existing variable.
    #
    # @param variable [LLVM::Value] The variable to assign to.
    # @param value    [LLVM::Value] The value to assign.
    def assign_var(variable, value)
      @builder.store(value, variable)
    end

    # Declares a new function with the given name, argument types, and return type.
    #
    # Declared functions are just like normal functions, but they exist without a body. They are used to
    # declare functions that are defined elsewhere, such as the C standard library functions or forward declarations.
    #
    # @param name           [String]            The name of the function.
    # @param argument_types [Array<LLVM::Type>] The types of the function's arguments.
    # @param return_type    [LLVM::Type]        The type of the function's return value.
    #
    # @return [LLVM::Function] The defined function.
    def declare_function(name, argument_types, return_type, *)
      raise "Function '#{name}' already declared." if func_registered?(name)

      @functions[name] = @module.functions.add(name, argument_types, return_type, *)
    end

    # Defines a new function with the given name, argument types, and return type.
    #
    # @param name           [String]            The name of the function.
    # @param argument_types [Array<LLVM::Type>] The types of the function's arguments.
    # @param return_type    [LLVM::Type]        The type of the function's return value.
    #
    # @return [LLVM::Function] The defined function.
    def define_function(name, argument_types, return_type, *)
      # If the function hasn't been forward declared yet, do so now.
      declare_function(name, argument_types, return_type, *) if @functions[name].nil?

      function = @functions[name]

      # Append a new block to the function, where we can push expressions to.
      entry = function.basic_blocks.append('entry')

      @builder.position_at_end(entry)
      @block_stack.push(entry)

      yield(function, function.params) if block_given?

      @builder.position_at_end(@block_stack.pop)

      # Return the defined function.
      function
    end

    # Creates a new instruction to invoke a function.
    #
    # @param target     [String, LLVM::Function]       The function to invoke.
    # @param arguments  [Array<Node>]  The arguments to pass to the function.
    #
    # @return [LLVM::Instruction] The invoke instruction.
    def invoke(target, arguments)
      # If the target is a string, it should be referencing an existing function
      if target.is_a?(String)
        raise ArgumentError, "Function not found: #{target}" unless @functions.key?(target)

        target = @functions[target]
      end

      # If the target is an expression, visit it to get the LLVM function
      target = visit(target) if target.is_a?(Expression)

      # Convert all function arguments into LLVM expressions
      arguments = arguments.map { |value| visit(value) }

      @builder.call(target, *arguments)
    end

    # Creates a new instruction to return from the current function / method.
    #
    # @param value [LLVM::Value] The value to return.
    #
    # @return [LLVM::Instruction] The return instruction.
    def ret(value)
      # Parse the value into an LLVM expression, if it's an AST node
      value = visit(value) if value.is_a?(Node)

      @builder.ret(value)
    end

    # Executes the given block within a builder block in the current function / scope.
    def in_builder_block
      raise ArgumentError, 'Block required' unless block_given?

      # Gets the last block in the stack or the main block if the stack is empty.
      # If the stack is empty, it means we are outside of any function or scope,
      # which will default to the main function block.
      block = @block_stack[-1] || main.basic_blocks.first

      @builder.position_at_end block

      yield
    end

    # Gets the LLVM type of a value.
    #
    # @param value [Object] The value to get the type of.
    #
    # @return [LLVM::Type] The LLVM type of the value.
    def typeof(value)
      LLVM::Type(value)
    end

    # Wraps the given arguments into the invoked function parameters.
    #
    # @param parameters [Array<Parameter>] The parameters of the function.
    # @param args       [Array<LLVM::Value>] The arguments given to the function.
    #
    # @return [LLVM::Type] The LLVM type of the value.
    def wrap_parameters(parameters, args)
      parameters.each_index do |index|
        parameters[index].ir = args[index]
      end
    end

    # Determines if a value is an integral type.
    #
    # @param value [LLVM::Value] The value to get the type of.
    #
    # @return [Boolean] `true` if the value is an integral type, `false` otherwise.
    def integral?(value)
      value.is_a?(LLVM::IntType)
    end

    # Determines whether the given function name has been registered.
    #
    # @param name [String] The name of the function.
    #
    # @return [Boolean] `true` if the function has been registered, `false` otherwise.
    def func_registered?(name)
      @functions.include?(name)
    end

    # Registers aliases for C library functions, such as `malloc`, `free`, etc.
    #
    # These functions are essential for low-level operations, which cannot be replicated in Lume.
    #
    # @return [void]
    def register_libc
      LIBC_FUNCTIONS.each do |func|
        method("register_#{func}").call
      end
    end

    def register_printf
      declare_function('printf', [LLVM::Int8.pointer], LLVM::Int32, varargs: true)
    end
  end
end
