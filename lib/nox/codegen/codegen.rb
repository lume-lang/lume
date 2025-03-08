# frozen_string_literal: true

require 'llvm/core'
require 'llvm/execution_engine'
require 'llvm/transforms/scalar'

module Nox
  module Codegen # :nodoc:
    MAIN_NAME = 'main'

    # initializes the backend components for Nox (such as LLVM, JIT, etc.)
    #
    # @return [void]
    def initialize_codegen!
      LLVM.init_jit

      @module = LLVM::Module.new('nox')
      @engine = LLVM::JITCompiler.new(@module)
      @pass_manager = LLVM::PassManager.new
      @builder = LLVM::Builder.new
      @target_machine = @engine.target_machine

      @visitor = NodeVisitor.new(@module, @engine, @builder)
    end

    # Finishes the compilation process.
    #
    # @return [void]
    def finish
      @builder.dispose
      @module.dispose
    end

    # Optimizes the module using LLVM's optimization passes.
    #
    # @return [void]
    def optimize!
      @pass_manager.run(@module)
    end

    # Compiles the given node expressions into LLVM IR.
    #
    # @param node [Nox::Language::Node] The node to compile.
    #
    # @return [void]
    def codegen_node!(node)
      @visitor.visit(node)
    end

    # Dumps the LLVM module to `stdout`.
    #
    # @return [void]
    def dump!
      @module.dump

      @module.verify!
    end

    # Emits the LLVM module as an object file to the given file.
    #
    # @param filename [String] The name of the file to emit the module to.
    #
    # @return [void]
    def emit(filename)
      @target_machine.emit(@module, filename, :object)
    end

    # Finishes the compilation process.
    #
    # @return [void]
    def finalize!
      @visitor.finalize!
    end

    # Evaluates / executes the compiled module, as if it were a compiled executable.
    #
    # @param args [Array] The arguments to pass to the module's `main` function.
    #
    # @return [Integer]
    def evaluate(*args)
      argc = args.length
      argv = nil if args.empty?

      @engine.run_function(@engine.functions[MAIN_NAME], argc, argv)
    end
  end

  # Visitor for nodes in the AST, used to generate LLVM IR from the AST.
  class NodeVisitor
    def initialize(mod, engine, builder)
      @module = mod
      @engine = engine
      @builder = builder

      @functions = {}
      @block_stack = []
      @variables = {}

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
    # @param node [Nox::Language::Node] The node to visit.
    #
    # @return [void]
    def visit(node)
      case node
      when Nox::Language::Expression then visit_expression(node)
      when Nox::Language::Literal then visit_literal(node)
      when Nox::Language::Type then visit_type(node)
      when Nox::Language::Token then retrieve_var(node.value)
      when String then retrieve_var(node)
      else
        raise "Unsupported node type: #{node.class}"
      end
    end

    private

    # Visits an expression node in the AST and generates LLVM IR.
    #
    # @param expression [Nox::Language::Expression] The expression to visit.
    #
    # @return [void]
    def visit_expression(expression)
      case expression
      when Nox::Language::Assignment then visit_assignment_expression(expression)
      when Nox::Language::OperatorExpression then visit_operator_expression(expression)
      when Nox::Language::VariableDeclaration then visit_variable_declaration(expression)
      when Nox::Language::VariableReference then visit_variable_reference(expression)
      when Nox::Language::MethodDefinition then visit_method_definition(expression)
      when Nox::Language::FunctionInvocation then visit_function_invocation(expression)
      when Nox::Language::Return then visit_return_expression(expression)
      else
        raise "Unsupported expression type: #{expression.class}"
      end
    end

    # Visits an assignment expression in the AST and generates LLVM IR.
    #
    # @param expression [Nox::Language::Assignment] The expression to visit.
    #
    # @return [void]
    def visit_assignment_expression(expression)
      target = expression.target
      value = visit(expression.value)

      declare_var(target, value)
    end

    # Visits an operator expression node in the AST and generates LLVM IR.
    #
    # @param expression [Nox::Language::OperatorExpression] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_operator_expression(expression)
      case expression
      when Nox::Language::Addition then visit_addition_expression(expression)
      else
        raise "Unsupported operator expression type: #{expression.class}"
      end
    end

    # Visits an addition expression node in the AST and generates LLVM IR.
    #
    # @param expression [Nox::Language::Addition] The expression to visit.
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
    # @param expression [Nox::Language::VariableDeclaration] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_variable_declaration(expression)
      name = expression.name
      value = visit(expression.value)
      type = visit(expression.type)

      declare_var(name, value, type: type)
    end

    # Visits a variable reference expression node in the AST and generates LLVM IR.
    #
    # @param expression [Nox::Language::VariableReference] The expression to visit.
    #
    # @return [LLVM::Value]
    def visit_variable_reference(expression)
      variable_name = expression.name

      retrieve_var(variable_name)
    end

    # Visits a method definition expression node in the AST and generates LLVM IR.
    #
    # @param expression [Nox::Language::MethodDefinition] The expression to visit.
    #
    # @return [LLVM::Value]
    def visit_method_definition(expression)
      method_name = expression.name
      argument_types = expression.arguments.map(&:type).map { |type| visit(type) }
      return_type = visit(expression.return)

      define_function(method_name, argument_types, return_type) do
        expression.expressions.each { |expr| visit(expr) }
      end
    end

    # Visits a function invocation expression node in the AST and generates LLVM IR.
    #
    # @param expression [Nox::Language::FunctionInvocation] The expression to visit.
    #
    # @return [LLVM::Value]
    def visit_function_invocation(expression)
      in_builder_block { invoke(expression.action, expression.arguments) }
    end

    # Visits a return expression node in the AST and generates LLVM IR.
    #
    # @param expression [Nox::Language::Return] The expression to visit.
    #
    # @return [LLVM::Value]
    def visit_return_expression(expression)
      in_builder_block { ret(expression.value) }
    end

    # Visits a literal node in the AST and generates LLVM IR.
    #
    # @param literal [Nox::Language::Literal] The literal to visit.
    #
    # @return [void]
    def visit_literal(literal)
      case literal
      when Nox::Language::NumberLiteral then visit_number_literal(literal)
      when Nox::Language::BooleanLiteral then visit_boolean_literal(literal)
      else
        raise "Unsupported literal type: #{literal.class}"
      end
    end

    # Visits a number literal node in the AST and generates LLVM IR.
    #
    # @param literal [Nox::Language::NumberLiteral] The literal to visit.
    #
    # @return [LLVM::Value]
    def visit_number_literal(literal)
      case literal
      when Nox::Language::ByteLiteral then LLVM::Int8.from_i(literal.value, true)
      when Nox::Language::UnsignedByteLiteral then LLVM::Int8.from_i(literal.value, false)

      when Nox::Language::ShortLiteral then LLVM::Int16.from_i(literal.value, true)
      when Nox::Language::UnsignedShortLiteral then LLVM::Int16.from_i(literal.value, false)

      when Nox::Language::WordLiteral then LLVM::Int32.from_i(literal.value, true)
      when Nox::Language::UnsignedWordLiteral then LLVM::Int32.from_i(literal.value, false)

      when Nox::Language::LongLiteral then LLVM::Int64.from_i(literal.value, true)
      when Nox::Language::UnsignedLongLiteral then LLVM::Int64.from_i(literal.value, false)

      when Nox::Language::FloatLiteral then LLVM.Float(literal.value)
      when Nox::Language::DoubleLiteral then LLVM.Double(literal.value)
      else
        raise "Unsupported number literal type: #{literal.class}"
      end
    end

    # Visits a boolean literal node in the AST and generates LLVM IR.
    #
    # @param literal [Nox::Language::BooleanLiteral] The literal to visit.
    #
    # @return [LLVM::Value]
    def visit_boolean_literal(literal)
      LLVM::Int1.from_i(literal.value == true ? 1 : 0)
    end

    # Visits a type node in the AST and generates LLVM IR.
    #
    # @param type [Nox::Language::Type] The type to visit.
    #
    # @return [void]
    def visit_type(type)
      case type
      when Nox::Language::Void then LLVM.Void
      when Nox::Language::Scalar then visit_scalar_type(type)
      else
        raise "Unsupported type: #{type.class}"
      end
    end

    # Visits a scalar type node in the AST and generates LLVM IR.
    #
    # @param type [Nox::Language::Scalar] The type to visit.
    #
    # @return [void]
    def visit_scalar_type(type)
      case type.name
      when 'Int8', 'UInt8' then LLVM::Int8
      when 'Int16', 'UInt16' then LLVM::Int16
      when 'Int32', 'UInt32' then LLVM::Int32
      when 'Int64', 'UInt64' then LLVM::Int64
      when 'Boolean' then LLVM::Int1
      when 'Float' then LLVM::Float
      when 'Double' then LLVM::Double
      else
        raise "Unsupported scalar type: #{type.name}"
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

      @main_func = define_function(Codegen::MAIN_NAME, argument_types, return_type)
    end

    # Declares a new variable and initializes it with a value.
    #
    # @param name   [String]            The name of the variable.
    # @param value  [LLVM::Instruction] The value of the variable.
    # @param type   [LLVM::Type]        The type of the variable.
    def declare_var(name, value, type: nil)
      type ||= typeof(value)

      @variables[name] = variable = allocate(type, name)
      assign_var(variable, value)

      variable
    end

    # Allocates a new variable.
    #
    # @param type   [LLVM::Type]  The type of the variable.
    # @param name   [String]      The name of the variable.
    #
    # @return       [LLVM::Instruction] The allocated variable.
    def allocate(type, name = '')
      in_builder_block { @builder.alloca(type, name) }
    end

    # Assigns the value of an existing variable.
    #
    # @param variable [LLVM::Value] The variable to assign to.
    # @param value    [LLVM::Value] The value to assign.
    def assign_var(variable, value)
      @builder.store(value, variable)
    end

    # Retrieves the value of an existing variable.
    #
    # @param name [LLVM::Value] The variable to retrieve.
    #
    # @return [LLVM::Value] The value of the variable.
    def retrieve_var(name)
      variable = name
      variable = @variables[name] if name.is_a?(String)

      raise "Undefined variable: '#{name}'" if variable.nil?

      @builder.load(variable)
    end

    # Defines a new function with the given name, argument types, and return type.
    #
    # @param name           [String]            The name of the function.
    # @param argument_types [Array<LLVM::Type>] The types of the function's arguments.
    # @param return_type    [LLVM::Type]        The type of the function's return value.
    #
    # @return [LLVM::Function] The defined function.
    def define_function(name, argument_types, return_type)
      @functions[name] = @module.functions.add(name, argument_types, return_type) do |function, *arguments|
        entry = function.basic_blocks.append('entry')

        @builder.position_at_end(entry)
        @block_stack.push(entry)

        yield(function, *arguments) if block_given?

        @builder.position_at_end(@block_stack.pop)
      end
    end

    # Creates a new instruction to invoke a function.
    #
    # @param target     [String|LLVM::Function]       The function to invoke.
    # @param arguments  [Array<Nox::Language::Node>]  The arguments to pass to the function.
    #
    # @return [LLVM::Instruction] The invoke instruction.
    def invoke(target, arguments)
      # If the target is a string, it should be referencing an existing function
      if target.is_a?(String)
        raise ArgumentError, "Function not found: #{target}" unless @functions.key?(target)

        target = @functions[target]
      end

      # If the target is an expression, visit it to get the LLVM function
      target = visit(target) if target.is_a?(Nox::Language::Expression)

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
      value = visit(value) if value.is_a?(Nox::Language::Node)

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

    # Determines if a value is an integral type.
    #
    # @param value [LLVM::Value] The value to get the type of.
    #
    # @return [Boolean] `true` if the value is an integral type, `false` otherwise.
    def integral?(value)
      value.is_a?(LLVM::IntType)
    end
  end
end
