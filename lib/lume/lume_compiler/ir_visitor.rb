# frozen_string_literal: true

require 'lume/lume_mir/mir'
require 'lume/lume_compiler/conditionals'
require 'lume/lume_compiler/ops'
require 'lume/lume_llvm/module'

module Lume
  # Visitor for nodes in the AST, used to generate LLVM IR from MIR.
  class NodeVisitor
    include Lume::MIR

    attr_reader :module

    LIBC_FUNCTIONS = %w[
      malloc
      realloc
      free
      printf
    ].freeze

    def initialize(name)
      @module = Lume::LLVMModule.new(name)
      @builder = Lume::Compiler::Builder.new(@module.inner)

      # Register all the required LibC library functions
      register_libc
    end

    # Visits a module node and generates LLVM IR.
    #
    # @param mod          [Lume::Module]  The module node to visit.
    #
    # @return [void]
    def self.visit_module(mod)
      visitor = NodeVisitor.new(mod.name)

      # Generate LLVM IR from each of the root nodes within the MIR
      mod.mir.nodes.each { |node| visitor.visit(node) }

      visitor.finalize!

      visitor.module
    end

    # Finishes the compilation process.
    def finalize!
      @builder.dispose
    end

    # Visits a node in the AST and generates LLVM IR.
    #
    # @param node [Node] The node to visit.
    #
    # @return [LLVM::Instruction]
    def visit(node)
      case node
      when Expression then visit_expression(node)
      when Block then visit_many(node.expressions)
      when Literal then visit_literal(node)
      when Argument then visit(node.value)
      when Type then visit_type(node)
      else
        raise "Unsupported node type: #{node.class}"
      end
    end

    # Visits multiple nodes in the AST and generates LLVM IR.
    #
    # @param nodes [Array<Node>] The nodes to visit.
    #
    # @return [Array<LLVM::Instruction>]
    def visit_many(nodes)
      nodes.map { |node| visit(node) }
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
      when Conditional then visit_conditional(expression)
      when MethodDefinition then visit_method_definition(expression)
      when FunctionDefinition then visit_function_definition(expression)
      when FunctionDeclaration then visit_function_declaration(expression)
      when FunctionCall then visit_function_call_expression(expression)
      when MethodCall then visit_method_call_expression(expression)
      when Return then visit_return_expression(expression)
      when New then visit_new_expression(expression)
      when Cast then visit_cast_expression(expression)
      when Negation then visit_negation_expression(expression)
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

      @builder.store(target, value)
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

      expression.ir = @builder.declare_var(name, type, value: value)
    end

    # Visits a variable reference expression node in the AST and generates LLVM IR.
    #
    # @param expression [VariableReference] The expression to visit.
    #
    # @return [LLVM::Value]
    def visit_variable_reference(expression)
      variable = expression.reference.ir

      raise "Undefined variable: '#{expression.name}'" if variable.nil?

      @builder.load(variable)
    end

    # Visits a class definition expression node in the AST and generates LLVM IR.
    #
    # @param expression [ClassDefinition] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_class_definition(expression)
      methods = expression.block.expressions.select { |exp| exp.is_a?(MethodDefinition) }

      methods.each { |method| visit_method_definition(method) }
    end

    # Visits a conditional expression node in the AST and generates LLVM IR.
    #
    # @param expression [Conditional] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_conditional(expression)
      handle_conditional(expression)
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

      func = @builder.define_function(method_name, parameter_types, return_type) do |_, *args|
        generate_function_content(expression, *args)
      end

      define_parameter_names(func, expression.parameters)
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

      func = @builder.define_function(method_name, parameter_types, return_type) do |_, *args|
        generate_function_content(expression, *args)
      end

      define_parameter_names(func, expression.parameters)
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

      @builder.declare_function(function_name, parameter_types, return_type)
    end

    # Visits a method call expression node in the AST and generates LLVM IR.
    #
    # @param expression [MethodCall] The expression to visit.
    #
    # @return [LLVM::Value]
    def visit_method_call_expression(expression)
      # If the method is an intrinsic, handle it accordingly.
      return builtin_op(expression) if expression.intrinsic?

      # Convert all method arguments into LLVM expressions
      arguments = expression.arguments.map { |value| visit(value) }

      @builder.invoke(expression.full_name, arguments)
    end

    # Visits a return expression node in the AST and generates LLVM IR.
    #
    # @param expression [Return] The expression to visit.
    #
    # @return [LLVM::Value]
    def visit_return_expression(expression)
      value = visit(expression.value)

      @builder.ret(value)
    end

    # Visits a `new` expression node in the AST and generates LLVM IR.
    #
    # @param expression [New] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_new_expression(expression)
      # Convert all function arguments into LLVM expressions
      arguments = expression.arguments.map { |value| visit(value) }

      @builder.invoke(expression.full_name, arguments)
    end

    # Visits a cast expression node in the AST and generates LLVM IR.
    #
    # @param expression [Cast] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_cast_expression(expression)
      raise NotImplementedError, 'Cannot cast to non-named types' unless expression.type.is_a?(NamedType)

      cast(expression.value, expression.type)
    end

    # Visits a negation expression node in the AST and generates LLVM IR.
    #
    # @param expression [Negation] The expression to visit.
    #
    # @return [LLVM::Instruction]
    def visit_negation_expression(expression)
      subexpression = visit(expression.expression)

      @builder.not(subexpression)
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
      @builder.allocate(expression.size)
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
      signed = literal.class.signed?

      type = LLVM::Type.i(literal.bytesize * 8)
      type.from_i(literal.value, { signed: signed })
    end

    # Visits a boolean literal node in the AST and generates LLVM IR.
    #
    # @param literal [BooleanLiteral] The literal to visit.
    #
    # @return [LLVM::Value]
    def visit_boolean_literal(literal)
      @builder.bool(value: literal.value)
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
      when Void then @builder.void
      when Pointer then LLVM.Pointer(visit(type.of))
      when NamedType then visit_named_type(type)
      else
        raise "Unsupported type: #{type.class}"
      end
    end

    # Visits a named type node in the AST and generates LLVM IR.
    #
    # @param type [NamedType] The type to visit.
    #
    # @return [LLVM::Type]
    def visit_named_type(type)
      # Integers use N-width int type to store the contents.
      return @builder.int_type(type.width) if type.integer?

      # Floats use N-width real type to store the contents.
      return @builder.float_type(type.width) if type.floating?

      # Strings use a byte array to store the contents.
      return @builder.ptr(@builder.i8_type) if type.string?

      # Pointers use a non-descript pointer type.
      return @builder.void_ptr if type.pointer?

      # Boolean values use a 1-bit integer type.
      return @builder.bool_type if type.boolean?

      # If the referenced class is a non-builtin type, i.e. a non-struct type, reference it with a pointer.
      return @builder.void_ptr unless type.reference.builtin?

      raise "Unsupported scalar type: #{type.name}"
    end

    # Casts a literal value to a different type.
    #
    # @param value  [Literal] The value to cast.
    # @param to     [Type]    The type to cast to.
    #
    # @return       [LLVM::Value] The result of the cast.
    def cast(value, to)
      from = value.expression_type

      # Integer -> Integer
      return @builder.int_to_int(visit(value), visit(to), signed: to.signed?) if from.integer? && to.integer?

      # Float -> Integer
      return @builder.float_to_int(visit(value), visit(to), signed: to.signed?) if from.float? && to.integer?

      # Integer -> Float
      return @builder.int_to_float(visit(value), visit(to)) if from.integer? && to.float?

      raise "Unsupported type for cast: attempted to cast #{from.name} to #{to.name}"
    end

    # Generates the LLVM IR for a function's content.
    #
    # @param expression [Expression] The function whose content is being generated.
    # @param *args      [Array<LLVM::Value>] The arguments passed to the function.
    #
    # @return [void]
    def generate_function_content(expression, *args)
      wrap_parameters(expression.parameters, args)

      expression.block.expressions.each { |expr| visit(expr) }
    end

    # Sets the parameter names for the given function.
    #
    # @param func         [LLVM::Function]    The function whose parameters are being defined.
    # @param parameters   [Array<Parameter>]  The parameters of the function.
    #
    # @return [LLVM::Function] The function with its parameters defined.
    def define_parameter_names(func, parameters)
      parameters.each_index do |index|
        func.params[index].name = parameters[index].name
      end

      func
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

    def register_malloc
      @builder.declare_function('malloc', [LLVM::Int64], LLVM::Int8.pointer)
    end

    def register_realloc
      @builder.declare_function('realloc', [LLVM::Int8.pointer, LLVM::Int64], LLVM::Int8.pointer)
    end

    def register_free
      @builder.declare_function('free', [LLVM::Int8.pointer], LLVM.Void)
    end

    def register_printf
      @builder.declare_function('printf', [LLVM::Int8.pointer], LLVM::Int32, varargs: true)
    end
  end
end
