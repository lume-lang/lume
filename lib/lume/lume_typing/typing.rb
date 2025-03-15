# frozen_string_literal: true

require 'lume/errors'
require 'lume/lume_ir_visitor/visitor'
require 'lume/lume_typing/errors'
require 'lume/lume_typing/constraints'

module Lume
  module Typing
    # The type checker is responsible for visiting all the expressions within an abstract syntax tree (AST)
    # and verifying that every expression is valid, in the context of it's resulting type.
    class TypeChecker
      include Lume::IR
      include Lume::IR::Visitor
      include Lume::Typing::Errors

      def initialize
        @errors = []
        @type_definitions = {}
      end

      # Checks the given abstract syntax tree (AST) for type errors.
      #
      # If any correctable errors are found, they are corrected. Otherwise, they are reported as errors and returned.
      #
      # @param ast [AST] The abstract syntax tree to type check.
      #
      # @return [Array<TypeError>] An array of errors found during type checking, if any.
      def check(ast)
        # Ensure that no errors are present from last type check.
        @errors = []

        # Pre-discover all the type definitions from the AST for later use.
        discover_type_definitions(ast)

        # Run the AST through the type checker.
        accept_ast(ast)

        @errors
      end

      # Determines whether the given type name is an alias.
      #
      # @param type [String] The type to check.
      #
      # @return [Boolean] `true` if the type is an alias, `false` otherwise.
      def alias?(type)
        return false unless type.is_a?(Scalar)

        !@type_definitions[type.name].nil?
      end

      # Gets the concrete type definition for the given type alias.
      #
      # @param type [String] The type to get the concrete type definition for.
      #
      # @return [TypeDefinition, nil] The concrete type definition if it is an alias, `nil` otherwise.
      def find_alias(type)
        return false unless type.is_a?(Scalar)

        @type_definitions[type.name]
      end

      private

      # Discovers all the declared type definitions in the given AST.
      #
      # @param ast [AST] The abstract syntax tree to discover type definitions in.
      #
      # @return [Array<TypeDefinition>] An array of type definitions found in the AST.
      def discover_type_definitions(ast)
        # Ensure that no type definitions are present from last type check.
        @type_definitions = {}

        ast.find_all(TypeDefinition).each do |definition|
          @type_definitions[definition.name] = definition
        end
      end

      # Solves a constraint against the expected type and the actual type given.
      #
      # If the constraint fails, but a cast is suggested, the cast is applied to the `actual` expression.
      #
      # @param expected [Type] The expected type of the expression.
      # @param actual   [Expression] The actual expression.
      # @param location [Location] An optional location of the expression.
      #
      # @return [Boolean]
      #   `true` if the constraint is satisfied.
      #   `false` if the constraint failed and no cast was suggested.
      def solve(expected, actual, location: nil)
        result = attempt_solve(expected, actual)

        # If the constraint returned true, return happily.
        return true unless result.failure?

        # Otherwise, the result is an array of errors.
        #
        # Associate the location of all the errors, if any is given.
        result.errors.each { |error| error.location = location } unless location.nil?

        @errors.concat(result.errors)

        false
      end

      # Expands the given type alias out to the concrete type.
      #
      # @param type [Type] The type to expand.
      #
      # @return [Type] The expanded type. If the type is not an alias, it returns the type itself.
      def expand_type_alias(type)
        # If the type isn't an alias, return it as-is.
        return type unless alias?(type)

        # Otherwise, recursively expand the type alias.
        expand_type_alias(find_alias(type).type)
      end

      # Attempts to solve a constraint against the expected type and the actual type given.
      #
      # If the constraint fails, but a cast is suggested, the cast is applied to the `actual` expression.
      #
      # @param expected [Type] The expected type of the expression.
      # @param actual   [Expression] The actual expression.
      #
      # @return [ConstraintResult]
      def attempt_solve(expected, actual)
        expanded_actual = expand_type_alias(actual.expression_type)

        constraint = construct_constraint(expected)
        return ConstraintResult.failure if constraint.nil?

        result = constraint.solve(expanded_actual)

        # If the constraint suggests a cast, handle it separately.
        return ConstraintResult.success if result.suggestion? && solve_suggested_cast(actual, result.suggestion)

        # If the constraint is satisfied, return true.
        return ConstraintResult.success if result.success?

        # If the constraint failed, return a list of the errors reported.
        errors = constraint.report(actual.expression_type)

        ConstraintResult.failure(errors)
      end

      # Solves a constraint by casting the expression to a type of the given value.
      #
      # @param actual     [Expression] The actual type of the expression.
      # @param suggestion [Type] The suggested type to cast to.
      #
      # @return [Boolean]
      def solve_suggested_cast(actual, suggestion)
        # If the expression doesn't support casting, we have to exit out.
        return false unless actual.respond_to?(:cast_to)

        # Otherwise, we can cast the expression to the suggested type.
        actual.cast_to(suggestion)

        # Update the expression type to the suggested type.
        actual.expression_type = suggestion.expression_type

        true
      end

      # Constructs a constraint, which represents the expected type of an expression.
      #
      # @param expected [Type] The expected type of the expression.
      #
      # @return [Constraint] The constructed constraint.
      def construct_constraint(expected)
        # Expand any type aliases, if the type is aliased.
        expected = expand_type_alias(expected.expression_type)

        constraint = Constraint.construct_for(self, expected)
        return constraint unless constraint.nil?

        raise_error("Unsupported constraint type: #{expected.class}", location: expected.location)

        nil
      end

      # Raises a new typing error with the given message.
      #
      # @param message  [Lume::LumeDiagnostic, String] The error message.
      # @param location [Location] The location of the error.
      def raise_error(message, location: nil)
        if message.is_a?(String)
          @errors << TypeError.new(message, location: location)
        elsif message.is_a?(Lume::LumeDiagnostic)
          @errors << message
        end
      end

      # Checks that the arguments passed to a function call satisfy the expected types.
      #
      # @param node [FunctionCall]
      def accept_function_call(node)
        arguments = node.arguments
        parameters = node.reference.parameters
        parameter_count = node.reference.parameter_count

        # Ensure that the number of arguments is allowed for the given function definition.
        # If it's not within the expected range, raise an error.
        unless parameter_count.include?(arguments.length)
          parameter_count = parameter_count.min if parameter_count.min == parameter_count.max

          raise_error("Expected #{parameter_count} arguments, but got #{arguments.length}", location: node.location)
        end

        # Solve the types of the arguments against their expected parameter types.
        solve_argument_list(arguments, parameters)
      end

      # Checks that the arguments passed to a method call satisfy the expected types.
      #
      # @param node [MethodCall]
      def accept_method_call(node)
        # Raise an error if the method is static and an instance is provided.
        raise_error(StaticMethodWithInstance.new(node)) if node.reference.static? && !node.static?

        # Conversely, raise an error if the method is not static and an instance is not provided.
        raise_error(InstanceMethodWithoutInstance.new(node)) if !node.reference.static? && node.static?

        accept_function_call(node)
      end

      # Checks that the values returned within a function definition satisfy it's return type.
      #
      # @param node [FunctionDefinition]
      def accept_function_definition(node)
        return_statements = node.find_all(Return)

        return_statements.each do |statement|
          solve(node.return.expression_type, statement, location: statement.location)
        end
      end

      # Checks that the value of a variable declaration satisfies the type of the variable.
      #
      # @param node [VariableDeclaration]
      def accept_variable_declaration(node)
        solve(node.type, node.value) unless node.value.nil?
      end

      # Solves the types of the arguments against their expected parameter types.
      #
      # @param arguments [Array<Expression>]
      # @param parameters [Array<Parameter>]
      def solve_argument_list(arguments, parameters)
        arguments.each_with_index do |argument, index|
          parameter = parameters[index]

          solve(parameter.type, argument, location: argument.value.location)
        end
      end
    end
  end
end
