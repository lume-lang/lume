# frozen_string_literal: true

module Lume
  class Analyzer
    class Pass
      # Resolves the resulting types of all expressions.
      #
      # For each expression, it infers the resulting type based on the context, such as:
      #
      #   def some_value(): int
      #     return 42
      #   end
      #
      #   let a = some_value()
      #
      # We can then infer, since `some_value` returns an `int`, that `a` is also an `int`.
      class TypeResolver < Pass
        include Lume::Language

        # Resolves resulting type of the given variable declaration node.
        #
        # @param node [Lume::Language::VariableDeclaration] The variable declaration node to resolve.
        def accept_variable_reference(node)
          return unless node.expression_type.nil?

          declaration = retrieve_var(node.name)
          raise "Variable '#{node.name}' not found" if declaration.nil?

          node.expression_type = declaration.type
        end

        # Resolves resulting type of the given variable declaration node.
        #
        # @param node [Lume::Language::VariableDeclaration] The variable declaration node to resolve.
        def accept_variable_declaration(node)
          define_var(node.name, node)

          return unless node.value.is_a?(New) || node.type.is_a?(Scalar)

          node.type = Scalar.new(node.value.class_name)
        end

        # Resolves resulting type of the given parameter node.
        #
        # @param node [Lume::Language::Parameter] The parameter node to resolve.
        def accept_parameter(node)
          define_var(node.name, node)
        end
      end
    end
  end
end
