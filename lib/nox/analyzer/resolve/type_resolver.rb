# frozen_string_literal: true

module Nox
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
      #   a = some_value()
      #
      # We can then infer, since `some_value` returns an `int`, that `a` is also an `int`.
      class TypeResolver < Pass
        # Resolves resulting type of the given variable declaration node.
        #
        # @param node [Nox::Language::VariableDeclaration] The variable declaration node to resolve.
        def accept_variable_reference(node)
          return unless node.expression_type.nil?

          declaration = retrieve_var(node.name)
          raise "Variable '#{node.name}' not found" if declaration.nil?

          node.expression_type = declaration.type
        end

        # Resolves resulting type of the given variable declaration node.
        #
        # @param node [Nox::Language::VariableDeclaration] The variable declaration node to resolve.
        def accept_variable_declaration(node)
          define_var(node.name, node)
        end
      end
    end
  end
end
