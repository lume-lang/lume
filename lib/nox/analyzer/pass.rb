# frozen_string_literal: true

module Nox
  class Analyzer
    class Pass # :nodoc:
      def initialize
        @@variables = {}
        @@functions = {}
      end

      # Accepts an AST node and runs it through the pass.
      #
      # @param node [Nox::Language::Node] The AST node to be analyzed.
      def accept(node)
        node_class = node.class.name.underscore
        method_name = "accept_#{node_class}"

        # Resurse into all the child nodes
        node.accept_children(self) if node.respond_to?(:accept_children)

        method(method_name).call(node) if respond_to?(method_name)
      end

      def define_var(name, declaration)
        @@variables[name] = declaration
      end

      def retrieve_var(name)
        @@variables[name]
      end
    end
  end
end
