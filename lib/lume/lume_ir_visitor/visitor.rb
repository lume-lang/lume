# frozen_string_literal: true

module Lume
  module IR
    # Defines an abstract visitor for AST nodes.
    #
    # Classes implementing this module should define methods for each type of AST node, such as:
    #   - `accept_method_definition` for `Lume::IR::MethodDefinition`
    #   - `accept_scalar` for `Lume::IR::Scalar`
    #   - and so on.
    #
    # Child nodes are walked recursively, if the node defines a method named `accept_children`.
    # When child nodes can be walked, they are visited before the node itself it visited - functioning
    # as a depth-first traversal.
    module Visitor
      # Accepts an AST and runs all the root nodes through the visitor.
      #
      # @param ast [Lume::IR::AST] The AST to be analyzed.
      def accept_ast(ast)
        ast.nodes.each { |node| accept(node) }
      end

      # Accepts an AST node and runs it through the visitor.
      #
      # @param node [Lume::IR::Node] The AST node to be analyzed.
      def accept(node)
        # If the node has already been visited, skip it.
        return if visited?(node)

        # Mark the node as visited.
        #
        # The value of the entry is irrelevant, since it's existence within the hash is
        # sufficient to track visitation.
        @visited[node] = true

        # Resurse into all the child nodes
        node.accept_children(self) if node.respond_to?(:accept_children, true)

        current = node.class

        until current.nil?
          node_class = current.name.underscore
          method_name = "accept_#{node_class}"

          # Visit the node itself
          method(method_name).call(node) if respond_to?(method_name, true)

          current = current.superclass
        end
      end

      private

      # Determines whether the given node has already been visited.
      #
      # @param node [Lume::IR::Node] The AST node to be checked.
      #
      # @return [Boolean] Whether the node has been visited.
      def visited?(node)
        # Ensure that visitation map exists
        @visited ||= {}

        # Ensure that identity comparison is used
        @visited.compare_by_identity unless @visited.compare_by_identity?

        @visited.key?(node)
      end
    end
  end
end
