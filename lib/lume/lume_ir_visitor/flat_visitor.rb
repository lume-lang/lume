# frozen_string_literal: true

require 'lume/lume_ir/ir'
require 'lume/lume_ir_visitor/visitor'

module Lume
  module IR
    # The flat visitor is used to visit the AST and flatten it into a single array,
    # without disrupting the original AST or its referential integrity.
    #
    # The search of the visitor is depth-first.
    class FlatVisitor
      include Lume::IR::Visitor

      def initialize
        @nodes = []
      end

      # Flattens the given AST into a single array, without altering it.
      #
      # @param ast [Lume::IR::AST]    The AST to flatten.
      #
      # @return [Array<Lume::IR::Node>]    The flattened AST.
      def self.flatten(ast)
        instance = FlatVisitor.new
        instance.flatten(ast)
      end

      # Flattens the AST into a single array.
      #
      # @param ast [Lume::IR::AST, Lume::IR::Node]    The AST to flatten.
      #
      # @return [Array<Lume::IR::Node>]    The flattened AST.
      def flatten(ast)
        accept_ast(ast) if ast.is_a?(Lume::IR::AST)
        accept(ast) if ast.is_a?(Lume::IR::Node)

        @nodes
      end

      private

      # Accepts any type of node and adds it to the nodes array.
      #
      # @param node [Lume::IR::Node]    The node to accept.
      def accept_node(node)
        @nodes << node
      end
    end
  end
end
