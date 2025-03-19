# frozen_string_literal: true

require 'lume/lume_mir/mir'
require 'lume/lume_mir_visitor/visitor'

module Lume
  module MIR
    # The flat visitor is used to visit the AST and flatten it into a single array,
    # without disrupting the original AST or its referential integrity.
    #
    # The search of the visitor is depth-first.
    class FlatVisitor
      include Lume::MIR::Visitor

      def initialize
        @nodes = []
      end

      # Flattens the given AST into a single array, without altering it.
      #
      # @param ast [Lume::MIR::AST]    The AST to flatten.
      #
      # @return [Array<Lume::MIR::Node>]    The flattened AST.
      def self.flatten(ast)
        instance = FlatVisitor.new
        instance.flatten(ast)
      end

      # Flattens the AST into a single array.
      #
      # @param ast [Lume::MIR::AST, Lume::MIR::Node]    The AST to flatten.
      #
      # @return [Array<Lume::MIR::Node>]    The flattened AST.
      def flatten(ast)
        accept_ast(ast) if ast.is_a?(Lume::MIR::AST)
        accept(ast) if ast.is_a?(Lume::MIR::Node)

        @nodes
      end

      private

      # Accepts any type of node and adds it to the nodes array.
      #
      # @param node [Lume::MIR::Node]    The node to accept.
      def accept_node(node)
        @nodes << node
      end
    end
  end
end
