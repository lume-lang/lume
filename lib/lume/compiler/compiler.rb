# frozen_string_literal: true

module Lume
  class Compiler # :nodoc:
    include Lume::Codegen

    def initialize
      @nodes = []

      initialize_codegen!
    end

    # Adds nodes from the given AST to the compiler.
    #
    # @param tree [Array<Lume::Language::AST>]  AST containing all the nodes to add.
    def add_ast!(tree)
      @nodes.concat(tree.nodes)
    end

    # Compiles the expressions in the compiler into bytecode.
    def compile!
      @nodes.each { |node| codegen_node!(node) }
    end
  end
end
