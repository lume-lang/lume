# frozen_string_literal: true

require_relative 'error'
require_relative 'pass'
require_relative 'infer/cast_scalar_types'
require_relative 'infer/resolve_auto_types'
require_relative 'resolve/type_resolver'

module Lume
  class Analyzer # :nodoc:
    def initialize(nodes = [])
      @passes = []
      @nodes = nodes
    end

    # Creates a new analyzer with the given AST.
    #
    # @param tree [Lume::Language::AST] The AST to analyze.
    #
    # @return [Analyzer]
    def self.with_tree(tree)
      Analyzer.new(tree.nodes)
    end

    # Returns the default passes for the analyzer.
    #
    # @return [Array<Pass>]
    def self.default_passes
      [
        Lume::Analyzer::Pass::ResolveAutoTypes,
        Lume::Analyzer::Pass::TypeResolver,
        Lume::Analyzer::Pass::CastScalarTypes
      ]
    end

    # Registers the given analyzer pass to the analyzer.
    #
    # @param pass [Pass|String] The pass to register.
    #
    # @return [void]
    def add_pass!(pass)
      pass = pass.new if pass.is_a?(Class)

      @passes << pass
    end

    # Registers the given analyzer passes to the analyzer.
    #
    # @param passes [Array<Pass|String>] The passes to register.
    #
    # @return [void]
    def add_passes!(passes)
      passes.each { |pass| add_pass!(pass) }
    end

    # Registers all the default passes for the analyzer.
    #
    # @return [void]
    def add_default_passes!
      add_passes!(Analyzer.default_passes)
    end

    # Performs the analysis on all the nodes added to the analyzer.
    #
    # **This process is destructive** and will modify the AST in place.
    #
    # @return [void]
    def analyze!
      raise ArgumentError, 'No nodes to analyze' if @nodes.empty?

      analyze_nodes!(@nodes)
    end

    private

    # Analyzes the given node.
    #
    # @param [Lume::Language::Node] node The node to analyze.
    def analyze_node!(node)
      @passes.each { |pass| pass.accept(node) }
    end

    # Analyzes all the nodes in the given array.
    #
    # @param [Array<Lume::Language::Node>] nodes The nodes to analyze.
    def analyze_nodes!(nodes)
      nodes.each { |node| analyze_node!(node) }
    end
  end
end
