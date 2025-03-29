# frozen_string_literal: true

require 'lume/lume_mir_visitor/visitor'
require 'lume/lume_lowering/generator'
require 'lume/lume_analyzer/main_visitor'
require 'lume/lume_typing/typing'

module Lume
  class Analyzer # :nodoc:
    # Defines a collection of analyzer passes, which will be executed in order.
    class Passes
      attr_reader :passes

      def initialize
        @passes = []
      end

      # Adds the new pass at the bottom of the pass collection.
      #
      # @param pass [String, Symbol] The name of the pass to add.
      #
      # @return [void]
      def use(pass)
        pass = pass.to_sym if pass.is_a?(String)

        @passes << pass
      end

      # Adds a new pass before the specified existing pass in the collection.
      #
      # @param existing [String, Symbol]  The name of the existing pass.
      # @param new [String, Symbol]       The name of the new pass to add.
      #
      # @return [void]
      def insert_before(existing, new)
        existing = existing.to_sym if existing.is_a?(String)

        index = @passes.index(existing)
        return if index.nil?

        new = new.to_sym if new.is_a?(String)
        @passes.insert(index, new)
      end

      # Adds a new pass after the specified existing pass in the collection.
      #
      # @param existing [String, Symbol]  The name of the existing pass.
      # @param new [String, Symbol]       The name of the new pass to add.
      #
      # @return [void]
      def insert_after(existing, new)
        existing = existing.to_sym if existing.is_a?(String)

        index = @passes.index(existing)
        return if index.nil?

        new = new.to_sym if new.is_a?(String)
        @passes.insert(index + 1, new)
      end
    end

    # Defines a pass for the analyzer, which accepts an array of modules and performs analysis on them.
    #
    # This type of pass is a base class for all analyzer passes. While it can be used by itself, it's recommended to
    # use one of the subsequent passes:
    #   - VisitorPass
    #   - FlatVisitorPass
    class AnalyzerPass
      include Lume::MIR

      def initialize(logger)
        @logger = logger
      end

      # Performs the analysis pass on the given modules.
      #
      # @param modules [Array<Lume::Module>] The modules to analyze.
      def call(modules)
        raise NotImplementedError, "Subclasses must implement `AnalyzerPass::call`. Missing on #{self.class}"
      end
    end

    # Defines a pass for the analyzer, which reports errors to the analyzer.
    #
    # This type of pass is not intended to be used for removing or changing nodes within the modules.
    class VisitorPass < AnalyzerPass
      include Lume::MIR::Visitor

      # Performs the analysis pass on the given modules.
      #
      # @param modules [Array<Lume::Module>] The modules to analyze.
      def call(modules)
        modules.each { |mod| accept_ast(mod.mir) }
      end
    end

    # Defines a pass for the analyzer, which accepts a flattened array of MIR nodes.
    class FlatVisitorPass < AnalyzerPass
      # Performs the analysis pass on the given modules.
      #
      # @param modules [Array<Lume::Module>] The modules to analyze.
      def call(modules)
        modules.each do |mod|
          flat_ast = Lume::MIR::FlatVisitor.flatten(mod.mir)

          visit(mod, flat_ast)
        end
      end

      private

      # Visits a module and all of it's MIR nodes, flattened to a single array.
      #
      # @param mod [Lume::Module] The module to visit.
      # @param nodes [Array<Lume::Node>] The flattened array of MIR nodes.
      def visit(mod, nodes)
        raise NotImplementedError, "Subclasses must implement `FlatVisitorPass::visit`. Missing on #{self.class}"
      end
    end
  end
end
