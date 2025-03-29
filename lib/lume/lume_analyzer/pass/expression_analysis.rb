# frozen_string_literal: true

module Lume
  class Analyzer
    module Pass
      # Performs expression analysis on the given modules.
      class PerformExpressionAnalysis < AnalyzerPass
        # Passes the modules through the main visitor, which handles the expansion of result
        # types of expressions within it.
        #
        # The main visitor is responsible for expanding expression result types, so that the type checker
        # can verify that all type constraints are satisfied.
        #
        # @param modules [Array<Lume::Module>]  The modules to be analyzed.
        def call(modules)
          visitor = MainVisitor.new

          # rubocop:disable Style/CombinableLoops -- We must prediscover *all* modules before attempting to visit them.

          # Pre-discovery class- and function-definition in all modules before attempting to analyze them.
          modules.each { |mod| visitor.prediscover(mod.mir) }

          # All modules need to share the same visitor, as they need to refer to the same symbol table.
          modules.each { |mod| visitor.visit(mod.mir) }

          # rubocop:enable Style/CombinableLoops
        end
      end
    end
  end
end
