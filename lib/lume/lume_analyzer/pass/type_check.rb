# frozen_string_literal: true

module Lume
  class Analyzer
    module Pass
      # Performs type-checking analysis on the given MIR AST.
      class TypeCheck < AnalyzerPass
        # Performs type-checking analysis on the given MIR AST.
        #
        # @param modules [Array<Lume::Module>] The modules to type-check.
        def call(modules)
          type_checker = Lume::Typing::TypeChecker.new
          errors = type_checker.check(modules)

          # If any errors arose during type checking, report them to the user.
          errors.each { |error| @logger.report(error) }
        end
      end
    end
  end
end
