# frozen_string_literal: true

require_relative 'ir'
require_relative 'ir_gen'
require_relative 'visitor'
require_relative 'flat_visitor'
require_relative 'typing'

require_relative 'errors/instance_method_without_instance'
require_relative 'errors/static_method_with_instance'

module Lume
  class Analyzer # :nodoc:
    # Creates a new analyzer with the given AST.
    #
    # @param ast    [Lume::Analyzer::IR::AST] The AST to analyze.
    # @param logger [Lume::ErrorPrinter]      The error printer to use.
    #
    # @return [Analyzer]
    def initialize(ast, logger: nil)
      @ast = ast
      @logger = logger
    end

    # Creates a new analyzer with the given AST.
    #
    # @param tree [Lume::Analyzer::IR::AST] The AST to analyze.
    #
    # @return [Analyzer]
    def self.with_tree(tree)
      Analyzer.new(tree)
    end

    # Performs the analysis on all the nodes added to the analyzer.
    #
    # @return [Lume::Analyzer::IR::AST]
    def analyze!
      raise ArgumentError, 'No nodes to analyze' if @ast.nodes.empty?

      # Generate the compiler IR from the language AST.
      #
      # Compiler IR is more referential than the language AST. For example, instead of variable nodes
      # referencing a variable by name, they reference the variable declaration node itself.
      #
      # This is important for the analysis stage, since all nodes are expected to be altered
      # during the analysis process. If there are duplicate nodes within the tree which refer to the same
      # node, but is actually a different object, the analyzer would only analyze and update one of them, which
      # could lead to incorrect analysis results.
      irgen = IRGen.new
      ast = irgen.generate(@ast)

      # Pass the AST through the main visitor.
      #
      # The main visitor is responsible for expanding expression result types, so that the type checker
      # can verify that all type constraints are satisfied.
      visit_main(ast)

      # Run the AST through to the type checker to verify that all type constraints are satisfied.
      type_checker = TypeChecker.new
      errors = type_checker.check(ast)

      # If any errors arose during type checking, report them to the user.
      report_errors(errors) if errors.any?

      ast
    end

    private

    # Reports all the compilation errors within the given array.
    #
    # @param errors [Array<Lume::LumeError>] The errors to report.
    def report_errors(errors)
      errors.each { |error| @logger.report(error) }
    end
  end
end
