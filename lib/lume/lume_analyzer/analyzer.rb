# frozen_string_literal: true

require 'lume/lume_mir_visitor/visitor'
require 'lume/lume_lowering/generator'
require 'lume/lume_analyzer/main_visitor'
require 'lume/lume_analyzer/passes'
require 'lume/lume_typing/typing'

module Lume
  class Analyzer # :nodoc:
    attr_reader :pass

    # Creates a new analyzer with the given AST.
    #
    # @param ast    [Lume::MIR::AST]     The AST to analyze.
    # @param logger [Lume::ErrorPrinter] The error printer to use.
    #
    # @return [Analyzer]
    def initialize(ast, logger: nil)
      @ast = ast
      @logger = logger

      # Defines the default passes to be executed during analysis.
      @pass = Passes.new
      @pass.use(:expression_analysis)
      @pass.use(:type_checking)
    end

    # Creates a new analyzer with the given AST.
    #
    # @param tree [Lume::MIR::AST] The AST to analyze.
    #
    # @return [Analyzer]
    def self.with_tree(tree)
      Analyzer.new(tree)
    end

    # Performs the analysis on all the nodes added to the analyzer.
    #
    # @return [Lume::MIR::AST]
    def analyze!
      raise ArgumentError, 'No nodes to analyze' if @ast.nodes.empty?

      # Lower the HIR AST into MIR.
      mir = lower_to_mir(@ast)

      # Invoke all the passes registered.
      @pass.passes.each { |pass_name| invoke_pass(pass_name, mir) }

      mir
    end

    private

    # Lowers in the HIR (High-Level Intermediate Representation) into MIR (Middle-Level Intermediate Representation),
    # which can be consumed by the analyzer, compiler and type checker.
    #
    # @param hir [Lume::Syntax::AST] The HIR AST to lower into MIR.
    #
    # @return [Lume::MIR::AST] The MIR AST.
    def lower_to_mir(hir)
      generator = Lume::Lowering::Generator.new
      generator.generate(hir)
    end

    # Performs expression analysis on the given MIR AST.
    #
    # @param mir [Lume::MIR::AST] The MIR AST to analyze.
    def expression_analysis(mir)
      # The main visitor is responsible for expanding expression result types, so that the type checker
      # can verify that all type constraints are satisfied.
      visit_main(mir)
    end

    # Performs type-checking analysis on the given MIR AST.
    #
    # @param mir [Lume::MIR::AST] The MIR AST to analyze.
    def type_checking(mir)
      type_checker = Lume::Typing::TypeChecker.new
      errors = type_checker.check(mir)

      # If any errors arose during type checking, report them to the user.
      report_errors(errors) if errors.any?
    end

    # Invokes the pass with the given name.
    #
    # @param name [Symbol] The name of the pass to invoke.
    # @param mir [Lume::MIR::AST] The MIR AST to analyze.
    def invoke_pass(name, mir)
      # If the method doesn't exist, emit a warning.
      return invalid_pass(name) unless respond_to?(name, true)

      # Otherwise, invoke the pass with the current MIR AST.
      method(name).call(mir)
    end

    # Reports a warning about an invalid pass.
    #
    # @param pass_name [String] The name of the invalid pass.
    def invalid_pass(pass_name)
      diagnostic = Lume::LumeDiagnostic.new("Invalid pass defined in analyzer: #{pass_name}", type: Lume::WARNING)

      @logger.report(diagnostic)
    end

    # Reports all the compilation errors within the given array.
    #
    # @param errors [Array<Lume::LumeDiagnostic>] The errors to report.
    def report_errors(errors)
      errors.each { |error| @logger.report(error) }
    end
  end
end
