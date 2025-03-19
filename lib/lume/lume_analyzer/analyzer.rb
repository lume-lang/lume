# frozen_string_literal: true

require 'lume/lume_mir_visitor/visitor'
require 'lume/lume_lowering/generator'
require 'lume/lume_analyzer/main_visitor'
require 'lume/lume_analyzer/passes'
require 'lume/lume_typing/typing'

module Lume
  class Analyzer # :nodoc:
    attr_reader :pass

    # Creates a new analyzer with the given modules.
    #
    # @param modules  [Array<Lume::Parser::Module>] The modules to analyze.
    # @param logger   [Lume::ErrorPrinter]          The error printer to use.
    #
    # @return [Analyzer]
    def initialize(modules, logger: nil)
      @modules = modules
      @logger = logger

      # Defines the default passes to be executed during analysis.
      @pass = Passes.new
      @pass.use(:expression_analysis)
      @pass.use(:type_checking)
    end

    # Creates a new analyzer with the given parser modules.
    #
    # @param modules [Array<Lume::Parser::Module>] The modules to analyze.
    #
    # @return [Analyzer]
    def self.with_modules(modules)
      Analyzer.new(modules)
    end

    # Performs the analysis on all the modules added to the analyzer.
    #
    # @return [void]
    def analyze!
      raise ArgumentError, 'No modules to analyze' if @modules.empty?

      # For all modules, lower the HIR AST into MIR.
      lower_to_mir(@modules)

      # Invoke all the passes registered.
      @pass.passes.each { |pass_name| invoke_pass(pass_name) }
    end

    private

    # Lowers in the HIR (High-Level Intermediate Representation) into MIR (Middle-Level Intermediate Representation),
    # which can be consumed by the analyzer, compiler and type checker.
    #
    # @param modules [Array<Lume::Parser::Module>] The modules to lower.
    #
    # @return [void]
    def lower_to_mir(modules)
      generator = Lume::Lowering::Generator.new

      modules.each do |mod|
        mod.mir = generator.generate(mod.hir)
      end
    end

    # Performs expression analysis on the given modules.
    #
    # @param modules [Array<Lume::Parser::Module>] The modules to analyze.
    def expression_analysis(modules)
      # The main visitor is responsible for expanding expression result types, so that the type checker
      # can verify that all type constraints are satisfied.
      visit_main(modules)
    end

    # Performs type-checking analysis on the given MIR AST.
    #
    # @param modules [Array<Lume::Parser::Module>] The modules to type-check.
    def type_checking(modules)
      type_checker = Lume::Typing::TypeChecker.new
      errors = type_checker.check(modules)

      # If any errors arose during type checking, report them to the user.
      report_errors(errors) if errors.any?
    end

    # Invokes the pass with the given name.
    #
    # @param name [Symbol] The name of the pass to invoke.
    def invoke_pass(name)
      # If the method doesn't exist, emit a warning.
      return invalid_pass(name) unless respond_to?(name, true)

      # Otherwise, invoke the pass with the current modules.
      method(name).call(@modules)
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
