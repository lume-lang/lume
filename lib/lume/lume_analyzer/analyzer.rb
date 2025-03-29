# frozen_string_literal: true

require 'lume/lume_mir_visitor/visitor'
require 'lume/lume_lowering/generator'
require 'lume/lume_analyzer/main_visitor'
require 'lume/lume_analyzer/passes'
require 'lume/lume_typing/typing'

Dir.glob("#{__dir__}/pass/*.rb").each { |file| require file }

module Lume
  class Analyzer # :nodoc:
    include Pass

    attr_reader :pass

    # Creates a new analyzer with the given modules.
    #
    # @param modules  [Array<Lume::Module>] The modules to analyze.
    # @param logger   [Lume::ErrorPrinter]          The error printer to use.
    #
    # @return [Analyzer]
    def initialize(modules, logger: nil)
      @modules = modules
      @logger = logger || NullReporter.new

      # Defines the default passes to be executed during analysis.
      @pass = Passes.new
      @pass.use(PerformExpressionAnalysis)
      @pass.use(TypeCheck)
      @pass.use(ReportUnusedSymbol)
      @pass.use(WarnUnreachableCode)
    end

    # Creates a new analyzer with the given parser modules.
    #
    # @param modules [Array<Lume::Module>] The modules to analyze.
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
    # @param modules [Array<Lume::Module>] The modules to lower.
    #
    # @return [void]
    def lower_to_mir(modules)
      generator = Lume::Lowering::Generator.new

      modules.each do |mod|
        mod.mir = generator.generate(mod)
      end
    end

    # Invokes the pass with the given name.
    #
    # @param name [Class] The class of the pass to invoke.
    def invoke_pass(name)
      pass_instance = name.new(@logger)

      # If the `call` method doesn't exist, emit a warning.
      return invalid_pass(name) unless pass_instance.respond_to?(:call, true)

      # Otherwise, invoke the pass with the current modules.
      pass_instance.call(@modules)
    end

    # Reports a warning about an invalid pass.
    #
    # @param pass_name [String] The name of the invalid pass.
    def invalid_pass(pass_name)
      diagnostic = Lume::LumeDiagnostic.new("Invalid pass defined in analyzer: #{pass_name}", type: Lume::WARNING)

      @logger.report(diagnostic)
    end
  end
end
