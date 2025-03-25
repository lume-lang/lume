# frozen_string_literal: true

module Lume
  class Analyzer
    module Pass
      # Reports warnings about unused symbols.
      class ReportUnusedSymbol
        def initialize(logger)
          @logger = logger
        end

        # Performs the analysis pass on the given modules.
        #
        # @param modules [Array<Lume::Module>] The modules to analyze.
        def call(modules)
          modules.each { |mod| visit_module(mod) }
        end

        private

        # Visits a module and reports any unused symbols.
        #
        # @param mod [Lume::Module] The module to visit.
        def visit_module(mod)
          flat_ast = Lume::MIR::FlatVisitor.flatten(mod.mir)

          report_unused_variables(flat_ast)
          report_unused_functions(flat_ast)
        end

        # Reports unused variables in the given AST.
        #
        # @param ast [Array<Lume::MIR::Node>] The flattened AST to analyze.
        def report_unused_variables(ast)
          declarations = ast.select { |node| node.instance_of?(Lume::MIR::VariableDeclaration) }
          references = ast.select { |node| node.instance_of?(Lume::MIR::Variable) }

          declarations.each do |decl|
            unless references.any? { |ref| ref.reference == decl }
              @logger.report(Lume::Analyzer::Errors::UnusedSymbol.new(decl))
            end
          end
        end

        # Reports unused functions in the given AST.
        #
        # @param ast [Array<Lume::MIR::Node>] The flattened AST to analyze.
        def report_unused_functions(ast)
          declarations = ast.select { |node| node.instance_of?(Lume::MIR::FunctionDefinition) }
          references = ast.select { |node| node.instance_of?(Lume::MIR::FunctionCall) }

          declarations.each do |decl|
            # The `main` function is always used.
            next if decl.name == 'main'

            unless references.any? { |ref| ref.reference == decl }
              @logger.report(Lume::Analyzer::Errors::UnusedSymbol.new(decl))
            end
          end
        end
      end
    end
  end
end
