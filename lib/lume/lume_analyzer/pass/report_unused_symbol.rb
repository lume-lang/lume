# frozen_string_literal: true

module Lume
  class Analyzer
    module Pass
      # Reports warnings about unused symbols.
      class ReportUnusedSymbol < FlatVisitorPass
        private

        # Visits a module and reports any unused symbols.
        #
        # @param _mod    [Lume::Module] The module to visit.
        # @param nodes  [Array<Lume::Node>] The flattened array of MIR nodes.
        def visit(_mod, nodes)
          report_unused_variables(nodes)
          report_unused_functions(nodes)
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
