# frozen_string_literal: true

module Lume
  class Analyzer
    module Errors
      # Defines that a symbol was defined but not used:
      #
      #   let a = 1 # This will raise an UnusedSymbol warning
      #
      #   return 0
      class UnusedSymbol < Lume::LumeDiagnostic
        attr_reader :location

        def initialize(node, name: nil)
          name ||= node.name
          message = "Symbol '#{name}' was defined but not used."

          super(message, type: Lume::WARNING)

          attach_location(node.location)
        end
      end
    end
  end
end
