# frozen_string_literal: true

module Lume
  class Analyzer
    module Errors
      # Defines that a symbol was referenced without being defined:
      #
      #   let a = b  # This will raise a UndefinedSymbol error
      class UndefinedSymbol < Lume::LumeDiagnostic
        attr_reader :location

        def initialize(node, name: nil)
          name ||= node.name
          message = "Symbol '#{name}' does not exist in the current scope."

          super(message, type: Lume::ERROR)

          attach_location(node.location)
        end
      end
    end
  end
end
