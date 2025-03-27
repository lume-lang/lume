# frozen_string_literal: true

module Lume
  class Analyzer
    module Errors
      # Defines that one-or-more statements are unreachable.
      #
      #   return 0
      #
      #   let a = 1 # This will raise an UnreachableCode warning
      class UnreachableCode < Lume::LumeDiagnostic
        attr_reader :location

        def initialize(node)
          message = 'Code is unreachable.'

          super(message, type: Lume::WARNING)

          attach_location(node.location)
        end
      end
    end
  end
end
