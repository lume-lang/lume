# frozen_string_literal: true

module Lume
  class Analyzer
    module Errors
      # Defines that a `break` statement is used outside of a loop.
      #
      #   break  # This will raise a BreakOutsideLoop error
      class BreakOutsideLoop < Lume::LumeDiagnostic
        attr_reader :location

        def initialize(node)
          message = 'Break statement used outside of a loop.'

          super(message, type: Lume::ERROR)

          attach_location(node.location)
        end
      end
    end
  end
end
