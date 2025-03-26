# frozen_string_literal: true

module Lume
  class Analyzer
    module Errors
      # Defines that a `continue` statement is used outside of a loop.
      #
      #   continue  # This will raise a ContinueOutsideLoop error
      class ContinueOutsideLoop < Lume::LumeDiagnostic
        attr_reader :location

        def initialize(node)
          message = 'Continue statement used outside of a loop.'

          super(message, type: Lume::ERROR)

          attach_location(node.location)
        end
      end
    end
  end
end
