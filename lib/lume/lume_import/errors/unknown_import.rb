# frozen_string_literal: true

module Lume
  class Importer
    module Errors
      # Defines that an import is unknown or cannot be found.
      class UndefinedImport < Lume::LumeDiagnostic
        attr_reader :location

        def initialize(node)
          message = "Library '#{node.library}' cannot be found or does not exist."

          super(message, type: Lume::ERROR)

          attach_location(node.location)
        end
      end
    end
  end
end
