# frozen_string_literal: true

module Nox
  class Analyzer
    module Errors
      # Defines that a value is out of range for a given type.
      #
      # This can be triggered when trying to assign a value, which is outside the allowed range,
      # most often when using numeric values.
      #
      #   a: UInt8 = 1000
      #
      # In the example above, the value 1000 is out of range for `UInt8`, which only allows values between 0 and 255.
      class ValueOutOfRange < Nox::Analyzer::Error
        CODE = 308

        def initialize(type, value)
          message = "Value #{value} is out of range for the given type #{type.class.name}."

          super(CODE, message)

          if type.is_a?(Nox::Language::IntegerLiteral)
            add_help!('Numeric types can only hold values within a specific range, depending on the type.')
          end
        end
      end
    end
  end
end
