# frozen_string_literal: true

module Lume
  module Language
    # Unexpected token errors are raised when the parser encounters a token that is not expected. These errors
    # contain information about the expected token, the actual token, and the location of the error.
    #
    # These errors are meant to be reported to the user and cause the parser to stop parsing the current file.
    class UnexpectedTokenError < Lume::LumeDiagnostic
      attr_reader :expected, :token, :location

      def initialize(message, expected, token, location)
        super(message, type: Lume::ERROR)

        @expected = expected
        @token = token
        @location = location
      end
    end

    # Unknown token errors are raised when the parser encounters a token that is not recognized. These errors
    # contain information about the actual token, and the location of the error.
    #
    # These errors are meant to be reported to the user and cause the parser to stop parsing the current file.
    class UnknownTokenError < Lume::LumeDiagnostic
      attr_reader :token, :location

      def initialize(message, token, location)
        message ||= "Could not recognize token, unknown token: #{token.value}"

        super(message, type: Lume::ERROR)

        @token = token
        @location = location
      end
    end
  end
end
