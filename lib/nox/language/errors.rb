# frozen_string_literal: true

module Nox
  module Language
    class Error < StandardError # :nodoc:
      attr_accessor :message

      def initialize(message)
        super(message)

        @message = message
      end
    end

    class UnexpectedTokenError < Error # :nodoc:
      attr_reader :token, :location

      def initialize(expected, token, location, message = nil)
        @token = token
        @location = location

        message ||= "unexpected token: expected #{format_expected(expected)}, found '#{token.value}' (#{token.type})"
        message += " at line #{@location.line}, column #{@location.column}."
        message += " Found '#{token.value}'"

        super(message)
      end

      private

      def format_expected(expected)
        return "'#{expected.first}'" if expected.is_a?(Array) && expected.size == 1

        joined = expected.map(&:to_s).join(', ')

        "one of [#{joined}]"
      end
    end
  end
end
