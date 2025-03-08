# frozen_string_literal: true

module Nox
  module Language
    class Token # :nodoc:
      attr_reader :type, :value
      attr_accessor :start, :end

      def initialize(type:, value: nil)
        @type = type
        @value = value

        @start = 0
        @end = 0
      end
    end
  end
end
