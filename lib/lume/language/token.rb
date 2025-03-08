# frozen_string_literal: true

module Lume
  module Language
    class Token # :nodoc:
      attr_reader :type, :value
      attr_accessor :start, :end, :kind

      def initialize(type:, value: nil)
        @type = type
        @value = value
        @kind = nil

        @start = 0
        @end = 0
      end
    end
  end
end
