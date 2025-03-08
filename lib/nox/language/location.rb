# frozen_string_literal: true

module Nox
  module Language
    class Location # :nodoc:
      attr_accessor :line, :column, :file

      def initialize(line: 0, column: 0, file: '<memory>')
        @line = line
        @column = column
        @file = file
      end
    end
  end
end
