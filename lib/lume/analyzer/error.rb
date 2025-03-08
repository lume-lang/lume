# frozen_string_literal: true

module Lume
  class Analyzer
    # Base class for all anaylizer errors.
    class Error < StandardError
      attr_reader :code, :message, :previous, :location, :appendix

      def initialize(code, message, previous: nil)
        super(message)

        @code = code
        @message = message
        @previous = previous

        @location = nil
        @appendix = []
      end

      # Attaches a location to the error, so the error can be pinpointed to a specific line in a source file.
      #
      # @param location [Lume::Language::Location] The location to attach.
      def attach_location!(location)
        @location = location
      end

      # Appends a new note onto the error, which can be used to provide
      # additional context or information about the error.
      #
      # @param note [String] The note to add.
      def add_note!(note)
        @appendix << { type: :note, content: note }
      end

      # Appends a new help message onto the error, which can help the user understand the error.
      #
      # @param help [String] The help message to add.
      def add_help!(help)
        @appendix << { type: :help, content: help }
      end
    end
  end
end

require_relative 'errors/value_out_of_range'
