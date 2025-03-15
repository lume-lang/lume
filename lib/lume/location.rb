# frozen_string_literal: true

module Lume
  # Locations represent some index-based range within a file, which is
  # used to track where a specific node or token is located within a source file.
  #
  # Locations don't use lines and columns - instead they character offsets within the file.
  class Location
    attr_accessor :span, :file

    def initialize(span, file: nil)
      @span = span
      @file = file
    end

    def ==(other)
      @span == other.span && @file == other.file
    end
  end
end
