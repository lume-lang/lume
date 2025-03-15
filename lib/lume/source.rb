# frozen_string_literal: true

require 'colorize'

module Lume
  # The `SourceFile` class represents a single source within a compilation unit.
  class SourceFile
    attr_reader :path, :content

    def initialize(path, content)
      @path = path
      @content = content.to_sym
    end

    # Returns the content of the source file as a string.
    #
    # @return [String] The content of the source file.
    def to_s
      @content.to_s
    end

    # Returns the line at the given index.
    #
    # @param idx [Integer] The index of the line to return.
    #
    # @return [nil, String] The line at the given index, if any. Otherwise, `nil`.
    def line_at(idx)
      lines = to_s.each_line.with_index.select { |_, index| index == idx }

      return nil if lines.empty?

      lines[0][0]
    end

    # Returns the lines at the given indices.
    #
    # @param idxs [Range<Integer>, Array<Integer>] The indices of the lines to return.
    #
    # @return [Array<String>] The lines at the given indices.
    def lines_at(idxs)
      idxs = idxs.to_a if idxs.is_a?(Range)
      idxs = [idxs] if idxs.is_a?(Integer)

      lines = to_s.each_line.with_index.select { |_, index| idxs.include?(index) }

      lines.map { |line| line[0] }
    end

    def method_missing(name, *, &)
      to_s.send(name, *, &)
    end

    def respond_to_missing?(name, include_private = false)
      to_s.respond_to?(name, include_private)
    end
  end
end
