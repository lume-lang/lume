# frozen_string_literal: true

require 'lume/location'

module Lume
  module Syntax
    # The humble token. The smallest unit of the Lume language and essential building block for everything past it.
    #
    # Tokens are produced by the lexer, which breaks down the input source code into individual tokens. They represent
    # a specific identifier, value or operator within the programming language. Tokens don't have any intrinsic meaning,
    # and function only as a stepping stone for the parser to understand the structure of the code.
    #
    # Tokens are very similar to letters in traditional languages. While letters don't contain any semantic value
    # themselves, they are essential to the structure and meaning of words and sentences. Similarly, tokens are
    # the building blocks of the Lume language, providing the foundation for more complex structures and expressions.
    #
    # @see [Lexer]
    # @see [Parser]
    class Token
      attr_reader :type, :value
      attr_accessor :start, :end, :kind

      def initialize(type:, value: nil)
        @type = type
        @value = value
        @kind = nil

        @start = 0
        @end = 0
      end

      # Determines whether the token is unknown.
      #
      # @return [Boolean] `true` if the token is unknown, `false` otherwise.
      def unknown?
        type == :unknown
      end

      # Gets a location object representing the token's position in the source code.
      #
      # @param file [SourceFile, nil] The source file (optional).
      #
      # @return [Lume::Location] A location object representing the token's position.
      def location(file: nil)
        Location.new(@start..@end, file: file)
      end
    end
  end
end
