# frozen_string_literal: true

require 'lume/lume_syntax/hir'
require 'lume/lume_syntax/token'
require 'lume/lume_lexer/errors'

module Lume
  class Lexer # :nodoc:
    include Lume::Syntax

    attr_reader :source

    def initialize(source)
      source = source.to_s if source.is_a?(Symbol)

      @source = source
      @index = 0
    end

    # Gets the next token from the source and returns it.
    #
    # This method alters the internal cursor position to the start of the next token.
    #
    # @return [Token] The next token from the source.
    def next_token!
      token = read_token!
      @index = token.end

      # If the lexer encounters an unknown token, raise an error.
      raise UnknownTokenError.new(nil, token, token.location(file: @source.path)) if token.unknown?

      token
    end

    # Gets all the tokens from the source and returns them.
    #
    # @param include_comments [Boolean] Whether to include comments in the tokens.
    #
    # @return [Array<Token>] The tokens parsed from the source.
    def all!(include_comments: true)
      tokens = []

      loop do
        token = next_token!
        next if token.type == :comment && !include_comments

        tokens << token
        break if token.type == :eof
      end

      tokens
    end

    WHITESPACE_TOKENS = [
      "\r",
      "\t",
      "\n",
      ' '
    ].freeze

    KEYWORDS = %i[
      let
      const
      if
      unless
      else
      fn
      class
      return
      true
      false
      null
      new
      public
      private
      static
      external
      enum
      import
    ].freeze

    SYMBOLS_PATTERN = %r(!=|!|==|=|->|\+=|-=|/=|\*=|,|\.|\+|-|/|\*|\(|\)|\{|\}|\[|\]|\||&|:|\+\+|--|>|<|>=|<=)

    private

    # Creates a new token with the given type and value.
    #
    # @param type   [Symbol]  The type of the token.
    # @param value  [String]  The value of the token.
    # @param start  [Integer] The starting index of the token in the source.
    # @param last   [Integer] The ending index of the token in the source.
    #
    # @return       [Token]   The created token.
    def token(type, value: nil, start: nil, last: nil)
      Token.new(type: type, value: value).tap do |token|
        last ||= @index + value.length unless value.nil?

        token.start = start || @index
        token.end = last || @index
      end
    end

    # Moves the internal cursor to the next token and parses it.
    #
    # If the source contains no more tokens, returns an `eof` token.
    #
    # @return [Token] The parsed token from the source.
    def read_token!
      return token(:eof) if @source.empty?

      move_to_next_word!

      return token(:eof) if @index >= @source.length

      parse_token
    end

    # Move the internal cursor to the next word.
    #
    # If the source contains no more words and/or tokens, goes to the end of the source.
    def move_to_next_word!
      position = @index
      pattern = next_word_pattern

      while position < @source.length
        break if @source.match?(pattern, position) && !WHITESPACE_TOKENS.include?(@source[position])

        position += 1
      end

      @index = position
    end

    # Determines the pattern for the next word based on the current index.
    #
    # @return [Regexp] The pattern for the next word.
    def next_word_pattern
      return /$/ if @index >= @source.length

      /\S/
    end

    # Parses the current token at the internal cursor.
    #
    # If the source contains no more tokens, returns an `unknown` token.
    #
    # @return [Token] The parsed token from the source.
    def parse_token
      first_character = @source[@index]
      raise StandardError, "Invalid token: #{first_character.to_i}" unless token?(first_character)

      token = parse_block_token
      return token unless token.type == :unknown

      token = parse_keyword_token
      return token unless token.type == :unknown

      token = parse_symbol_token
      return token unless token.type == :unknown

      token(:unknown, value: first_character, start: @index, last: @index + 1)
    end

    # Parses the current token at the internal cursor as a keyword.
    #
    # @return [Token] The parsed token from the source.
    def parse_keyword_token
      word = next_word
      return token(word.to_sym, value: word) if KEYWORDS.include?(word.to_sym)

      token(:unknown)
    end

    # Parses the current token at the internal cursor as a single symbol or glyph. Symbol tokens are
    # single-character symbols which have varying different meanings depending on their context.
    #
    # @return [Token] The parsed token from the source.
    def parse_symbol_token
      match = @source.match(SYMBOLS_PATTERN, @index)
      return token(:unknown) if match.nil?

      value = match.to_s

      token(value.to_sym, value: value)
    end

    # Parses the current token at the internal cursor as a block token. Block tokens can be either:
    # - Names of variables, functions, classes, etc.
    # - Numeric literals (either integers or floating-point numbers)
    # - Single-line comments starting with '#'
    # - Multi-line comments starting with '/*'
    # - String literals enclosed in single or double quotes
    #
    # @return [Token] The parsed token from the source.
    def parse_block_token
      first_character = @source[@index]

      return parse_name_token if first_character.match?(/[a-zA-Z_]/)
      return parse_number_token if first_character.match?(/[0-9]/)
      return parse_comment_token if first_character == '#'
      return parse_string_token if ["'", '"'].include?(first_character)

      token(:unknown)
    end

    # Parses the current token at the internal cursor as a string-word string literals.
    #
    # @return [Token] The parsed token from the source.
    def parse_name_token
      word = next_word

      # Forcefully ignore keywords and send them back up the chain
      return token(:unknown) if KEYWORDS.include?(word.to_sym)

      return token(:unknown) unless word.match?(/^[a-zA-Z_][a-zA-Z0-9_]*$/)

      token(:name, value: word, start: @index, last: @index + word.length)
    end

    # Parses the current token at the internal cursor as a numeric literal.
    #
    # @return [Token] The parsed token from the source.
    def parse_number_token
      slice, kind = parse_number_value
      return token(:unknown) if slice.nil?

      number = parse_integer_number_token(slice) || parse_float_number_token(slice)
      return token(:unknown) if number.nil?

      create_number_token(number, slice, kind)
    end

    # Parses the value at the current cursor position as a numeric literal.
    #
    # @return [String] The parsed number content from the source.
    def parse_number_value
      pattern = /[-+]?(?<value>(?:[\d_.]+(?:e-?\d+)?)|(?:0[dxob]\w+))(?:_(?<kind>\w+))?\b/
      match = @source.match(pattern, @index)

      return [nil, nil] if match.nil?

      [match[:value], match[:kind]]
    end

    # Creates a number token with the given value, source slice and optional number kind.
    #
    # @param value [String] The value of the number token.
    # @param slice [String] The source slice of the number token.
    # @param kind [String] The kind of the number token, if any.
    #
    # @return [Token] The created number token.
    def create_number_token(value, slice, kind)
      token = token(:number, value: value, start: @index, last: @index + slice.length)
      token.kind = kind if kind
      token.end = @index + slice.length + kind.length + 1 if kind

      token
    end

    # Parses the current token at the internal cursor as an integer literal.
    #
    # @param value [String] The value of the integer literal.
    #
    # @return [Token|nil] The parsed token from the source, if the literal is a valid integer. Otherwise, returns nil.
    def parse_integer_number_token(value)
      Integer(value)
    rescue ArgumentError
      nil
    end

    # Parses the current token at the internal cursor as an floating-point literal.
    #
    # @param value [String] The value of the floating-point literal.
    #
    # @return [Token|nil] The parsed token from the source, if the literal is a valid float. Otherwise, returns nil.
    def parse_float_number_token(value)
      Float(value)
    rescue ArgumentError
      nil
    end

    # Parses the current token at the internal cursor as a single-line comment.
    #
    # @return [Token] The parsed token from the source.
    def parse_comment_token
      end_of_line_index = @source.index("\n", @index) || @source.length
      comment_content = @source[@index + 1..end_of_line_index]

      token(:comment, value: comment_content.strip, start: @index, last: end_of_line_index + 1)
    end

    # Parses the current token at the internal cursor as a multi-line comment.
    #
    # @return [Token] The parsed token from the source.
    def parse_comment_block
      end_index = @source.index('*/', @index) || (@source.length - 1)
      comment_content = @source[@index + 2...end_index]

      token(:comment, value: comment_content.strip, start: @index, last: end_index + 3)
    end

    # Parses the current token at the internal cursor as a string literal, enclosed in single or double quotes.
    #
    # @return [Token] The parsed token from the source.
    def parse_string_token
      uses_single_quotes = @source[@index] == "'"
      quote_character = uses_single_quotes ? "'" : '"'

      index, slice = parse_string_content(@index + 1, quote: quote_character)

      token(:string, value: slice, start: @index, last: index)
    end

    # Parses the content of a string literal, enclosed in single or double quotes.
    #
    # @param from   [Integer]   The starting index to search from.
    # @param quote  [String]    The quote character to search for. Default is '"'.
    #
    # @return [Array<Integer, String>] The index of the closing quote character and the parsed string content.
    def parse_string_content(from, quote: '"')
      slice = ''
      index = from

      while index < @source.length
        if @source[index] == '\\' && index + 1 < @source.length
          slice += @source[index + 1]
          index += 2
        elsif @source[index] == quote
          index += 1
          break
        else
          slice += @source[index]
          index += 1
        end
      end

      [index, slice]
    end

    # Determines whether the given character is a valid token character.
    #
    # @param char [String] The character to check.
    #
    # @return [Boolean] True if the character is a valid token character, false otherwise.
    def token?(char)
      char.bytes.first >= 0x20 && char != '\t' && char != '\n'
    end

    # Returns the next word from the source at the current cursor position.
    #
    # @return [String] The parsed word.
    def next_word
      index = @source.index(/(\W|$)/, @index + 1) || (@source.length - 1)

      @source.slice(@index, index - @index)
    end
  end
end
