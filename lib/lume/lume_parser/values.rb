# frozen_string_literal: true

require 'lume/lume_syntax/types'
require 'lume/lume_syntax/values'
require 'lume/lume_syntax/hir_helpers'
require 'lume/lume_parser/errors'

module Lume
  class Parser # :nodoc:
    # Parses zero-or-more values from an expression, separated by commas.
    #
    # @return [Array<Expression>] The parsed values.
    def parse_values
      iterate_all! do |index|
        next nil if peek(:'(')

        # We shouldn't break on the first iteration, as we haven't read any values yet.
        next nil if !peek(:',') && index.positive?

        # If the next token is a comma, consume it.
        consume(type: :',') if index.positive?

        parse_expression
      end
    end

    # Parses a single value from an expression.
    #
    # @return [Literal] The parsed value.
    def parse_value
      return parse_string_value if peek(:string)
      return parse_number_value if peek(:number)
      return parse_boolean_value if peek(%i[true false])
      return parse_null_value if peek(:null)

      unexpected_token(%i[string number true false null])
    end

    # Parses a single string value from an expression.
    #
    # @return [StringLiteral] The parsed string value.
    def parse_string_value
      value = consume!(type: :string).value

      StringLiteral.new(value)
    end

    # Parses a single number value from an expression.
    #
    # @return [NumberLiteral] The parsed number value.
    def parse_number_value
      token = consume!(type: :number)

      # If an explicit type was given in the literal, use that to create the value.
      return NUMERIC_TYPE_MAP[token.kind].new(token.value) if NUMERIC_TYPE_MAP.key?(token.kind)

      # If no explicit type was given, try to infer the type from the value.
      # We're looping through all numeric values to see if the value can be contained.
      # If it can be contained within a given type, create a new instance of that type and return it.
      NUMERIC_LITERAL_TYPES.each do |type|
        return type.new(token.value) if type.can_contain?(token.value)
      end

      raise "Number out of range (#{token.value})"
    end

    # Parses a single boolean value from an expression.
    #
    # @return [BooleanLiteral] The parsed boolean value.
    def parse_boolean_value
      value = consume!(type: %i[true false]).value
      value = value.is_a?(String) && value.casecmp?('true')

      BooleanLiteral.new(value)
    end

    # Parses a single `null` value from an expression.
    #
    # @return [NullLiteral] The parsed `null` value.
    def parse_null_value
      consume!(type: :null).value

      NullLiteral.new
    end
  end
end
