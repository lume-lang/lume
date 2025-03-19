# frozen_string_literal: true

require 'lume/lume_syntax/types'
require 'lume/lume_syntax/values'
require 'lume/lume_syntax/ast_helpers'
require 'lume/lume_parser/errors'

module Lume
  class Parser # :nodoc:
    # Parses a single type definition.
    #
    # @return [TypeDefinition] The parsed type definition.
    #
    # @see TypeDefinition
    def parse_type_definition
      case @token.type
      when :type then parse_type_alias_definition
      when :enum then parse_type_enum_definition
      end
    end

    # Parses a single type alias definition.
    #
    # @return [TypeDefinition] The parsed type definition.
    def parse_type_alias_definition
      consume!(value: :type)

      name = consume!(type: name)

      # Skip equal sign between name and type
      consume!(type: :'=')

      type = with_location { parse_type }

      TypeDefinition.new(name.value, type)
    end

    # Parses a single enum definition.
    #
    # @return [Enum] The parsed enum definition.
    def parse_type_enum_definition
      consume!(value: :enum)

      name = consume!(type: name).value

      consume_wrapped! do
        cases = iterate_all! do |index|
          # If the next token is a closing token, theres no cases defined
          next nil if peek(:'}')

          # If the next token is a comma, there's still more cases
          next nil if index.positive? && !consume(type: :',')

          parse_type_enum_case
        end

        Enum.new(name, cases)
      end
    end

    # Parses a single enum case.
    #
    # @return [EnumCase] The parsed enum case.
    def parse_type_enum_case
      name = consume!(type: name).value
      parameters = parse_parameters if peek(:'(')

      EnumCase.new(name, parameters || [])
    end

    # Parses a single type definition expression.
    #
    # @return [Type] The parsed type definition.
    def parse_type
      type = parse_type_prefix_expression

      # If the next token is a pipe, parse it as a union type.
      type = parse_union_type(type) if peek(:|)

      if type.is_a?(Union)
        # Merge all the nested unions within a single union
        type.merge_nested_unions

        # If there is only a single type in the union, return it directly
        return type.types.first if type.types.size == 1
      end

      type
    end

    # Parses a type prefix expression at the current cursor position.
    #
    # Prefix expressions are expressions which appear at the start of a type definition.
    #
    # @return [Type] The parsed type expression.
    def parse_type_prefix_expression
      return nil if peek(:eof)

      # If the next token is an opening parentheses, parse it as a nested type.
      return parse_union_type if peek(:'(')

      # If the next token is a opening bracket, parse it as an array type.
      return parse_array_type if peek(:'[')

      # If the next token is a name, parse it as a named type.
      return parse_named_type if peek(:name)

      # If the next token is an asterisk, parse it as a pointer type.
      return parse_pointer_type if peek(:*)

      nil
    end

    # Parses a nested type definition expression.
    #
    # @return [Type] The parsed type definition.
    def parse_nested_type
      consume_wrapped!(left: :'(', right: :')') { parse_type }
    end

    # Parses a named type definition expression.
    #
    # @return [Type] The parsed type definition.
    def parse_named_type
      name = consume!(type: :name).value

      return Void.new if name.casecmp?('void')

      return Null.new if name.casecmp?('null')

      NamedType.new(name)
    end

    # Parses a union type definition expression.
    #
    # @return [Type] The parsed type definition.
    def parse_union_type(lhs)
      consume(type: :|)

      Union.new([lhs, parse_type])
    end

    # Parses an array type definition expression.
    #
    # @return [ArrayType] The parsed type definition.
    def parse_array_type
      inner = consume_wrapped!(left: :'[', right: :']') { parse_type }

      ArrayType.new(inner)
    end

    # Parses a pointer type definition expression.
    #
    # @return [Type] The parsed type definition.
    def parse_pointer_type
      consume!(type: :*)

      Pointer.new(parse_type)
    end
  end
end
