# frozen_string_literal: true

module Nox
  class Analyzer
    class Pass
      # Resolves all undefined types in the AST.
      #
      # Effectively, if a type is not defined, it will be inferred from the context. So, code such as:
      #
      #   i = true
      #
      # Will be rewritten as:
      #
      #   i: Boolean = true
      class CastScalarTypes < Pass
        # Renames the given scalar node, if it refers to an alias of a built-in type.
        #
        # @param node [Nox::Language::VariableDeclaration] The scalar node to rename.
        def accept_variable_declaration(node)
          node.value = convert_literal_type(node.type, node.value)
        end

        private

        VALUE_TYPE_MAP = {
          'String' => Nox::Language::StringLiteral,
          'Int8' => Nox::Language::ByteLiteral,
          'UInt8' => Nox::Language::UnsignedByteLiteral,
          'Int16' => Nox::Language::ShortLiteral,
          'UInt16' => Nox::Language::UnsignedShortLiteral,
          'Int32' => Nox::Language::WordLiteral,
          'UInt32' => Nox::Language::UnsignedWordLiteral,
          'Int64' => Nox::Language::LongLiteral,
          'UInt64' => Nox::Language::UnsignedLongLiteral,
          'Float' => Nox::Language::FloatLiteral,
          'Double' => Nox::Language::DoubleLiteral,
          'Boolean' => Nox::Language::BooleanLiteral
        }.freeze

        # Converts the given literal from the existing type to the type defined in `value`.
        #
        # @param from [String] The existing type of the literal.
        # @param value [Nox::Language::Literal] The literal to convert.
        #
        # @return [Nox::Language::Literal] The converted literal.
        def convert_literal_type(from, value)
          # If the value isn't a literal, we cannot convert it to a literal.
          return value unless value.is_a?(Nox::Language::Literal)

          from = from.name if from.is_a?(Nox::Language::Scalar)
          value = value.value if value.is_a?(Nox::Language::Literal)

          raise "Invalid value type: #{from}" unless VALUE_TYPE_MAP.key?(from)

          node_class = VALUE_TYPE_MAP[from]

          unless value.is_a?(Nox::Language::Node) || node_class.can_contain?(value)
            raise Nox::Analyzer::Errors::ValueOutOfRange.new(from, value)
          end

          node_class.new(value)
        end
      end
    end
  end
end
