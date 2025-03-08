# frozen_string_literal: true

module Lume
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
        # @param node [Lume::Language::VariableDeclaration] The scalar node to rename.
        def accept_variable_declaration(node)
          node.value = convert_literal_type(node.type, node.value)
        end

        VALUE_TYPE_MAP = {
          'String' => Lume::Language::StringLiteral,
          'Int8' => Lume::Language::ByteLiteral,
          'UInt8' => Lume::Language::UnsignedByteLiteral,
          'Int16' => Lume::Language::ShortLiteral,
          'UInt16' => Lume::Language::UnsignedShortLiteral,
          'Int32' => Lume::Language::WordLiteral,
          'UInt32' => Lume::Language::UnsignedWordLiteral,
          'Int64' => Lume::Language::LongLiteral,
          'UInt64' => Lume::Language::UnsignedLongLiteral,
          'Float' => Lume::Language::FloatLiteral,
          'Double' => Lume::Language::DoubleLiteral,
          'Boolean' => Lume::Language::BooleanLiteral
        }.freeze

        private

        # Converts the given literal from the existing type to the type defined in `value`.
        #
        # @param from [String] The existing type of the literal.
        # @param value [Lume::Language::Literal] The literal to convert.
        #
        # @return [Lume::Language::Literal] The converted literal.
        def convert_literal_type(from, value)
          # If the value isn't a literal, we cannot convert it to a literal.
          return value unless value.is_a?(Lume::Language::Literal)

          from = from.name if from.is_a?(Lume::Language::Scalar)
          value = value.value if value.is_a?(Lume::Language::Literal)

          raise "Invalid value type: #{from}" unless VALUE_TYPE_MAP.key?(from)

          node_class = VALUE_TYPE_MAP[from]

          unless value.is_a?(Lume::Language::Node) || node_class.can_contain?(value)
            raise Lume::Analyzer::Errors::ValueOutOfRange.new(from, value)
          end

          node_class.new(value)
        end
      end
    end
  end
end
