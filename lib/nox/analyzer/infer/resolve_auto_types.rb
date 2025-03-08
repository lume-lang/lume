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
      class ResolveAutoTypes < Pass
        # Renames the given scalar node, if it refers to an alias of a built-in type.
        #
        # @param node [Nox::Language::Scalar] The scalar node to rename.
        def accept_scalar(node)
          existing_type = node.name

          return unless TYPE_MAP.key?(existing_type)

          node.name = TYPE_MAP[existing_type]
        end

        TYPE_MAP = {
          'string' => 'String',
          'byte' => 'UInt8',
          'sbyte' => 'Int8',
          'short' => 'Int16',
          'ushort' => 'UInt16',
          'int' => 'Int32',
          'integer' => 'Int32',
          'long' => 'Int64',
          'ulong' => 'UInt64',
          'float' => 'Float',
          'double' => 'Double',
          'bool' => 'Boolean',
          'boolean' => 'Boolean'
        }.freeze
      end
    end
  end
end
