# frozen_string_literal: true

require_relative '../test_helper'
require 'lume/lume_syntax/hir_helpers'

module Minitest
  class Test
    def self.it_parses(source, *expected)
      it "parses #{source}" do
        parser = Lume::Parser.with_source(source)
        actual = parser.parse_module.nodes

        assert_equal(expected, actual)
      end
    end

    def self.it_cannot_parse(source)
      it "cannot parse #{source}" do
        assert_raises Lume::Parser::UnexpectedTokenError do
          parser = Lume::Parser.with_source(source)
          parser.parse_module
        end
      end
    end
  end
end
