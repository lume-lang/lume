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
  end
end
