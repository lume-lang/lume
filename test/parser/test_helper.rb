# frozen_string_literal: true

require_relative '../test_helper'

module Minitest
  class Test
    def self.it_parses(source, *expected)
      it "parses #{source}" do
        parser = Lume::Language::Parser.with_source(source)
        actual = parser.parse.nodes

        assert_equal(expected, actual)
      end
    end
  end
end
