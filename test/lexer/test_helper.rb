# frozen_string_literal: true

require_relative '../test_helper'

module Minitest
  class Test
    def assert_tokens_equal(expected, actual)
      actual = actual.map { |token| [token.type, token.value] }

      assert_equal(expected, actual)
    end
  end
end
