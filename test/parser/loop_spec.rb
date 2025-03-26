# frozen_string_literal: true

require_relative 'test_helper'

include Lume::Syntax

describe Lume::Parser do
  it_parses 'loop { }', Loop.new(
    []
  )

  it_parses 'loop { a = 1 }', Loop.new(
    [
      Assignment.new(
        'a'.var,
        1.int8
      )
    ]
  )

  it_parses 'loop { a = 1 b = 2 }', Loop.new(
    [
      Assignment.new(
        'a'.var,
        1.int8
      ),
      Assignment.new(
        'b'.var,
        2.int8
      )
    ]
  )
end
