# frozen_string_literal: true

require_relative 'test_helper'

include Lume::Syntax

describe Lume::Parser do
  it_parses 'for i in a { }', IteratorLoop.new(
    'i',
    'a'.var,
    []
  )

  it_parses 'for i in a.iter() { }', IteratorLoop.new(
    'i',
    Call.new(
      'a',
      'iter'
    ),
    []
  )

  it_parses 'for i in a { sum = sum + i }', IteratorLoop.new(
    'i',
    'a'.var,
    [
      Assignment.new(
        'sum'.var,
        Call.new(
          'sum'.var,
          '+',
          'i'.var
        )
      )
    ]
  )
end
