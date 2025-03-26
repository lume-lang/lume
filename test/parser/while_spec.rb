# frozen_string_literal: true

require_relative 'test_helper'

include Lume::Syntax

describe Lume::Parser do
  it_parses 'while true { }', WhileLoop.new(
    BooleanLiteral.new(true),
    []
  )

  it_parses 'while a < 10 { }', WhileLoop.new(
    Call.new(
      'a'.var,
      '<',
      ByteLiteral.new(10)
    ),
    []
  )

  it_parses 'while a < 10 { a = a + 1 }', WhileLoop.new(
    Call.new(
      'a'.var,
      '<',
      ByteLiteral.new(10)
    ),
    [
      Assignment.new(
        'a'.var,
        Call.new(
          'a'.var,
          '+',
          ByteLiteral.new(1)
        )
      )
    ]
  )
end
