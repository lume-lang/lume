# frozen_string_literal: true

require_relative 'test_helper'

include Lume::Language

describe Parser do
  it_parses 'nil', NilLiteral.new

  it_parses 'true', BooleanLiteral.new(true)
  it_parses 'false', BooleanLiteral.new(false)

  it_parses '1', 1.int8
  it_parses '-1', -1.int8

  it_parses '1.0', 1.0.float32
  it_parses '-1.0', -1.0.float32
  it_parses '1.2345678', 1.2345678.float64
  it_parses '-1.2345678', -1.2345678.float64

  it_parses '1 + 2', Call.new(1.int8, '+', 2.int8)
  it_parses "1 +\n2", Call.new(1.int8, '+', 2.int8)
  it_parses "1 -\n2", Call.new(1.int8, '-', 2.int8)
  it_parses '1 +2', Call.new(1.int8, '+', 2.int8)
  it_parses '1 -2', Call.new(1.int8, '-', 2.int8)
  it_parses '1 + 2.0', Call.new(1.int8, '+', 2.0.float32)
  it_parses '1 * 2', Call.new(1.int8, '*', 2.int8)
  it_parses '1 * -2', Call.new(1.int8, '*', -2.int8)
  it_parses '1 * 2 * 3', Call.new(Call.new(1.int8, '*', 2.int8), '*', 3.int8)
  it_parses '1 / 2', Call.new(1.int8, '/', 2.int8)
  it_parses '1 / -2', Call.new(1.int8, '/', -2.int8)
  it_parses '2 * (2 + 4)', Call.new(2.int8, '*', Call.new(2.int8, '+', 4.int8))
  it_parses '2 * 3 + 4', Call.new(Call.new(2.int8, '*', 3.int8), '+', 4.int8)
  it_parses '2 + 3 * 4', Call.new(2.int8, '+', Call.new(3.int8, '*', 4.int8))
  it_parses 'f(1) + 3', Call.new(Call.new(nil, 'f', 1.int8), '+', 3.int8)
  it_parses '3 + f(1)', Call.new(3.int8, '+', Call.new(nil, 'f', 1.int8))

  it_parses 'a = 1', Assignment.new('a'.var, 1.int8)

  it_parses '""', StringLiteral.new('')
  it_parses '"Hello"', StringLiteral.new('Hello')
end
