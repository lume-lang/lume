# frozen_string_literal: true

require_relative 'test_helper'

include Nox::Language

describe Parser do
  it_parses 'a = 1', Assignment.new('a'.var, 1.int8)
  it_parses 'a = nil', Assignment.new('a'.var, NilLiteral.new)
  it_parses 'a = b = 1', Assignment.new('a'.var, Assignment.new('b'.var, 1.int8))
  it_parses "a = 1\nb = 2", Assignment.new('a'.var, 1.int8), Assignment.new('b'.var, 2.int8)
end
