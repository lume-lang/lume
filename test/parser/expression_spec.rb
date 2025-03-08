# frozen_string_literal: true

require_relative 'test_helper'

include Nox::Language

describe Parser do
  it_parses 'a = 1', Assignment.new('a'.var, 1.int8)
  it_parses 'a = nil', Assignment.new('a'.var, NilLiteral.new)
  it_parses 'a = b = 1', Assignment.new('a'.var, Assignment.new('b'.var, 1.int8))
  it_parses "a = 1\nb = 2", Assignment.new('a'.var, 1.int8), Assignment.new('b'.var, 2.int8)

  it_parses 'let a: int = 1', VariableDeclaration.new('a', Scalar.new('int'), 1.int8, const: false)
  it_parses 'const a: int = 1', VariableDeclaration.new('a', Scalar.new('int'), 1.int8, const: true)

  it_parses 'a = b.c', Assignment.new('a'.var, MemberAccess.new('b', 'c'))
  it_parses 'a = b.c.d', Assignment.new('a'.var, MemberAccess.new(MemberAccess.new('b', 'c'), 'd'))
  it_parses 'a = b.c.d()', Assignment.new('a'.var, Call.new(MemberAccess.new('b', 'c'), 'd'))
  it_parses 'a = b.c.d(1)', Assignment.new('a'.var, Call.new(MemberAccess.new('b', 'c'), 'd', 1.int8))

  it_parses 'def foo(): void end', MethodDefinition.new('foo', [], Void.new, [])
  it_parses 'def foo(a: int): void end', MethodDefinition.new('foo', [Parameter.new('a', Scalar.new('int'))], Void.new, [])
end
