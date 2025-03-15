# frozen_string_literal: true

require_relative 'test_helper'

include Lume::Syntax

describe Lume::Parser do
  it_parses 'a = 1', Assignment.new('a'.var, 1.int8)
  it_parses 'a = nil', Assignment.new('a'.var, NilLiteral.new)
  it_parses 'a = b = 1', Assignment.new('a'.var, Assignment.new('b'.var, 1.int8))
  it_parses "a = 1\nb = 2", Assignment.new('a'.var, 1.int8), Assignment.new('b'.var, 2.int8)

  it_parses 'let a: int = 1', VariableDeclaration.new('a', NamedType.new('int'), 1.int8, const: false)
  it_parses 'const a: int = 1', VariableDeclaration.new('a', NamedType.new('int'), 1.int8, const: true)

  it_parses 'a = b.c', Assignment.new('a'.var, MemberAccess.new('b', 'c'))
  it_parses 'a = b.c.d', Assignment.new('a'.var, MemberAccess.new(MemberAccess.new('b', 'c'), 'd'))
  it_parses 'a = b.c.d()', Assignment.new('a'.var, Call.new(MemberAccess.new('b', 'c'), 'd'))
  it_parses 'a = b.c.d(1)', Assignment.new('a'.var, Call.new(MemberAccess.new('b', 'c'), 'd', 1.int8.arg))

  it_parses 'fn foo(): void end', MethodDefinition.new('foo', [], Void.new, [])
  it_parses 'fn foo(a: int): void end',
            MethodDefinition.new('foo', [Parameter.new('a', NamedType.new('int'))], Void.new, [])

  it_parses %(
    class Foo
      fn foo(a: int, b: int): void end
    end
    ), ClassDefinition.new(
      'Foo', [
        MethodDefinition.new(
          'foo', [
            Parameter.new('a', NamedType.new('int')),
            Parameter.new('b', NamedType.new('int'))
          ],
          Void.new,
          []
        )
      ]
    )

  it_parses %(
    class Foo
      fn Foo(a: int) end
    end
    ), ClassDefinition.new(
      'Foo', [
        MethodDefinition.new(
          'Foo', [
            Parameter.new('a', NamedType.new('int'))
          ],
          Void.new,
          []
        )
      ]
    )

  it_parses %(
    class Foo
      name: String
    end
    ), ClassDefinition.new(
      'Foo', [
        Property.new('name', type: NamedType.new('String'), default: nil)
      ]
    )

  it_parses %(
    class Foo
      name: String = 'John'
    end
    ), ClassDefinition.new(
      'Foo', [
        Property.new('name', type: NamedType.new('String'), default: 'John'.string)
      ]
    )

  it_parses %(
    class Foo
      name = 'John'
    end
    ), ClassDefinition.new(
      'Foo', [
        Property.new('name', type: nil, default: 'John'.string)
      ]
    )
end
