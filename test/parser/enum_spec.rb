# frozen_string_literal: true

require_relative 'test_helper'

include Lume::Syntax

describe Lume::Parser do
  it_parses %(
    enum Foo { }
    ), Enum.new('Foo', [])

  it_parses %(
    enum Foo {
      Bar
    }
    ), Enum.new(
      'Foo',
      [
        EnumCase.new('Bar', [])
      ]
    )

  it_parses %(
      enum Foo {
        Bar,
        Baz
      }
      ), Enum.new(
        'Foo',
        [
          EnumCase.new('Bar', []),
          EnumCase.new('Baz', [])
        ]
      )

  it_parses %(
    enum Foo {
      Bar()
    }
    ), Enum.new(
      'Foo',
      [
        EnumCase.new('Bar', [])
      ]
    )

  it_parses %(
  enum Foo {
    Bar(baz: Int)
  }
  ), Enum.new(
    'Foo',
    [
      EnumCase.new('Bar', [Parameter.new('baz', NamedType.new('Int'))])
    ]
  )

  it_parses %(
    enum Foo {
      Bar(value: Int),
      Baz(value: Int)
    }
    ), Enum.new(
      'Foo',
      [
        EnumCase.new('Bar', [Parameter.new('value', NamedType.new('Int'))]),
        EnumCase.new('Baz', [Parameter.new('value', NamedType.new('Int'))])
      ]
    )
end
