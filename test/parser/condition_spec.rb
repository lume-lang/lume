# frozen_string_literal: true

require_relative 'test_helper'

include Lume::Syntax

describe Lume::Parser do
  it_parses 'if true { }', IfConditional.new(
    condition: BooleanLiteral.new(true)
  )

  it_parses 'if a == true { }', IfConditional.new(
    condition: Call.new('a'.var, '==', BooleanLiteral.new(true))
  )

  it_parses 'if a == true { }', IfConditional.new(
    condition: Call.new('a'.var, '==', BooleanLiteral.new(true))
  )

  it_parses 'if true { a = 1 }', IfConditional.new(
    condition: BooleanLiteral.new(true),
    then_block: [
      Assignment.new('a'.var, ByteLiteral.new(1))
    ]
  )

  it_parses 'if true { a = 1 b = 2 }', IfConditional.new(
    condition: BooleanLiteral.new(true),
    then_block: [
      Assignment.new('a'.var, ByteLiteral.new(1)),
      Assignment.new('b'.var, ByteLiteral.new(2))
    ]
  )

  it_parses 'if true { } else { b = 1 }', IfConditional.new(
    condition: BooleanLiteral.new(true),
    then_block: [],
    else_block: [
      Assignment.new('b'.var, ByteLiteral.new(1))
    ]
  )

  it_parses 'if true { } else { a = 1 b = 2 }', IfConditional.new(
    condition: BooleanLiteral.new(true),
    then_block: [],
    else_block: [
      Assignment.new('a'.var, ByteLiteral.new(1)),
      Assignment.new('b'.var, ByteLiteral.new(2))
    ]
  )

  it_parses 'if true { } else { a = 1 b = 2 }', IfConditional.new(
    condition: BooleanLiteral.new(true),
    then_block: [],
    else_block: [
      Assignment.new('a'.var, ByteLiteral.new(1)),
      Assignment.new('b'.var, ByteLiteral.new(2))
    ]
  )

  it_parses 'if true { } else if false { }', IfConditional.new(
    condition: BooleanLiteral.new(true),
    else_if: [
      ElseIfConditional.new(
        condition: BooleanLiteral.new(false),
        then_block: []
      )
    ]
  )

  it_parses 'if true { } else if false { a = 1 }', IfConditional.new(
    condition: BooleanLiteral.new(true),
    else_if: [
      ElseIfConditional.new(
        condition: BooleanLiteral.new(false),
        then_block: [
          Assignment.new('a'.var, ByteLiteral.new(1))
        ]
      )
    ]
  )

  it_parses 'if true { } else if false { a = 1 b = 2 }', IfConditional.new(
    condition: BooleanLiteral.new(true),
    else_if: [
      ElseIfConditional.new(
        condition: BooleanLiteral.new(false),
        then_block: [
          Assignment.new('a'.var, ByteLiteral.new(1)),
          Assignment.new('b'.var, ByteLiteral.new(2))
        ]
      )
    ]
  )

  it_parses 'if true { } else if false { a = 1 }', IfConditional.new(
    condition: BooleanLiteral.new(true),
    else_if: [
      ElseIfConditional.new(
        condition: BooleanLiteral.new(false),
        then_block: [
          Assignment.new('a'.var, ByteLiteral.new(1))
        ]
      )
    ]
  )

  it_parses 'if true { } else if false { a = 1 b = 2 }', IfConditional.new(
    condition: BooleanLiteral.new(true),
    else_if: [
      ElseIfConditional.new(
        condition: BooleanLiteral.new(false),
        then_block: [
          Assignment.new('a'.var, ByteLiteral.new(1)),
          Assignment.new('b'.var, ByteLiteral.new(2))
        ]
      )
    ]
  )

  it_parses 'if true { } else if false { a = 1 } else { b = 2 }', IfConditional.new(
    condition: BooleanLiteral.new(true),
    else_if: [
      ElseIfConditional.new(
        condition: BooleanLiteral.new(false),
        then_block: [
          Assignment.new('a'.var, ByteLiteral.new(1))
        ]
      )
    ],
    else_block: [
      Assignment.new('b'.var, ByteLiteral.new(2))
    ]
  )

  it_parses 'unless a == true { }', UnlessConditional.new(
    condition: Call.new('a'.var, '==', BooleanLiteral.new(true))
  )

  it_parses 'unless true { a = 1 }', UnlessConditional.new(
    condition: BooleanLiteral.new(true),
    then_block: [
      Assignment.new('a'.var, ByteLiteral.new(1))
    ]
  )

  it_parses 'unless true { a = 1 b = 2 }', UnlessConditional.new(
    condition: BooleanLiteral.new(true),
    then_block: [
      Assignment.new('a'.var, ByteLiteral.new(1)),
      Assignment.new('b'.var, ByteLiteral.new(2))
    ]
  )

  it_parses 'unless true { a = 1 } else { b = 2 }', UnlessConditional.new(
    condition: BooleanLiteral.new(true),
    then_block: [
      Assignment.new('a'.var, ByteLiteral.new(1))
    ],
    else_block: [
      Assignment.new('b'.var, ByteLiteral.new(2))
    ]
  )

  it_cannot_parse 'unless true { } else if false { }'
  it_cannot_parse 'unless true { a = 1 } else if false { b = 2 }'
end
