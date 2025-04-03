# frozen_string_literal: true

require_relative 'test_helper'

describe 'return values' do
  include CompilerHelper

  it 'return the value of the expression' do
    ret = compile(%(
      fn main(argc: int, argv: *byte) -> int {
        return 4
      }
    ))

    assert_same(ret, 4)
  end

  it 'return values from expressions' do
    ret = compile(%(
      fn main(argc: int, argv: *byte) -> int {
        let a = 4
        return a
      }
    ))

    assert_same(ret, 4)
  end

  it 'return inside conditionals (truthy)' do
    ret = compile(%(
      fn main(argc: int, argv: *byte) -> int {
        if true {
          return 1
        }

        return 0
      }
    ))

    assert_same(ret, 1)
  end

  it 'return inside conditionals (falsy)' do
    ret = compile(%(
      fn main(argc: int, argv: *byte) -> int {
        if false {
          return 1
        }

        return 0
      }
    ))

    assert_same(ret, 0)
  end
end
