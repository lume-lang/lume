# frozen_string_literal: true

require_relative 'test_helper'

describe 'conditions' do
  include CompilerHelper

  it 'branches to then-block given truthy condition' do
    ret = compile(%(
      fn main(argc: int, argv: Pointer) -> int {
        if true {
          return 1
        }

        return 0
      }
    ))

    assert_same(ret, 1)
  end

  it 'branches to merge-block given falsy condition' do
    ret = compile(%(
      fn main(argc: int, argv: Pointer) -> int {
        if false {
          return 1
        }

        return 0
      }
    ))

    assert_same(ret, 0)
  end

  it 'branches to else-block given falsy condition' do
    ret = compile(%(
      fn main(argc: int, argv: Pointer) -> int {
        if false {
          return 1
        } else {
          return 2
        }

        return 0
      }
    ))

    assert_same(ret, 2)
  end

  it 'branches to elif-block' do
    ret = compile(%(
      fn main(argc: int, argv: Pointer) -> int {
        if false {
          return 1
        } else if true {
          return 2
        }

        return 0
      }
    ))

    assert_same(ret, 2)
  end

  it 'branches to else-block' do
    ret = compile(%(
      fn main(argc: int, argv: Pointer) -> int {
        if false {
          return 1
        } else if false {
          return 2
        } else {
          return 3
        }

        return 0
      }
    ))

    assert_same(ret, 3)
  end

  it 'handles equality tests' do
    ret = compile(%(
      fn main(argc: int, argv: Pointer) -> int {
        let a = true

        if a == true {
          return 1
        }

        return 0
      }
    ))

    assert_same(ret, 1)
  end
end
