# frozen_string_literal: true

require_relative 'test_helper'

describe 'scalars' do
  include AnalyzerHelper

  %w[Int8 Int16 Int32 Int64 UInt8 UInt16 UInt32 UInt64].each do |type|
    it "handles creating instances of built-in integer type '#{type}'" do
      analyze(%(
        fn main(argc: int, argv: *byte) -> int {
          let a = new #{type}(42)

          return 0
        }
      ))
    end
  end

  it 'handles creating instances of built-in boolean types' do
    analyze(%(
      fn main(argc: int, argv: *byte) -> int {
        let a = new Boolean(true)

        return 0
      }
    ))
  end

  %w[Float Double].each do |type|
    it "handles creating instances of built-in floating-point type '#{type}'" do
      analyze(%(
        fn main(argc: int, argv: *byte) -> int {
          let a = new #{type}(3.14)

          return 0
        }
      ))
    end
  end
end
