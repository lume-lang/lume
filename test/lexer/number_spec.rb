# frozen_string_literal: true

require_relative 'test_helper'

describe 'lexer numbers' do
  it 'lexes positive integer' do
    lexer = Lume::Language::Lexer.new('1')
    tokens = lexer.all!

    assert_tokens_equal([[:number, 1], [:eof, nil]], tokens)
  end

  it 'lexes negative integer' do
    lexer = Lume::Language::Lexer.new('-1')
    tokens = lexer.all!

    assert_tokens_equal([[:-, '-'], [:number, 1], [:eof, nil]], tokens)
  end

  it 'lexes floating point numbers' do
    lexer = Lume::Language::Lexer.new('1.23456')
    tokens = lexer.all!

    assert_tokens_equal([[:number, 1.23456], [:eof, nil]], tokens)
  end

  it 'lexes negative floating point numbers' do
    lexer = Lume::Language::Lexer.new('-1.23456')
    tokens = lexer.all!

    assert_tokens_equal([[:-, '-'], [:number, 1.23456], [:eof, nil]], tokens)
  end

  it 'lexes numbers in scientific notation' do
    lexer = Lume::Language::Lexer.new('1234e-2')
    tokens = lexer.all!

    assert_tokens_equal([[:number, 12.34], [:eof, nil]], tokens)
  end

  it 'lexes numbers in human-readable notation' do
    lexer = Lume::Language::Lexer.new('1_234')
    tokens = lexer.all!

    assert_tokens_equal([[:number, 1234], [:eof, nil]], tokens)
  end

  it 'lexes numbers in decimal notation' do
    lexer = Lume::Language::Lexer.new('0d170')
    tokens = lexer.all!

    assert_tokens_equal([[:number, 170], [:eof, nil]], tokens)
  end

  it 'lexes numbers in hexadecimal notation' do
    lexer = Lume::Language::Lexer.new('0xaa')
    tokens = lexer.all!

    assert_tokens_equal([[:number, 170], [:eof, nil]], tokens)
  end

  it 'lexes numbers in binary notation' do
    lexer = Lume::Language::Lexer.new('0b10101010')
    tokens = lexer.all!

    assert_tokens_equal([[:number, 170], [:eof, nil]], tokens)
  end

  %w[i8 u8 i16 u16 i32 u32 i64 u64 i128 u128 f32 f64].each do |type|
    it "lexes numbers with explicit type (#{type})" do
      lexer = Lume::Language::Lexer.new("1_#{type}")
      token = lexer.next_token!

      assert_equal(:number, token.type)
      assert_equal(1, token.value)
      assert_equal(type, token.kind)
    end
  end
end
