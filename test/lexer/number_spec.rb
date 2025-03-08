# frozen_string_literal: true

require_relative 'test_helper'

describe 'lexer numbers' do
  it 'lexes positive integer' do
    lexer = Nox::Language::Lexer.new('1')
    tokens = lexer.all!

    assert_tokens_equal([[:number, 1], [:eof, nil]], tokens)
  end

  it 'lexes negative integer' do
    lexer = Nox::Language::Lexer.new('-1')
    tokens = lexer.all!

    assert_tokens_equal([[:dash, '-'], [:number, 1], [:eof, nil]], tokens)
  end

  it 'lexes floating point numbers' do
    lexer = Nox::Language::Lexer.new('1.23456')
    tokens = lexer.all!

    assert_tokens_equal([[:number, 1.23456], [:eof, nil]], tokens)
  end

  it 'lexes negative floating point numbers' do
    lexer = Nox::Language::Lexer.new('-1.23456')
    tokens = lexer.all!

    assert_tokens_equal([[:dash, '-'], [:number, 1.23456], [:eof, nil]], tokens)
  end

  it 'lexes numbers in scientific notation' do
    lexer = Nox::Language::Lexer.new('1234e-2')
    tokens = lexer.all!

    assert_tokens_equal([[:number, 12.34], [:eof, nil]], tokens)
  end

  it 'lexes numbers in human-readable notation' do
    lexer = Nox::Language::Lexer.new('1_234')
    tokens = lexer.all!

    assert_tokens_equal([[:number, 1234], [:eof, nil]], tokens)
  end

  it 'lexes numbers in decimal notation' do
    lexer = Nox::Language::Lexer.new('0d170')
    tokens = lexer.all!

    assert_tokens_equal([[:number, 170], [:eof, nil]], tokens)
  end

  it 'lexes numbers in hexadecimal notation' do
    lexer = Nox::Language::Lexer.new('0xaa')
    tokens = lexer.all!

    assert_tokens_equal([[:number, 170], [:eof, nil]], tokens)
  end

  it 'lexes numbers in binary notation' do
    lexer = Nox::Language::Lexer.new('0b10101010')
    tokens = lexer.all!

    assert_tokens_equal([[:number, 170], [:eof, nil]], tokens)
  end
end
