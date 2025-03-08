# frozen_string_literal: true

require_relative 'test_helper'

describe 'lexer strings' do
  it 'lexes empty string literal' do
    lexer = Nox::Language::Lexer.new('""')
    tokens = lexer.all!

    assert_tokens_equal([[:string, ''], [:eof, nil]], tokens)
  end

  it 'lexes whitespace string literal' do
    lexer = Nox::Language::Lexer.new('"   "')
    tokens = lexer.all!

    assert_tokens_equal([[:string, '   '], [:eof, nil]], tokens)
  end

  it 'lexes simple string literal' do
    lexer = Nox::Language::Lexer.new('"hello world"')
    tokens = lexer.all!

    assert_tokens_equal([[:string, 'hello world'], [:eof, nil]], tokens)
  end

  it 'lexes string literal with newline' do
    lexer = Nox::Language::Lexer.new("\"hello\nworld\"")
    tokens = lexer.all!

    assert_tokens_equal([[:string, "hello\nworld"], [:eof, nil]], tokens)
  end

  it 'lexes string literal with single quotes' do
    lexer = Nox::Language::Lexer.new("'hello world'")
    tokens = lexer.all!

    assert_tokens_equal([[:string, 'hello world'], [:eof, nil]], tokens)
  end

  it 'lexes string literal with mixed quotes' do
    lexer = Nox::Language::Lexer.new("'hello\"world'")
    tokens = lexer.all!

    assert_tokens_equal([[:string, 'hello"world'], [:eof, nil]], tokens)
  end

  it 'lexes string literal with back slashes' do
    lexer = Nox::Language::Lexer.new("'hello\\\\world'")
    tokens = lexer.all!

    assert_tokens_equal([[:string, 'hello\\world'], [:eof, nil]], tokens)
  end

  it 'lexes string literal escaped quotes' do
    lexer = Nox::Language::Lexer.new("'hello\\'world'")
    tokens = lexer.all!

    assert_tokens_equal([[:string, "hello'world"], [:eof, nil]], tokens)
  end
end
