# frozen_string_literal: true

require_relative 'test_helper'

describe 'lexer comments' do
  it 'lexes empty comments' do
    lexer = Nox::Language::Lexer.new('# ')
    tokens = lexer.all!(include_comments: true)

    assert_tokens_equal([[:comment, ''], [:eof, nil]], tokens)
  end

  it 'lexes simple comments' do
    lexer = Nox::Language::Lexer.new('# some comment content')
    tokens = lexer.all!(include_comments: true)

    assert_tokens_equal([[:comment, 'some comment content'], [:eof, nil]], tokens)
  end

  it 'lexes comments until newline' do
    lexer = Nox::Language::Lexer.new(%(# content
 a))

    tokens = lexer.all!(include_comments: true)

    assert_tokens_equal([[:comment, 'content'], [:name, 'a'], [:eof, nil]], tokens)
  end

  it 'lexes multiple comments' do
    lexer = Nox::Language::Lexer.new(%(# content 1
# content 2))

    tokens = lexer.all!(include_comments: true)

    assert_tokens_equal([[:comment, 'content 1'], [:comment, 'content 2'], [:eof, nil]], tokens)
  end

  it 'lexes multiline comments' do
    lexer = Nox::Language::Lexer.new(%(/* content */))

    tokens = lexer.all!(include_comments: true)

    assert_tokens_equal([[:comment, 'content'], [:eof, nil]], tokens)
  end

  it 'lexes comments between statements' do
    lexer = Nox::Language::Lexer.new(%(a /* content */ = 1))

    tokens = lexer.all!(include_comments: true)

    assert_tokens_equal([[:name, 'a'], [:comment, 'content'], [:equal, '='], [:number, 1], [:eof, nil]], tokens)
  end
end
