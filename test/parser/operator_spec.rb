# frozen_string_literal: true

require_relative 'test_helper'

include Lume::Syntax

describe Lume::Parser do
  Lume::Parser::OPERATORS.each do |operator|
    it_parses "fn external #{operator}() -> void", MethodDefinition.new(operator.to_s, [], Void.new, [])
  end
end
