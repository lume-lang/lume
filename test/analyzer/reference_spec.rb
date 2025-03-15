# frozen_string_literal: true

require_relative 'test_helper'

describe 'object referencing' do
  include AnalyzerHelper

  it 'should keep same reference to class definitions' do
    tree = analyze(%(
      class String end

      let a = new String()
    ))

    class_def = tree.find_first(Lume::IR::ClassDefinition)
    variable_decl = tree.find_first(Lume::IR::VariableDeclaration)

    assert_same(class_def, variable_decl.value.class_def)
  end
end
