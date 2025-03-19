# frozen_string_literal: true

require_relative 'test_helper'

describe 'object referencing' do
  include AnalyzerHelper

  it 'should keep same reference to class definitions' do
    mod, * = analyze(%(
      class String { }

      let a = new String()
    ))

    class_def = mod.mir.find_first(Lume::MIR::ClassDefinition)
    variable_decl = mod.mir.find_first(Lume::MIR::VariableDeclaration)

    assert_same(class_def, variable_decl.value.class_def)
  end
end
