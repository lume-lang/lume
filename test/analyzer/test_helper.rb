# frozen_string_literal: true

require_relative '../test_helper'

module AnalyzerHelper
  def analyze(source)
    parser = Lume::Language::Parser.with_source(source)
    tree = parser.parse

    analyzer = Lume::Analyzer.with_tree(tree)
    analyzer.analyze!
  end
end
