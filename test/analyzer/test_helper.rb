# frozen_string_literal: true

require_relative '../test_helper'

module AnalyzerHelper
  def analyze(source)
    parser = Lume::Parser.with_source(source)
    modules = parser.parse

    analyzer = Lume::Analyzer.with_modules(modules)
    analyzer.analyze!

    modules
  end
end
