# frozen_string_literal: true

require_relative '../test_helper'

module CompilerHelper
  def compile(source)
    driver = Lume::Driver.new
    driver.run(source)
  end
end
