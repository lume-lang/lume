# frozen_string_literal: true

Dir.glob("#{__dir__}/errors/*.rb").each { |file| require file }
