# frozen_string_literal: true

class Class # :nodoc:
  # Gets the short name of the class, without the module name.
  #
  # @return [String] The short name of the class.
  def short_name
    to_s.split('::').last
  end
end
