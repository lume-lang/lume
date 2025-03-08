# frozen_string_literal: true

class String # :nodoc:
  # Converts a camel case string to an underscored string.
  #
  # @return [String] The underscored string.
  def underscore
    to_s.gsub(/([A-Z]+)([A-Z][a-z])/, '\1_\2')
        .gsub(/([a-z\d])([A-Z])/, '\1_\2')
        .tr('-', '_')
        .downcase
        .split('::')
        .last
  end
end
