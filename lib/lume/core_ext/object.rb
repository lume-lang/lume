# frozen_string_literal: true

class Object # :nodoc:
  def printer_ignore(*names)
    names.each { |name| printer_ignored[name] = true }
  end

  def printer_ignored?(name)
    printer_ignored.include?(name)
  end

  def printer_ignored
    # rubocop:disable Style/ClassVars -- The printer needs to be able to access this variable across multiple instances of the class.

    @@printer_ignored ||= {}
    @@printer_ignored

    # rubocop:enable Style/ClassVars
  end
end
