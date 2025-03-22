# frozen_string_literal: true

class Object
  def printer_ignore(*names)
    names.each { |name| printer_ignored[name] = true }
  end

  def printer_ignored?(name)
    printer_ignored.include?(name)
  end

  def printer_ignored
    @@printer_ignored ||= {}
    @@printer_ignored
  end
end
