# frozen_string_literal: true

require 'colorize'

module Lume
  # Provides a way to pretty-print objects in a human-readable format.
  #
  # While very similar to Ruby's `pp` function, it allows for more customization and control over the output.
  class Printer
    attr_accessor :indentation

    def initialize(output = $stdout)
      @output = output

      @indentation = 2
      @indentation_level = 0

      @visited = {}
      @visited.compare_by_identity
    end

    # Increases the indentation level.
    #
    # @return [void]
    def indent
      @indentation_level += 1
    end

    # Decreases the indentation level.
    #
    # @return [void]
    def unindent
      @indentation_level -= 1
    end

    # Prints a new line.
    #
    # @return [void]
    def newline
      emit("\n")
    end

    # Prints the given line with the current indentation.
    #
    # @param text [String] The text to print.
    # @param indent [Boolean] Whether to indent the line.
    # @param color [Symbol, nil] The color to use for the text, if any.
    #
    # @return [void]
    def line(text, indentation: true, color: nil)
      text(text, indentation: indentation, color: color)

      newline
    end

    # Prints the given text with the current indentation.
    #
    # @param text [String] The text to print.
    # @param indent [Boolean] Whether to indent the line.
    # @param color [Symbol, nil] The color to use for the text, if any.
    #
    # @return [void]
    def text(text, indentation: true, color: nil)
      # Prepend the indentation to the text
      text = (' ' * @indentation * @indentation_level) + text if indentation

      emit(text, color: color)
    end

    # Emits the given text to the output device.
    #
    # @param text [String] The text to emit.
    # @param color [Symbol, nil] The color to use for the text, if any.
    #
    # @return [void]
    def emit(text, color: nil)
      # If a color is given, colorize the text
      text = text.colorize(color) if color

      @output << text
    end

    # Emits a group of lines with the given header.
    #
    # @param header [String] The header to print.
    # @param &block [Proc] The block to print.
    #
    # @return [void]
    def group(header, &)
      line header, indentation: false, color: :blue

      wrapped(&) if block_given?
    end

    # Emits a value to the output device.
    #
    # @param value [Object] The value to print.
    #
    # @return [void]
    def item(value, prefix: nil)
      prefix(prefix) if prefix

      case value
      when NilClass then line 'nil', indentation: false, color: :gray
      when TrueClass, FalseClass then line value.to_s, indentation: false, color: :green
      when Array then array(value)
      when String then string(value)
      when Object then obj(value)
      end
    end

    # Emits an object to the output device.
    #
    # @param obj [Object] The object to print.
    #
    # @return [void]
    def obj(obj)
      header = format('#<%<name>s 0x%<id>08X>', name: obj.class.name, id: obj.object_id)

      # If we've already visited the object, add an ellipsis
      if visited?(obj)
        header += '...'.colorize(:gray)
        return group(header)
      end

      group(header) do
        # If we've already visited the object, truncate it.
        return if visited?(obj)

        # Otherwise, mark the object as visited.
        visit(obj)

        obj_members(obj)
      end
    end

    # Emits an array to the output device.
    #
    # @param value [Array] The array to print.
    #
    # @return [void]
    def array(value)
      return line '[]', indentation: false if value.empty?

      line '[', indentation: false

      wrapped do
        value.each do |item|
          text ''
          item(item)
        end
      end

      line ']'
    end

    # Emits a string to the output device.
    #
    # @param value [String] The string to print.
    #
    # @return [void]
    def string(value)
      text "\"#{value}\"", indentation: false, color: :green
      newline
    end

    # Emits the given block within an indented block.
    #
    # @param &block [Proc] The block to print.
    #
    # @return [void]
    def wrapped
      indent
      yield
      unindent
    end

    private

    # Prints the given prefix.
    #
    # @param prefix [String, nil] The prefix to print.
    #
    # @return [void]
    def prefix(prefix)
      text format('[%s] = ', prefix), color: :yellow
    end

    # Emits the members of the given object.
    #
    # @param obj [Object] The object to print.
    #
    # @return [void]
    def obj_members(obj)
      attributes = obj.instance_variables

      attributes.each { |attr| obj_member(obj, attr) }
    end

    # Emits a single member of the given object.
    #
    # @param obj [Object] The object to print.
    # @param attr [Symbol] The name of the attribute to print.
    #
    # @return [void]
    def obj_member(obj, attr)
      name = attr.to_s.sub(/^@/, '')
      return if obj.printer_ignored?(name.to_sym)

      value = obj.instance_variable_get(attr)

      item(value, prefix: name)
    end

    # Determines if the given object has been visited.
    #
    # @param obj [Object] The object to check.
    #
    # @return [Boolean] True if the object has been visited, false otherwise.
    def visited?(obj)
      @visited.include?(obj)
    end

    # Marks the given object as visited.
    #
    # @param obj [Object] The object to mark as visited.
    #
    # @return [void]
    def visit(obj)
      @visited[obj] = true
    end
  end
end
