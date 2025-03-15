# frozen_string_literal: true

require 'colorize'

module Lume
  ERROR = :error
  WARNING = :warning
  INFO = :info

  # The `LumeDiagnostic` class represents any diagnostic or error within the Lume compiler, which
  # caused the compiler to fail or complain.
  class LumeDiagnostic < StandardError
    attr_accessor :message, :type, :previous

    # Initializes a new instance of the `LumeDiagnostic` class.
    #
    # @param message  [String] The diagnostic message.
    # @param type     [Symbol] The type of the diagnostic.
    # @param previous [StandardError, nil] The previous diagnostic that caused this diagnostic, if any.
    #
    # @return [void]
    def initialize(message, type: Lume::ERROR, previous: nil)
      super(message)

      @message = message
      @type = type
      @previous = previous
    end

    # Determines whether the diagnostic has a location associated with it.
    #
    # @return [Boolean] `true` if the diagnostic has a location, `false` otherwise.
    def location?
      !@location.nil?
    end

    # Determines whether the diagnostic has a code associated with it.
    #
    # @return [Boolean] `true` if the diagnostic has a code, `false` otherwise.
    def code?
      !@code.nil?
    end

    protected

    # Attaches a location to the diagnostic.
    #
    # @param location [Lume::Location] The location to attach.
    #
    # @return [void]
    def attach_location(location)
      @location = location
    end
  end

  # The error printer handles reporting compilation errors to the user in a clean, readable format.
  class ErrorPrinter
    # Initializes the error printer.
    #
    # @param context [Lume::CompilationContext, nil] The compilation context to attach, if any.
    #
    # @return [void]
    def initialize(context = nil)
      @has_errors = false
      @context = context
    end

    # Attaches the compiler context to the error printer. Attaching a context allows the error printer to provide
    # specific source locations, documenting where the error occurred.
    #
    # @param context [Lume::CompilationContext] The compilation context to attach.
    #
    # @return [void]
    def attach_context!(context)
      @context = context
    end

    # Reports the given error to the user via the standard output (`stdout`).
    #
    # @param error [StandardError] The error to report.
    #
    # @return [void]
    def report(error)
      return report_compiler_error(error) if error.is_a?(Lume::LumeDiagnostic)

      report_standard_error(error)
    end

    # Determines whether the error printer has reported any errors.
    #
    # @return [Boolean] `true` if errors have been reported, `false` otherwise.
    def errors?
      @has_errors
    end

    private

    # Reports a standard, non-compiler error to the user via the standard output (`stdout`).
    #
    # @param error [StandardError] The error to report.
    #
    # @return [void]
    def report_standard_error(error)
      raise error
    end

    # Reports a compiler error to the user via the standard output (`stdout`).
    #
    # @param error [Lume::LumeDiagnostic] The error to report.
    #
    # @return [void]
    def report_compiler_error(error)
      @has_errors = true if error.type == Lume::ERROR

      error_code = error_code(error)
      error_type = with_severity_color(error.type, error.type)

      puts "#{error_type}#{error_code}: #{error.message} [#{error.class.short_name}]"

      # If the error has a location attached, report it as well.
      report_error_location(error) if error.location?
    end

    # Determines the error code for the given error.
    #
    # @param error [Lume::LumeDiagnostic] The error to determine the code for.
    #
    # @return [String] The error code.
    def error_code(error)
      return '' unless error.code?

      # Use the first letter of the error type as a prefix to the error code.
      #
      # This helps to distinguish between different types of diagnostics and aids when searching for
      # a specific error code on search engines.
      type = error.type.freeze[0].to_s.capitalize

      "[#{type}#{error.code}]"
    end

    # Reports the location of the error to the user via the standard output (`stdout`).
    #
    # @param error [Lume::LumeDiagnostic] The error to report the location for.
    #
    # @return [void]
    def report_error_location(error)
      # If no context has been provided, we can only print the character index to a certain file,
      # which isn't terribly useful.
      return if @context.nil?

      # If there's no location attached, we can't report it.
      return unless error.location?
      return if error.location.nil? || !error.location.is_a?(Lume::Location)

      emit_source_snippet(error, error.location, @context.source)
    end

    # Emits a snippet from the given source code.
    #
    # @param error    [Lume::LumeDiagnostic] The diagnostic object.
    # @param location [Lume::Location] The location of the diagnostic.
    # @param source   [Lume::SourceFile] The source code of the file.
    # @param padding  [Integer] The number of lines to include before and after the error location. Default is `2`.
    #
    # @return [void]
    def emit_source_snippet(error, location, source, padding: 2)
      line, column = source_coordinates(location, source)

      # If a file is attached to the source, emit the file name
      emit_source_file_path(source.path, line, column) unless source.path.nil?

      (0...padding).reverse_each { |i| emit_source_line(source, line - i - 1) }

      emit_source_line_with_diagnostic(source, error, line, column)

      (0...padding).each { |i| emit_source_line(source, line + i + 1) }
    end

    # Emits the path to the given source file.
    #
    # @param path [String] The path to the source file.
    # @param line [Integer] The line number of the error.
    # @param column [Integer] The column number of the error.
    #
    # @return [void]
    def emit_source_file_path(path, line, column)
      puts " --> #{path}:#{line}:#{column}"
    end

    # Emits the text on the given line from the given source in a formatted manner.
    #
    # When emitting a source file, the output will be formatted with a line number and a vertical bar, like so:
    #
    #   3 |   return a
    #
    # @param source [Lume::SourceFile] The source code of the file.
    # @param line [Integer] The line number to em it.
    #
    # @return [void]
    def emit_source_line(source, line)
      source_line = source.line_at(line - 1)

      return if source_line.nil?

      emit_code_formatted_text(line, source_line)
    end

    # Emits the text on the given line from the given source in a formatted manner with the given diagnostic.
    #
    # When emitting a source file, the output will be formatted with a line number and a vertical bar, like so:
    #
    #   3 |   return a
    #     |   ^ Expected Int32, got [Int32, String]
    #
    # @param source [Lume::SourceFile] The source code of the file.
    # @param diagnostic [Lume::LumeDiagnostic] The diagnostic message to emit.
    # @param line [Integer] The line number to place the content on.
    # @param column [Integer] The column number to place the diagnostic on.
    #
    # @return [void]
    def emit_source_line_with_diagnostic(source, diagnostic, line, column)
      emit_source_line(source, line)
      emit_code_diagnostic(diagnostic.message, column - 1, severity: diagnostic.type)
    end

    # Emits a code-formatted diagnostic without a line number in the gutter.
    #
    # @param source [Lume::SourceFile] The source code of the file.
    # @param line [Integer] The line number to emit.
    # @param severity [nil, Symbol] The severity of the diagnostic.
    #
    # @return [void]
    def emit_code_diagnostic(text, padding, severity: nil)
      text = "^ #{text.to_s.freeze}"

      # If the severity is set, color the text accordingly
      text = with_severity_color(text, severity) unless severity.nil?

      adjustment = ' ' * padding
      formatted_text = "#{adjustment}#{text}"

      emit_code_formatted_text('', formatted_text)
    end

    # Emits the given text in a code formatted manner.
    #
    # When emitting a line of text, the output will be formatted with the given gutter and a vertical bar, like so:
    #
    #   3 |   return a
    #
    # Where `3` is the gutter and `return a` is the given text.
    #
    # @param gutter [Integer] The gutter to use.
    # @param text [String] The text to emit.
    #
    # @return [void]
    def emit_code_formatted_text(gutter, text)
      gutter = gutter.to_s.freeze.rjust(4)

      puts "#{gutter} | #{text}"
    end

    # Gets the coordinates of the given location within the source code.
    #
    # @param location [Lume::Location] The location to get the coordinates for.
    # @param source [Lume::SourceFile] The source code of the file.
    #
    # @return [Array<Integer>] The coordinates of the error location.
    def source_coordinates(location, source)
      source_coordinate(location.span.min, source)
    end

    # Gets the coordinate of the given index within the source code.
    #
    # @param index [Integer] The index to get the coordinate for.
    # @param source [e::SourceFile] The source code of the file.
    #
    # @return [Array<Integer>] The coordinates of the error location.
    def source_coordinate(index, source)
      line = 1
      column = 0

      (0..index).each do |i|
        column += 1

        next unless source[i] == "\n"

        line += 1
        column = 0
      end

      [line, column]
    end

    # Colors the given message based on the severity type.
    #
    # @param message [String] The message to format.
    # @param type [Symbol] The severity type.
    #
    # @return [String] The formatted message.
    def with_severity_color(message, type)
      message = message.to_s if message.is_a?(Symbol)

      case type
      when Lume::ERROR then message.colorize(:red)
      when Lume::WARNING then message.colorize(:yellow)
      when Lume::INFO then message.colorize(:light_blue)
      end
    end
  end
end
