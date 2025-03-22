# frozen_string_literal: true

require 'thor'

class Thor
  no_commands do
    # Compiles the target file using the Lume compiler.
    #
    # @param path [String, nil] The path to the target file.
    #
    # @return [Lume::CompilationContext] The output of the compiler.
    def compile_target(path = nil)
      # Get the absolute path to the target file.
      path ||= find_target_path(path)

      # Compile the target file using the Lume compiler.
      driver = Lume::Driver.new(Lume::CODEGEN, verbose: options[:verbose])
      driver.build_file(path)
    end

    # Determines the target path to build or run.
    #
    # @param path [String, nil] The path given by the user.
    #
    # @return [String] The target path to build or run.
    def find_target_path(path = nil)
      # If a path was given, return it.
      return path unless path.nil?

      # If no path was provided, attempt to find a main.lm file in the current directory
      path = 'main.lm' if path.nil? && File.file?('main.lm')

      path
    end
  end
end
