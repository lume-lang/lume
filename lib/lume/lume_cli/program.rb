# frozen_string_literal: true

require 'lume/lume_cli/thor'
require 'lume/lume_cli/commands/dev'

module Lume
  module CLI
    # Defines the Thor application for the Lume CLI.
    #
    # The Lume CLI is the main entry point for interacting with Lume projects and files.
    # It provides commands for building, executing, and managing Lume projects.
    class Program < Thor
      class_option :verbose, aliases: '-v', desc: 'Prints verbose output'

      desc 'dev SUBCOMMAND ...ARGS', 'suite of dev commands'
      subcommand 'dev', Commands::Dev

      desc 'build <path>', 'Builds a Lume project or file'
      long_desc <<-LONGDESC
        `lume build` builds a Lume project or Lume source file
        into an executable, which can be run on any platform.

        If no path is specified, Lume attempts to build the project in the
        current directory, if it exists. If not, it attempts to build `main.lm`.

        > $ lume build main.lm
      LONGDESC

      def build(path = nil)
        path = find_target_path(path)

        puts path
      end

      desc 'exec <path>', 'Evaluates a Lume project or file without building it'
      long_desc <<-LONGDESC
        `lume exec` evaluates a Lume project or Lume source file
        without building it into an executable.

        If no path is specified, Lume attempts to run the project in the
        current directory, if it exists. If not, it attempts to run `main.lm`.

        > $ lume exec main.lm
      LONGDESC

      option :stage, type: :string, default: 'codegen', desc: <<~DESC
        Defines which compilation stage to stop at. Options: [#{Lume::STAGES.join(', ')}]
      DESC

      def exec(path = nil)
        find_target_path(path)

        stage = options[:stage].downcase.to_sym

        driver = Lume::Driver.new(stage, verbose: options[:verbose])
        driver.run_file(path)
      rescue Lume::InvalidStageError
        say "Invalid stage given. Available options are [#{Lume::STAGES.join(', ')}]"
      end

      def self.exit_on_failure?
        true
      end
    end
  end
end
