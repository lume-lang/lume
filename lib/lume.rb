# frozen_string_literal: true

require 'lume/core_ext/class'
require 'lume/core_ext/string'

require 'lume/errors'
require 'lume/lume_lexer/lexer'
require 'lume/lume_parser/parser'
require 'lume/lume_analyzer/analyzer'
require 'lume/lume_compiler/compiler'

module Lume
  # Defines the root of the Lume gem project.
  LIB_DIR = File.expand_path('..', __dir__)

  # Defines the root of the Lume standard library.
  STD_DIR = File.join(LIB_DIR, 'std')

  LEX = :lex
  PARSE = :parse
  ANALYZE = :analyze
  CODEGEN = :codegen

  STAGES = [
    Lume::LEX,
    Lume::PARSE,
    Lume::ANALYZE,
    Lume::CODEGEN
  ].freeze

  # This exception is raised when the user provides an invalid stage argument
  # to the Lume driver.
  class InvalidStageError < StandardError
    def initialize(stage)
      super("Invalid stage '#{stage}' given. Available options are [#{Lume::STAGES.join(', ')}]")
    end
  end

  # Defines a common context for compilation runs.
  #
  # Depending on the stage of the compilation process, this class will provide the following:
  #   - Lexing: The source code being tokenized
  #   - Parsing: The tokens which have been lexed in the previous stage.
  #   - Analysis: The parsed AST which has not yet been analyzed.
  #   - Code Generation: The analyzed AST, which is ready to be transpiled into LLVM IR and the global symbol table.
  #   - Finish: The LLVM IR code, which is ready to be executed.
  class CompilationContext
    attr_accessor :stage, :entry, :tokens, :modules, :llvm_module

    def initialize(entry)
      @stage = Lume::LEX
      @entry = entry
    end

    # Gets all the source files, which are being compiled.
    #
    # @return [Hash<String, SourceFile>] A hash mapping file names to their source code.
    def files
      # If the modules haven't been populated yet, fallback to the entry source.
      return { @entry.path => @entry } if @modules.nil?

      @modules.to_h { |mod| [mod.name, mod.source] }
    end
  end

  # The Lume Driver is a wrapper around most compiler functionality, allowing for easy
  # compilation of Lume source code into LLVM IR. It provides a convenient interface for
  # running the entire compilation process from start to finish.
  #
  # The driver is also responsible for printing errors to the console via the `Lume::ErrorPrinter` class.
  class Driver
    attr_reader :stage, :dest_stage

    def initialize(stage = Lume::CODEGEN, verbose: false)
      @stage = Lume::LEX
      @dest_stage = stage
      @verbose = verbose

      unless Lume::STAGES.include?(stage)
        raise InvalidStageError, "Invalid stage: #{stage}. See Lume::Driver::STAGES for valid stages."
      end

      @logger = Lume::ErrorPrinter.new
    end

    # Runs the Lume compiler on the given source code and builds it.
    #
    # @param source [String] The source code to compile.
    # @param filename [String] The filename of the source code.
    #
    # @return [CompilationContext] The compiled code object(s).
    def build(source, filename: nil)
      source_file = SourceFile.new(filename, source)
      context = CompilationContext.new(source_file)

      @logger.attach_context!(context)

      Lume::STAGES.each do |stage|
        # If the stage fails to execute, stop and return
        break unless execute_stage(stage, context)

        # If the compilation process has finished, return the context
        return context if finished?
      end

      context
    rescue StandardError => e
      @logger.report(e)

      puts e.backtrace if @verbose && e.backtrace

      nil
    end

    # Runs the Lume compiler on the given file and builds it.
    #
    # @param filename [String] The filename of the source code.
    #
    # @return [CompilationContext] The compiled code object(s).
    def build_file(filename)
      # Read the entire file into memory.
      # This might be an issue later on, but for now it's fine.
      source = File.read(filename)

      build(source, filename: filename)
    end

    # Runs the Lume compiler on the given source code.
    #
    # @param source [String] The source code to compile.
    # @param filename [String] The filename of the source code.
    # @param args [Array<Object>] The arguments to pass to the module's `main` function.
    #
    # @return [Integer] The exit code of the program.
    def run(source, *, filename: nil)
      context = build(source, filename: filename)

      return nil unless context

      runner = Runner.new(context)
      result = runner.run(*)

      result.to_i
    end

    # Runs the Lume compiler on the given source file.
    #
    # @param filename [String] The filename of the source code.
    #
    # @return [Integer] The exit code of the program.
    def run_file(filename)
      # Read the entire file into memory.
      # This might be an issue later on, but for now it's fine.
      source = File.read(filename)

      run(source, filename: filename)
    end

    # Determines if the compilation process has finished.
    #
    # Whether the compilation process has finished depends on the destination stage, which is set during
    # the initialization of the compiler.
    #
    # @return [Boolean] Returns `true` if the compilation process has finished. Otherwise, returns `false`.
    #
    # @see Lume::Driver#initialize
    def finished?
      dest_stage == stage
    end

    private

    # Executes a single stage of the compilation process.
    #
    # @param stage    [Symbol] The stage to execute.
    # @param context  [CompilationContext] The compiler context.
    #
    # @return [Boolean] Returns `true` if the stage was executed successfully. Otherwise, returns `false`.
    def execute_stage(stage, context)
      # Update the current stage of the driver
      @stage = context.stage = stage

      # Execute the stage method on the context.
      method(stage).call(context)

      # If the logger reported any errors during the stage, return false.
      return false if @logger.errors?

      true
    end

    # Lexes the given source code into tokens, which can be processed by the parser.
    #
    # @param source [String] The source code to lex.
    #
    # @return [CompilationContext]
    def lex(context)
      lexer = Lume::Lexer.new(context.entry)
      context.tokens = lexer.all!

      context
    end

    # Parses the given tokens into an abstract syntax tree (AST).
    #
    # @param context [CompilationContext] The compiler context to analyze.
    #
    # @return [CompilationContext] A parsed AST representing the source code.
    def parse(context)
      parser = Lume::Parser.with_tokens(context.entry, context.tokens)
      context.modules = parser.parse

      context
    end

    # Analyzes the given ASTs and lowers it into MIR (Middle-level Intermediate Representation).
    #
    # @param context [CompilationContext] The compiler context to analyze.
    #
    # @return [CompilationContext]
    def analyze(context)
      analyzer = Lume::Analyzer.new(context.modules, logger: @logger)
      analyzer.analyze!

      context
    end

    # Compiles the given CompilerIR into an LLVM module, which can be executed by the LLVM runtime.
    #
    # @param context [CompilationContext] The compiler context to compile.
    #
    # @return [CompilationContext]
    def codegen(context)
      compiler = Lume::Compiler.new

      compiler.compile!(context)
      compiler.optimize!

      context.llvm_module = compiler.module

      context
    end
  end

  # Runner for executing and evaluating compiled Lume modules.
  class Runner
    def initialize(context)
      @context = context
      @module = context.llvm_module
    end

    # Evaluates / executes the compiled module, as if it were a compiled executable.
    #
    # @param args [Array] The arguments to pass to the module's `main` function.
    #
    # @return [Integer]
    def run(*args)
      engine = LLVM::JITCompiler.new(@module)

      argc = args.length
      argv = nil if args.empty?

      engine.run_function(engine.functions[Lume::Compiler::MAIN_NAME], argc, argv)
    end
  end
end
