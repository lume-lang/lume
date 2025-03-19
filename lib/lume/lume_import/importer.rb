# frozen_string_literal: true

require 'lume/lume_syntax/hir'
require 'lume/lume_import/errors/unknown_import'

module Lume
  # Imports other Lume files given the `import` statements within an AST.
  #
  # The importer handles recursive imports and ensures that each file is only imported once.
  # It also recursively imports files within imported files.
  class Importer
    include Lume::Importer::Errors

    attr_reader :imported_files, :dependencies

    def initialize
      @imported_files = {}
      @dependencies = {}
    end

    # Performs the import of the given AST.
    #
    # Import statements will be removed from the AST, as they will have been resolved.
    #
    # @param name [String] The name of the file or module being imported.
    # @param ast [Lume::AST] The AST to import.
    #
    # @return [Importer] The importer instance.
    def self.import!(name, ast)
      importer = new
      importer.import!(name, ast)

      importer
    end

    # Handles the imports within the given AST.
    #
    # @param name [String] The name of the file or module being imported.
    # @param ast [Lume::AST] The AST to import.
    #
    # @return [Lume::AST] The AST with all imports resolved.
    def import!(name, ast)
      # Import all the files recursively.
      ast.imports.each { |import| import_file!(ast, import.library) }

      # Map the dependencies of the module.
      @dependencies[name] = ast.imports.map(&:library)

      # Remove all the import statements within the AST.
      ast.nodes.delete_if { |node| node.is_a?(Lume::Syntax::Import) }

      ast
    end

    # Handles a single import of the given file.
    #
    # @param ast [Lume::AST] The AST to import symbols into.
    # @param path [String] The path to the file to import.
    #
    # @return [Lume::AST] The AST with all imports resolved.
    def import_file!(ast, path)
      # If the file is already imported, return early.
      return if imported?(path)

      # Mark the file as imported, so we don't enter an infinite loop.
      @imported_files[path] = true

      # If the file is not already imported, read the contents to a source file.
      source_file = read_imported_file(path)

      # Parse the contents of the library file.
      @imported_files[path] = parse_library(path, source_file)

      # Merge the AST nodes from the imported library into the current AST.
      # ast.nodes.concat(library_ast.nodes)

      ast
    end

    # Determines if the given file has already been imported.
    #
    # @param file [String] The file to check.
    #
    # @return [Boolean] True if the file has been imported, false otherwise.
    def imported?(file)
      @imported_files.include?(file)
    end

    private

    # Reads the library from the given path.
    #
    # @param path [String] The relative path to the library to import.
    #
    # @return [Lume::SourceFile] The source file of the library.
    def read_imported_file(path)
      path = resolve_import_path(path)
      content = File.read(path)

      SourceFile.new(path, content)
    end

    # Resolves the absolute path of the given import path.
    #
    # @param path [String] The path to resolve.
    #
    # @return [String] The absolute path of the given import path.
    def resolve_import_path(path)
      path = path.delete_suffix('.lm')

      # If the library is not a standard library, raise an error.
      raise UndefinedImport.new(expression) unless path == 'std' || path.start_with?('std/')

      # Resolve the absolute path to the library.
      library_path = File.expand_path(File.join(Lume::STD_DIR, '..', "#{path}.lm"))

      # If the library could not be found, raise an error.
      raise UndefinedImport.new(expression) unless File.exist?(library_path)

      library_path
    end

    # Parses the given source code and recursively imports all nested imports.
    #
    # @param name [String] The name of the library.
    # @param source [Lume::SourceFile] The library source file to parse.
    #
    # @return [Lume::Syntax::AST] The AST of the parsed source code.
    def parse_library(name, source)
      # Parse the contents of the library file.
      parser = Lume::Parser.with_source(source)
      library_ast = parser.parse_module

      # Handle imports within the library, as well. This will recursively import all nested imports,
      # until all imports have been visited.
      import!(name, library_ast)
    end
  end
end
