# frozen_string_literal: true

module Lume
  # Defines a module which has been parsed and optionally analyzed.
  #
  # Modules represent a "package" in Lume, where each package is some compilation unit, be it
  # a project, a single source or a library.
  #
  # Each module has a unique name, which is used to identify it within the system, the parsed HIR AST,
  # the analyzed MIR AST and some optional dependencies.
  class Module
    attr_reader :source
    attr_accessor :name, :hir, :mir, :dependencies

    printer_ignore :source, :dependencies

    def initialize(name, hir, source: nil)
      @name = name
      @source = source

      @hir = hir
      @mir = nil
      @dependencies = []
    end
  end
end
