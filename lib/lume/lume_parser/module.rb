# frozen_string_literal: true

module Lume
  class Parser
    # Defines a module which has been parsed and optionally analyzed.
    #
    # Modules represent a "package" in Lume, where each package is some compilation unit, be it
    # a project, a single source or a library.
    #
    # Each module has a unique name, which is used to identify it within the system, the parsed HIR AST,
    # the analyzed MIR AST and some optional dependencies.
    class Module
      attr_accessor :name, :hir, :mir, :dependencies

      def initialize(name)
        @name = name
        @hir = nil
        @mir = nil
        @dependencies = []
      end

      # Creates a new module with the given name and HIR AST.
      #
      # @param name [String] The name of the module.
      # @param hir  [Lume::Syntax::AST] The HIR AST of the module.
      def self.with_hir(name, hir)
        new(name).tap { |mod| mod.hir = hir }
      end
    end
  end
end
