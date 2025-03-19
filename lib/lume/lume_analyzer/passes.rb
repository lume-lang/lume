# frozen_string_literal: true

require 'lume/lume_mir_visitor/visitor'
require 'lume/lume_lowering/generator'
require 'lume/lume_analyzer/main_visitor'
require 'lume/lume_typing/typing'

module Lume
  class Analyzer # :nodoc:
    # Defines a collection of analyzer passes, which will be executed in order.
    class Passes
      attr_reader :passes

      def initialize
        @passes = []
      end

      # Adds the new pass at the bottom of the pass collection.
      #
      # @param pass [String, Symbol] The name of the pass to add.
      #
      # @return [void]
      def use(pass)
        @passes << pass.to_sym
      end

      # Adds a new pass before the specified existing pass in the collection.
      #
      # @param existing [String, Symbol]  The name of the existing pass.
      # @param new [String, Symbol]       The name of the new pass to add.
      #
      # @return [void]
      def insert_before(existing, new)
        index = @passes.index(existing.to_sym)
        return if index.nil?

        @passes.insert(index, new.to_sym)
      end

      # Adds a new pass after the specified existing pass in the collection.
      #
      # @param existing [String, Symbol]  The name of the existing pass.
      # @param new [String, Symbol]       The name of the new pass to add.
      #
      # @return [void]
      def insert_after(existing, new)
        index = @passes.index(existing.to_sym)
        return if index.nil?

        @passes.insert(index + 1, new.to_sym)
      end
    end
  end
end
