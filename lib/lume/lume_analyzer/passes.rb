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
        pass = pass.to_sym if pass.is_a?(String)

        @passes << pass
      end

      # Adds a new pass before the specified existing pass in the collection.
      #
      # @param existing [String, Symbol]  The name of the existing pass.
      # @param new [String, Symbol]       The name of the new pass to add.
      #
      # @return [void]
      def insert_before(existing, new)
        existing = existing.to_sym if existing.is_a?(String)

        index = @passes.index(existing)
        return if index.nil?

        new = new.to_sym if new.is_a?(String)
        @passes.insert(index, new)
      end

      # Adds a new pass after the specified existing pass in the collection.
      #
      # @param existing [String, Symbol]  The name of the existing pass.
      # @param new [String, Symbol]       The name of the new pass to add.
      #
      # @return [void]
      def insert_after(existing, new)
        existing = existing.to_sym if existing.is_a?(String)

        index = @passes.index(existing)
        return if index.nil?

        new = new.to_sym if new.is_a?(String)
        @passes.insert(index + 1, new)
      end
    end
  end
end
