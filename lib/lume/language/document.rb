# frozen_string_literal: true

module Lume
  module Language
    class Document # :nodoc:
      attr_reader :content
      attr_accessor :functions, :classes, :expressions

      def initialize(content)
        @content = content
        @functions = []
        @classes = []
        @expressions = []
      end
    end
  end
end
