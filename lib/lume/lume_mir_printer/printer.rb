# frozen_string_literal: true

require 'lume/lume_printer/printer'

module Lume
  module MIR
    class Printer < Lume::Printer
      # Emits the given context or module to the output device.
      #
      # @param ctx_or_mod [Lume::CompilationContext, Lume::Module] The context or parsed module to print.
      #
      # @return [void]
      def print(ctx_or_mod)
        return print_module(ctx_or_mod) if ctx_or_mod.is_a?(Lume::Module)

        ctx_or_mod.modules.each do |mod|
          print_module(mod)
        end
      end

      private

      # Prints the given module to the output device.
      #
      # @param mod [Lume::Module] The module to print.
      #
      # @return [void]
      def print_module(mod)
        name = mod.name || 'nil'

        item(mod, prefix: name)
      end
    end
  end
end
