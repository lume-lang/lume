# frozen_string_literal: true

require 'thor'

require 'lume/lume_mir_printer/printer'

module Lume
  module CLI
    module Commands
      # Defines a suite of commands which aid in developing the Lume compiler.
      class Dev < Thor
        desc 'print-ast <path>', 'Prints the AST of a Lume project or file'
        long_desc <<-LONGDESC
          `lume dev print-ast` compiles the contents of a Lume project or file in-memory and
          prints the Abstract Syntax Tree (AST) of the compiled code to the console.

          > $ lume dev print-ast main.lm

          If needed, you can also print the AST of a specific module by passing its name as an argument.

          > $ lume dev print-ast main.lm --module 'std/int'

          Likewise, you can print the AST of the entry module, by passing the `--entry-module` flag.

          > $ lume dev print-ast main.lm --entry-module
        LONGDESC

        class_exclusive do
          option :module, type: :string, default: nil, desc: <<~DESC
            Print the AST of a single module with the given name.
          DESC

          option :entry_module, type: :boolean, default: false, desc: <<~DESC
            Prints the AST of the entry module.
          DESC
        end

        def print_ast(path = nil)
          context = compile_target(path)

          printer = Lume::MIR::Printer.new

          if options[:entry_module] == true
            printer.print(context.entry_module)
          elsif !options[:module].nil? && !options[:module].empty?
            mod = context.mod(options[:module])
            return say "ERROR: Could not find module '#{options[:module]}'" if mod.nil?

            printer.print(mod)
          else
            printer.print(context)
          end
        end
      end
    end
  end
end
