# frozen_string_literal: true

module Lume
  class Analyzer
    class Pass
      # Resolves variable references and the declarations within the scope they're referenced.
      #
      # This pass binds variable references to their declarations, so code such as this:
      #
      #   let a = 1
      #   let b = a
      #
      # The expression `let a = 1` declares a new variable `a` and assigns it the value `1`. When the
      # expression `let b = a` is visited, it needs to know which variable is being referenced, when seeing `a`.
      #
      # Since this pass needs to contain a lookup table of variables, it also needs to know which scope each
      # variable is declared in. Because of this, this pass also inherently handles invalid variable references,
      # such as referencing a variable that has not been declared or variables which are out of scope. For example:
      #
      #   let a = b
      #
      # Would fail because `b` has not been declared yet. Similarly:
      #
      #   fn test()
      #     b = 1
      #   end
      #
      #   test()
      #
      #   let a = b
      #
      # Would fail because `b` is out of scope when `a` is declared, even though `b` was defined within `test`,
      # which was called before `a` was declared.
      class Scoping < Pass
        include Lume::Analyzer::IR

        def initialize(analyzer)
          super

          @symbols = []
        end

        # Pushes the given variable declaration into the symbol table.
        #
        # @param node [Lume::Analyzer::IR::VariableDeclaration] The variable declaration node to resolve.
        def accept_variable_declaration(node)
          add_symbol(node)
        end

        # Pushes the arguments of the given method call into the symbol table.
        #
        # @param node [Lume::Analyzer::IR::MethodCall] The method call node to resolve.
        def accept_method_call(node)
          # puts node.inspect
        end

        # Resolves the referenced variable from the current symbol scope.
        #
        # @param node [Lume::Analyzer::IR::Variable] The variable reference node to resolve.
        def accept_variable(node)
          # puts node.inspect
        end

        private

        # Pushes a new symbol scope with the given symbols into the symbol table.
        #
        # This is usually called when a function is invoked or block scope starts.
        #
        # @param symbols [Hash] The symbols to be defined in the new scope. Defaults to an empty hash.
        def push_frame(symbols = {})
          @symbols.push(symbols)
        end

        # Pops the current symbol scope from the symbol table.
        #
        # This is usually called when a function or block scope ends.
        def pop_frame
          @symbols.pop
        end

        # Appends a new variable declaration to the current symbol scope.
        #
        # This is usually called when a new variable is introduced within an existing block scope.
        #
        # @param declaration [Lume::Analyzer::IR::VariableDeclaration] The variable declaration to be added.
        def add_symbol(declaration)
          # If the symbol table is empty, push a new frame before adding the symbol.
          push_frame if @symbols.empty?

          # Push the declaration into the last symbol frame, keyed by its name.
          @symbols[-1][declaration.name] = declaration
        end

        # Retrieves a variable declaration from the current symbol scope, with the given name.
        #
        # @param name [String] The name of the variable to retrieve.
        #
        # @return [Lume::Analyzer::IR::VariableDeclaration]
        def retrieve_symbol(name)
          @symbols.reverse_each do |frame|
            return frame[name] if frame.key?(name)
          end

          nil
        end
      end
    end
  end
end
