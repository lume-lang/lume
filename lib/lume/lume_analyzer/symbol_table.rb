# frozen_string_literal: true

module Lume
  class Analyzer
    # Defines a single entry within the symbol table.
    #
    # Each symbol entry represents a node, which declares a symbol within the source. It can be variable declaration,
    # function definition or class definition. Each symbol also describes the following properties of the node:
    #   - `name`: The name of the symbol.
    #   - `type`: The type of the symbol, if applicable.
    #   - `node`: A reference to the underlying IR node.
    class SymbolEntry
      attr_accessor :name, :type, :node

      def initialize(name, type, node)
        @name = name
        @type = type
        @node = node
      end
    end

    # Represents a boundary between scopes, which cannot be crossed.
    #
    # Boundaries are only meant to be implemented when all symbols in the local scope should be hidden. This is most
    # often the case when calling a function or method, where the local scope is hidden.
    class Boundary; end

    # The symbol table keeps track of all symbols which are available within any given scope.
    #
    # This table binds variable references to their declarations, so code such as this:
    #
    #   let a = 1
    #   let b = a
    #
    # The expression `let a = 1` declares a new variable `a` and assigns it the value `1`. When the
    # expression `let b = a` is visited, it needs to know which variable is being referenced, when seeing `a`.
    #
    # Since this table needs to contain a lookup table of variables, it also needs to know which scope each
    # variable is declared in. Because of this, this table also inherently handles invalid variable references,
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
    class SymbolTable
      def initialize
        @symbols = []

        # The first frame functions as a global scope, so it should always be present.
        push_frame
      end

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
        # The last frame is the global scope and cannot be popped.
        return if @symbols.length == 1

        @symbols.pop
      end

      # Appends a new named symbol to the current symbol scope.
      #
      # This is usually called when a new variable is introduced within an existing block scope.
      #
      # @param declaration  [Lume::MIR::Node]  The corresponding IR node of the symbol.
      # @param name         [String]          Optional. The name of the symbol to be defined. Inferred if not given.
      # @param type         [Lume::MIR::Type]  Optional. The type of the symbol to be defined.
      #
      # @param declaration [Lume::MIR::Node] The IR node to be added as a symbol.
      def define(declaration, name: nil, type: nil)
        # If no name was given directory, assume it resides as a `name` attribute.
        name ||= declaration.name if declaration.respond_to?(:name)

        # Report an error if the declaration was given without an actual name.
        raise ArgumentError, "Declaration has no name: #{declaration.inspect}" if name.nil?

        # If no type was given, default to the actual node
        type ||= declaration

        # Push the declaration into the last symbol frame, keyed by its name.
        @symbols[-1][name] = SymbolEntry.new(name, type, declaration)
      end

      # Retrieves a declared symbol from the current symbol scope, with the given name.
      #
      # @param name [String] The name of the symbol to retrieve.
      # @param type [Array<String>, String, nil] The type(s) of the symbol to retrieve, if any.
      #
      # @return [Lume::MIR::Node]
      def retrieve(name, type: nil)
        symbol = retrieve_symbol(name)

        # Wrap the type in an array, if given
        type = [type] unless type.is_a?(Array) || type.nil?

        # If no symbol was found, return nil.
        return nil if symbol.nil?

        # If the type was given, ensure that the symbol type matches. If not, return nil.
        return nil if !type.nil? && !type.any? { |t| symbol.node.is_a?(t) }

        symbol.node
      end

      # Pushes a new symbol boundary onto the symbol stack.
      #
      # @return [void]
      def push_boundary
        @symbols.push(Boundary.new)

        # Push a new frame onto the stack, so new symbols can be registered.
        push_frame
      end

      # Pops the last symbol boundary from the symbol stack.
      #
      # If the last symbol is not a boundary, raise an error.
      #
      # @return [void]
      def pop_boundary
        # Pops the boundary frame off the frame stack.
        pop_frame

        last = @symbols.last
        raise StandardError, "Attempted to pop a non-boundary symbol: #{last}" unless last.is_a?(Boundary)

        @symbols.pop
      end

      private

      # Attempts to retrieve a symbol from the current symbol scope, with the given name. If not found before a
      # boundary, attempts to retrieve it from the global scope.
      #
      # This method will **not** cross scope boundaries.
      #
      # @param name [String] The name of the symbol to retrieve.
      #
      # @return [Lume::MIR::Node]
      def retrieve_symbol(name)
        frame = @symbols.reverse.find do |f|
          # If we hit a boundary, continue no further and return from the loop.
          break nil if f.is_a?(Boundary)

          f.key?(name)
        end

        # If a parent frame was found, try to retrieve the symbol from it.
        return frame[name] if !frame.nil? && frame.key?(name)

        # If no symbol was found within the frame, attempt to retrieve it from the global scope.
        retrieve_global(name)
      end

      # Attempts to retrieve a declaration from the global scope, with the given name.
      #
      # @param name [String] The name of the declaration to retrieve.
      #
      # @return [Lume::MIR::Node]
      def retrieve_global(name)
        @symbols.first[name]
      end
    end
  end
end
