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
    class SymbolTable # :nodoc:
      def initialize
        @symbols = []
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
        @symbols.pop
      end

      # Appends a new named symbol to the current symbol scope.
      #
      # This is usually called when a new variable is introduced within an existing block scope.
      #
      # @param declaration  [Lume::IR::Node]  The corresponding IR node of the symbol.
      # @param name         [String]          Optional. The name of the symbol to be defined. Inferred if not given.
      # @param type         [Lume::IR::Type]  Optional. The type of the symbol to be defined.
      #
      # @param declaration [Lume::IR::Node] The IR node to be added as a symbol.
      def define(declaration, name: nil, type: nil)
        # If the symbol table is empty, push a new frame before adding the symbol.
        push_frame if @symbols.empty?

        # If no name was given directory, assume it resides as a `name` attribute.
        name ||= declaration.name if declaration.respond_to?(:name)

        # Report an error if the declaration was given without an actual name.
        raise ArgumentError, "Declaration has no name: #{declaration.inspect}" if name.nil?

        # If no type was given, default to the actual node
        type ||= declaration

        # Push the declaration into the last symbol frame, keyed by its name.
        @symbols[-1][name] = SymbolEntry.new(name, declaration, type)
      end

      # Retrieves a variable declaration from the current symbol scope, with the given name.
      #
      # @param name [String] The name of the variable to retrieve.
      #
      # @return [Lume::IR::Node]
      def retrieve(name, type: nil)
        frame = @symbols.reverse.first { |f| f.key?(name) }
        symbol = frame[name]

        # If the type was given, ensure that the symbol type matches. If not, return nil.
        return nil if !type.nil? && !symbol.type.is_a?(type)

        symbol.node
      end
    end
  end
end
