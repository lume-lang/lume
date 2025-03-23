# frozen_string_literal: true

require 'lume/source'
require 'lume/lume_syntax/types'
require 'lume/lume_syntax/values'
require 'lume/lume_syntax/hir_helpers'
require 'lume/lume_import/importer'
require 'lume/lume_parser/errors'
require 'lume/lume_parser/module'
require 'lume/lume_parser/types'
require 'lume/lume_parser/values'

module Lume
  class Parser # :nodoc:
    include Lume::Syntax

    attr_reader :source, :tokens

    # @param source [Lume::SourceFile]             The source code to parse.
    # @param tokens [Array<Lume::Language::Token>] The tokens to use for parsing.
    def initialize(source, tokens)
      @source = source
      @filename = source.path
      @lines = @source.split("\n")

      @tokens = tokens
      @index = 0

      @token = @tokens[@index]
    end

    # Creates a new parser instance with the given source code and pre-lexed tokens.
    #
    # @param source   [Lume::SourceFile]             The source code to parse.
    # @param tokens   [Array<Lume::Language::Token>] The tokens to use for parsing.
    #
    # @return [Lume::Language::Parser]             The new parser instance.
    def self.with_tokens(source, tokens)
      new(source, tokens)
    end

    # Creates a new parser instance with the given source code.
    #
    # @param source   [Lume::SourceFile]  The source code to parse.
    #
    # @return [Lume::Language::Parser]    The new parser instance.
    def self.with_source(source)
      source = SourceFile.new(nil, source) if source.is_a?(String)
      tokens = Lexer.new(source).all!(include_comments: true)

      new(source, tokens)
    end

    # Parse the entire source code, given in the constructor, as a set of modules.
    #
    # @param name     [String, nil] The name of the entry module, if any. Defaults to `nil`.
    #
    # @return [Array<Lume::Parser::Module>] The parsed modules from the source code.
    def parse(name: nil)
      entry_hir = parse_module
      importer = Importer.import!(name, entry_hir)

      # Create a new module for each of the imported files
      modules = importer.imported_files.map do |mod_name, hir|
        source = SourceFile.new(mod_name, importer.sources[mod_name])

        Module.new(mod_name, hir, source: source)
      end

      # Add the entry module into the tree as well.
      modules.insert(0, Module.new(name, entry_hir, source: @source.content))

      # Map all the dependencies between modules.
      map_module_dependencies(modules, importer.dependencies)

      modules
    end

    # Parse the entire source code, given in the constructor, as a single module.
    #
    # @return [Lume::Syntax::AST] The parsed HIR AST from the source code.
    def parse_module
      nodes = parse_statements

      AST.new(nodes)
    end

    TOP_LEVEL_TYPES = %i[
      class
      fn
      type
      enum
      import
    ].freeze

    CONTROL_TYPES = %i[
      return
      if
      unless
    ].freeze

    STATEMENT_TYPES = [
      *CONTROL_TYPES,
      :let,
      :const
    ].freeze

    VISIBILITY_MODIFIERS = %i[
      public
      private
      static
    ].freeze

    OPERATORS = %i[
      +
      -
      *
      /
      ++
      --
      +=
      -=
      *=
      /=
      !=
      ==
      =
      <
      >
      <=
      >=
    ].freeze

    FUNCTIONS_NAME_TYPES = [
      :name,
      *OPERATORS
    ].freeze

    OPERATOR_PRECEDENCE = {
      '=': 1,
      '+=': 1,
      '-=': 1,
      '*=': 1,
      '/=': 1,
      '==': 3,
      '!=': 3,
      '<': 4,
      '>': 4,
      '<=': 4,
      '>=': 4,
      '+': 5,
      '-': 5,
      '*': 6,
      '/': 6,
      '++': 7,
      '--': 7,
      '.': 9
    }.freeze

    NUMERIC_TYPE_MAP = {
      'i8' => ByteLiteral,
      'u8' => UnsignedByteLiteral,
      'i16' => ShortLiteral,
      'u16' => UnsignedShortLiteral,
      'i32' => WordLiteral,
      'u32' => UnsignedWordLiteral,
      'i64' => LongLiteral,
      'u64' => UnsignedLongLiteral,
      'f32' => FloatLiteral,
      'f64' => DoubleLiteral
    }.freeze

    NUMERIC_LITERAL_TYPES = [
      ByteLiteral,
      UnsignedByteLiteral,
      ShortLiteral,
      UnsignedShortLiteral,
      WordLiteral,
      UnsignedWordLiteral,
      LongLiteral,
      UnsignedLongLiteral,
      FloatLiteral,
      DoubleLiteral
    ].freeze

    private

    # Maps all the dependencies of the given modules.
    #
    # @param modules      [Array<Module>]               The modules to map dependencies for.
    # @param dependencies [Hash<String, Array<String>>] The dependencies to map.
    def map_module_dependencies(modules, dependencies)
      dependencies.each_pair do |name, subdeps|
        mod = modules.find { |m| m.name == name }
        mod.dependencies = subdeps.map { |dep| modules.find { |m| m.name == dep } }
      end
    end

    # Peeks the current token's type or value.
    #
    # @param value [String|Symbol|Array<String|Symbol>] The type or value to peek for.
    #
    # @return [Boolean] `true` if the current token's type or value matches the given value, `false` otherwise.
    def peek(value, offset: 0)
      value = [value] if value.is_a?(Symbol) || value.is_a?(String)
      value = value.map(&:to_s)

      token = @tokens[@index + offset]

      value.include?(token.type.to_s) || value.include?(token.value.to_s)
    end

    # Peeks the next token's type.
    #
    # @param types [Array<String|Symbol>] The types to peek for.
    #
    # @return [Boolean] `true` if the current token's type matches the given type, `false` otherwise.
    def peek_next(types)
      types.each_with_index { |type, index| return false unless peek(type, offset: index) }

      true
    end

    # Asserts that the current token's type matches the given type(s).
    #
    # @param type [Array<String>|String] The type(s) to expect.
    def expect(type, error: nil)
      type = [type] if type.is_a?(Symbol) || type.is_a?(String)
      type = type.map(&:to_s)

      unexpected_token(type, message: error) unless type.include?(@token.type.to_s)
    end

    # Asserts that the current token's value matches the given value(s).
    #
    # @param type [Array<String>|String] The value(s) to expect.
    def expect_value(value, error: nil)
      value = [value] if value.is_a?(Symbol) || value.is_a?(String)
      value = value.map(&:to_s)

      unexpected_token(value, message: error) unless value.include?(@token.value.to_s)
    end

    # Advances the parser by the given count.
    #
    # @param count [Integer] The number of tokens to advance.
    def skip!(count: 1)
      count.times do
        @index += 1
        @token = @tokens[@index]
      end
    end

    # Consumes the current token and advances the parser by a single token.
    #
    # @param type   [String]  If not nil, asserts that the current token's type matches the given type.
    # @param value  [String]  If not nil, asserts that the current token's value matches the given value.
    # @param error  [String]  If not nil, raises an error with the given message if the assertion fails.
    #
    # @return       [Token]   The consumed token.
    def consume!(type: nil, value: nil, error: nil)
      expect(type, error: error) unless type.nil?
      expect_value(value, error: error) unless value.nil?

      token = @token
      skip!

      token
    end

    # Consumes the current token, if it matches the given type and/or value.
    #
    # @param type   [String]  If not nil, checks that the current token's type matches the given type.
    # @param value  [String]  If not nil, checks that the current token's value matches the given value.
    #
    # @return       [nil|Token]   The consumed token, if the token was consumed. Otherwise, returns `nil`.
    def consume(type: nil, value: nil)
      consume!(type: type, value: value)
    rescue UnexpectedTokenError
      nil
    end

    # Consumes the token returned in the given block, wrapped with the given delimiters.
    #
    # @param left   [String]  The left delimiter to consume.
    # @param right  [String]  The right delimiter to consume.
    # @param block  [Proc]    Anonymous block to consume the next token.
    #
    # @return       [Token]   The consumed token.
    def consume_wrapped!(left: :'{', right: :'}', &block)
      consume!(value: left, error: 'Expected opening delimiter')
      token = block.call
      consume!(value: right, error: 'Expected closing delimiter')

      token
    end

    # Iterates over the given parser method or block until it returns `nil`.
    #
    # @param parser [String|Proc] The parser method or block to iterate over.
    # @param block  [Proc]        Anonymous block to consume the next token.
    #
    # @return       [Array]       The array of consumed tokens.
    #
    # @example Parsing an argument list
    #   iterate_all! do |index|
    #     # If the next token is a comma, there's still more arguments
    #     next nil if index.positive? && !peek(:',')
    #
    #     parse_argument
    #   end
    def iterate_all!(parser = nil, &block)
      parser = method(parser).to_proc unless parser.nil?
      parser = block if block_given?

      nodes = []

      loop do
        node = parser.call(nodes.length)
        return nodes if node.nil?

        nodes << node
      end

      nodes
    end

    # Returns the location of the given token.
    #
    # @param token [Token] The token to get the location of.
    #
    # @return [Lume::Location] The location of the token.
    def location_of(token)
      Lume::Location.new(token.start..token.end, file: @source)
    end

    # Calls the given block and applies the location span to it's result.
    #
    # @return [Node] The node returned from the block.
    def with_location
      raise 'Block required' unless block_given?

      start = @token.start
      result = yield
      stop = @token.end

      # Set the location of the result, only if it's a node
      result.location ||= Lume::Location.new(start..stop, file: @source) if result.is_a?(Node)

      result
    end

    # Raises an unexpected token error.
    #
    # @param expected   [String, nil] The expected token type.
    # @param message    [String, nil] An optional error message to pass.
    def unexpected_token(expected = nil, message: nil)
      token = @token
      location = location_of(token)

      message ||= if expected.nil?
        "Unexpected token '#{token.value}'"
      else
        "Expected token '#{expected}', but found '#{token.value}'"
      end

      raise UnexpectedTokenError.new(message, expected, token, location)
    end

    # Gets the precedence of the given token.
    #
    # @param token   [Token] The token to get the precedence of.
    #
    # @return [Integer] The precedence of the token.
    def precedence_of(token)
      return 0 unless OPERATOR_PRECEDENCE.key?(token.type)

      OPERATOR_PRECEDENCE[token.type]
    end

    # Parses the statement at the current cursor position.
    #
    # @return [Node] The parsed statement.
    def parse_statement
      # If we've reached the last token, return `nil` so `iterate_all!` can return.
      return nil if peek(:eof)

      # If the token is a top-level statement token, parse it as such.
      return with_location { parse_top_level_statement } if peek(TOP_LEVEL_TYPES)

      # If the token is a statement token, parse it as such.
      return with_location { parse_statement_expression } if peek(STATEMENT_TYPES)

      # If the current token isn't a statement token, parse it as an expression.
      parse_expression
    end

    # Parses a list of statements within the top-level namespace, function or method.
    #
    # @return [Array<Node>] The parsed statements.
    def parse_statements
      iterate_all! { parse_statement }
    end

    # Parses the top-level statement at the current cursor position.
    #
    # @return [Node] The parsed statement.
    def parse_top_level_statement
      # If the statement is 'import', parse it as an import expression
      return parse_import_expression if peek(:import)

      # If the statement is a class definition, parse it as a class definition
      return parse_class_definition if peek(:class)

      # If the statement is a method definition, parse it as a method definition
      return parse_method_definition if peek(:fn)

      # If the next token is `type` or `enum`, it might be a type definition
      return parse_type_definition if peek(%i[enum type])

      unexpected_token(TOP_LEVEL_TYPES)
    end

    # Parses the statement expression at the current cursor position.
    #
    # @return [Node] The parsed expression.
    def parse_statement_expression
      # If the statement starts with `let` or `const`, parse it as a variable declaration
      return parse_variable_declaration if peek(%i[let const])

      # If the statement starts with 'if' or 'until', parse it as a condition
      return parse_conditional_expression if peek(%i[if until])

      # If the consumed token is a control token, parse it as a control statement
      return parse_control_expression if peek(CONTROL_TYPES)

      # If no statement was parsed, parse an expression
      parse_expression
    end

    # Parses the expression at the current cursor position.
    #
    # @return [Expression] The parsed expression.
    def parse_expression(precedence: 0)
      return nil if peek(:eof)

      with_location do
        left = parse_prefix_expression
        left = parse_infix_expression(left) while precedence < precedence_of(@token)

        left
      end
    end

    # Parses a list of expressions within the top-level namespace, function or method.
    #
    # @return [Array<Expression>] The parsed expressions.
    def parse_expressions
      iterate_all! { parse_expression }
    end

    # Parses a prefix expression at the current cursor position.
    #
    # Prefix expressions are expressions which appear at the start of an expression,
    # such as literals of prefix operators. In Pratt Parsing, this is also called "Nud" or "Null Denotation".
    #
    # @return [Expression] The parsed expression.
    def parse_prefix_expression
      return nil if peek(:eof)

      # If the next token is a comment, parse the comment block.
      return parse_comment if peek(:comment)

      # If the next expression is nested within parentheses, descend into it parse it again.
      return parse_nested_expression if nested_expression?

      # If the expression is a new-token, parse it as an object initialization.
      return parse_object_initialization if peek(:new)

      # If the expression is a name-token, parse it as a named expression
      return parse_named_expression if peek(:name)

      # If the expression is a literal-token, parse it as a literal expression
      return parse_literal_expression if literal?

      # If the next token is a unary operator, parse it as a unary expression
      return parse_unary_expression if unary?

      unexpected_token
    end

    # Parses a infix expression at the current cursor position.
    #
    # Infix expressions are expressions which appear in the middle of an expression,
    # such as infix of postfix operators. In Pratt Parsing, this is also called "Led" or "Left Denotation".
    #
    # @param left   [Expression]  The left-hand side of the infix expression.
    #
    # @return [Expression] The parsed expression.
    def parse_infix_expression(left)
      # If the next token is a '.', it's a chained expression and we should parse it as a member access expression.
      #
      # In essence, this statement is handling cases where:
      #
      #   let c = new Foo().bar()
      #
      # would be parsed as an operator expression, such as `c = Call(New('Foo'), '.', [FunctionCall('bar')])`
      return parse_member_access(target: left) if peek(:'.')

      token = consume!(type: OPERATORS)
      right = parse_expression(precedence: precedence_of(token))

      left.call(token.value, right)
    end

    # Parses the comment block at the current cursor position.
    #
    # @return [Comment] The parsed comment block.
    def parse_comment
      comments = iterate_all! do |_token|
        next consume(type: :comment)&.value
      end

      content = comments.join("\n")
      comment = Comment.new(content)

      statement = parse_statement
      statement.comment = comment if statement.is_a?(Expression)

      statement
    end

    # Parses a single import expression.
    #
    # @return [Import] The parsed import expression.
    def parse_import_expression
      # Consume the 'import' token
      consume!(type: :import)

      library = consume!(type: :string, error: 'Expected library name after import statement').value

      Import.new(library)
    end

    # Parses a list of parameter definitions. These are used in function- and method-definitions.
    #
    # @return [Array<Parameter>] The parsed parameter definitions.
    def parse_parameters
      consume_wrapped!(left: :'(', right: :')') do
        parameters = []

        loop do
          definition = with_location { parse_parameter }
          parameters << definition unless definition.nil?

          break unless peek(:',')

          skip!
        end

        parameters
      end
    end

    # Parses a single parameter definition. These are used in function- and method-definitions.
    #
    # @return [Parameter] The parsed parameter definition.
    def parse_parameter
      return nil unless peek(:name)

      name = consume!(type: :name).value
      consume!(type: :':', error: 'Expected colon between parameter name and type')

      Parameter.new(name, parse_type)
    end

    # Parses a list of argument values. These are used in function- and method-invocations.
    #
    # @return [Array<Argument>] The parsed argument values.
    def parse_arguments
      consume_wrapped!(left: :'(', right: :')') do
        arguments = []

        break arguments if consume(type: :')')

        loop do
          arguments << parse_argument

          break unless consume(type: :',')
        end

        arguments
      end
    end

    # Parses a single argument value. This is used in function- and method-invocations.
    #
    # @return [Argument] The parsed argument value.
    def parse_argument
      name = nil
      value = parse_expression

      if consume(type: :':')
        name = value.name
        value = parse_expression
      end

      Argument.new(name, value)
    end

    # Parses a nested expression. A nested expression is an expression that is wrapped in parentheses.
    #
    # @return [Expression] The parsed sub-expression.
    def parse_nested_expression
      consume_wrapped!(left: :'(', right: :')') { parse_expression }
    end

    # Parses an expression that creates a new instance of a class.
    #
    # @return [New] The parsed sub-expression.
    def parse_object_initialization
      # Consume the `new` token
      consume!(type: :new)

      name = consume!(type: :name).value
      arguments = parse_arguments

      New.new(name, *arguments)
    end

    # Parses an expression that references a named method, function or variable.
    #
    # @return [Expression] The parsed expression.
    def parse_named_expression
      target = consume!(type: :name)

      # If the next token is a left parenthesis, parse it as a function invocation
      return parse_function_invocation(action: target) if peek(:'(')

      # If the next token is a dot, parse it as some form of member access (method invocation, property access, etc.)
      return parse_member_access(target: target) if peek(:'.')

      # If the next token is an equal sign, parse it as an assignment
      return parse_assignment(target: target) if peek(:'=')

      # If the name stands alone, parse it as a variable reference
      parse_variable_reference(name: target)
    end

    # Parses a single control expression.
    #
    # @return [Expression] The parsed control expression.
    def parse_control_expression
      # If the consumed token is a return token, parse it as a return expression
      return parse_return if peek(:return)

      # If the consumed token is a conditional token, parse it as a conditional expression
      return parse_conditional_expression if peek(%i[if unless])

      unexpected_token(CONTROL_TYPES)
    end

    # Parses a single return expression.
    #
    # @return [Return] The parsed function invocation.
    def parse_return
      # Consume the return token
      consume!(type: :return)

      expression = with_location { parse_expression }

      Return.new(expression)
    end

    # Parses a single function invocation expression.
    #
    # @param action [Expression|Token] The action to be invoked.
    #
    # @return [Call] The parsed function invocation.
    def parse_function_invocation(action:)
      Call.new(nil, action.value, *parse_arguments)
    end

    # Parses a single member access expression.
    #
    # @param target [Expression|Token] The target object.
    #
    # @return [Expression] The parsed member access.
    def parse_member_access(target:)
      target = target.value if target.is_a?(Token)

      # Consume dot symbol
      consume(type: :'.')
      name = consume!(type: %i[name]).value

      # If the next token is an opening parenthesis, it's a method invocation
      return Call.new(target, name, *parse_arguments) if peek(:'(')

      expression = MemberAccess.new(target, name)

      # If there is yet another dot, it's part of a longer expression.
      return parse_member_access(target: expression) if peek(:'.')

      # Otherwise, return the expression, as-is.
      expression
    end

    # Parses a single method invocation expression.
    #
    # @param target [Expression|Token] The target object.
    #
    # @return [Call] The parsed method invocation.
    def parse_method_invocation(target:)
      target = target.value if target.is_a?(Token)

      # Consume dot symbol
      consume(type: :'.')

      name = consume!(type: %i[name]).value

      Call.new(target, name, parse_arguments)
    end

    # Parses a single variable declaration expression.
    #
    # @return [VariableDeclaration] The parsed variable declaration.
    def parse_variable_declaration
      is_const = consume!(type: %i[let const]).value == 'const'
      name = consume!(type: :name).value
      type = nil

      # If the next token is a colon, a type is explicitly specified
      type = with_location { parse_type } if consume(type: :':')

      expression = VariableDeclaration.new(name, type, const: is_const)

      # If no type was specified, we require there to be a value specified.
      if type.nil? && !peek(:'=')
        unexpected_token('=', message: 'Expected variable assignment since no type was specified')
      end

      expression.value = with_location { parse_expression } if consume(type: :'=')

      expression
    end

    # Parses a single literal value expression.
    #
    # @return [Literal] The parsed literal value.
    def parse_literal_expression
      parse_value
    end

    # Parses a single unary expression.
    #
    # @return [Expression] The parsed unary expression.
    def parse_unary_expression
      operator = consume!(type: %i[-])
      right = parse_expression(precedence: 3)

      # As a quality of life feature, we can apply the unary operator directly to the expression,
      # if it can be done at parsing time. If not, we create an ordinary unary expression.
      apply_unary_operator(operator, right) || Unary.new(operator.value, right)
    end

    # Applies a unary operator to an expression, if possible given the combination.
    #
    # @param operator [Token]       The unary operator token.
    # @param right    [Expression]  The right-hand side expression.
    #
    # @return [nil|Expression] The resulting expression after applying the unary operator.
    def apply_unary_operator(operator, right)
      # If the operator is a unary minus and the right-hand side is a number literal, we can negate
      # the value directly.
      return right.class.new(-right.value) if operator.type == :- && right.is_a?(NumberLiteral)

      nil
    end

    # Parses a single variable reference expression.
    #
    # @return [VariableReference] The parsed variable reference.
    def parse_variable_reference(name: nil)
      name ||= consume!(type: :name)

      VariableReference.new(name.value)
    end

    # Parses a single assignment expression.
    #
    # @return [Assignment] The parsed assignment.
    def parse_assignment(target: nil)
      target ||= parse_expression

      # If the target is a token, get it's value for the next conditional.
      target = target.value if target.is_a?(Token)

      # If the target is a string, convert it into an actual variable reference.
      target = target.var if target.is_a?(String)

      # Skip assignment operator
      skip!

      Assignment.new(target, parse_expression)
    end

    # Parses a single class definition.
    #
    # @return [MethodDefinition] The parsed method definition.
    #
    # @see MethodDefinition
    def parse_class_definition
      consume!(value: :class)

      # Determines whether the class is declared as builtin.
      builtin = !consume(value: :builtin).nil?

      name = consume!(type: :name, error: 'Expected class name in class definition').value
      definitions = parse_member_definitions

      ClassDefinition.new(name, definitions, builtin: builtin)
    end

    # Parses a list of member definitions.
    #
    # @return [Array<Expression>] The parsed member definitions.
    def parse_member_definitions
      consume_wrapped! do
        iterate_all! do
          with_location { parse_member_definition }
        end
      end
    end

    # Parses a single member definition.
    #
    # @return [Expression] The parsed member definition.
    def parse_member_definition
      # If a visibility modifier is present, parse it and set it on the following member definition.
      if peek(VISIBILITY_MODIFIERS)
        visibility = parse_visibility_modifiers

        member = parse_member_definition
        member.visibility = visibility

        return member
      end

      # If the current token is a `fn` keyword, it's a method declaration.
      return parse_method_definition if peek(:fn)

      # If the current token is a `name` keyword, it's likely a property definition.
      return parse_property_definition if peek(:name)

      nil
    end

    # Parses a single class property definition.
    #
    # @return [Property] The parsed property definition.
    def parse_property_definition
      name = consume!(type: :name, error: 'Expected property name').value
      type = nil
      default = nil

      # If the next token is a colon, an explicit type is specified.
      type = parse_type if consume(type: :':')

      # If the next token is an equal sign, a default value is specified.
      default = parse_expression if consume(type: :'=')

      Property.new(name, type: type, default: default)
    end

    # Parses a single method definition.
    #
    # @return [MethodDefinition] The parsed method definition.
    #
    # @see MethodDefinition
    def parse_method_definition
      consume!(value: :fn)

      is_external = !consume(type: :external).nil?

      name = consume!(type: FUNCTIONS_NAME_TYPES, error: 'Expected method name in signature').value

      parameters = parse_parameters
      return_type = with_location { parse_return_type }

      # If the method is external, it doesn't have a body nor an end token.
      expressions = parse_function_content unless is_external

      expression = MethodDefinition.new(name, parameters, return_type, expressions || [])
      expression.external = is_external

      expression
    end

    # Parses the content within a function- or method-definition.
    #
    # @return [Array<Expression>] The parsed function- or method-definition content.
    def parse_function_content
      consume_wrapped! do
        iterate_all! do
          next nil if peek(:'}')

          parse_statement
        end
      end
    end

    # Parses zero-or-more visibility modifiers.
    #
    # @return [Array<Visibility>] The parsed visibility modifiers.
    def parse_visibility_modifiers
      iterate_all! do
        next nil unless peek(VISIBILITY_MODIFIERS)

        with_location do
          modifier = consume!(type: VISIBILITY_MODIFIERS).value

          Visibility.new(modifier)
        end
      end
    end

    # Parses a method return type.
    #
    # @return [Type] The parsed type.
    def parse_return_type
      # If no return type is specified, return `void` by default
      return Void.new unless consume(type: :'->')

      parse_type
    end

    # Parses a conditional expression.
    #
    # @return [Conditional] The parsed conditional expression.
    def parse_conditional_expression
      type = consume!(type: %i[if unless]).type

      case type
      when :if then parse_if_conditional_expression
      when :unless then parse_unless_conditional_expression
      end
    end

    # Parses a `if`-conditional expression.
    #
    # @return [IfConditional] The parsed conditional expression.
    def parse_if_conditional_expression
      conditional = IfConditional.new
      conditional.condition = parse_expression
      conditional.then = parse_conditional_block
      conditional.else_if = parse_else_if_conditional_expression
      conditional.else = parse_else_conditional_expression

      conditional
    end

    # Parses a `unless`-conditional expression.
    #
    # @return [UnlessConditional] The parsed conditional expression.
    def parse_unless_conditional_expression
      conditional = UnlessConditional.new
      conditional.condition = parse_expression
      conditional.then = parse_conditional_block

      if peek(:else) && peek(:if, offset: 1)
        unexpected_token(:else, message: '`unless` conditions cannot have an `else if` clause')
      end

      conditional.else = parse_else_conditional_expression

      conditional
    end

    # Parses the block within a conditional expression.
    #
    # @return [Array<Node>] The statements within the block.
    def parse_conditional_block
      # If the next token is not an opening brace, parse it as an inline condition.
      return [parse_statement_expression] unless peek(:'{')

      # Consume the start of the block
      consume!(type: :'{')

      iterate_all! do
        # If we see `}`, the block is complete
        next nil if consume(type: :'}')

        # If we see `else`, we let the parent function handle the next block.
        next nil if peek(:else)

        next parse_statement_expression
      end
    end

    # Parses the block within an `else if` block of a conditional expression.
    #
    # @return [Array<ElseIfConditional>] The statements within the block.
    def parse_else_if_conditional_expression
      iterate_all! do
        # Unless we see `else` `if`, we let the parent function handle the next block.
        next nil unless peek(:else) && peek(:if, offset: 1)

        consume!(type: :else)
        consume!(type: :if)

        conditional = ElseIfConditional.new
        conditional.condition = parse_expression
        conditional.then = parse_conditional_block

        conditional
      end
    end

    # Parses the block within an `else` block of a conditional expression.
    #
    # @return [Array<Expression>] The statements within the block.
    def parse_else_conditional_expression
      # Unless we see `else`, we let the parent function handle the next block.
      return [] unless consume(type: :else)

      parse_conditional_block
    end

    # Determines whether the current token is a nested expression (contained within parentheses).
    #
    # @return [Boolean]
    def nested_expression?
      peek(:'(')
    end

    # Determines whether the current token is a literal expression.
    #
    # @return [Boolean]
    def literal?
      peek(%i[string number true false null])
    end

    # Determines whether the current token is a unary expression.
    #
    # @return [Boolean]
    def unary?
      peek(%i[-])
    end
  end
end
