# frozen_string_literal: true

require 'colorize'

require_relative 'ast'
require_relative 'document'
require_relative 'errors'
require_relative 'location'
require_relative 'token'

module Nox
  module Language
    class Parser # :nodoc:
      include Nox::Language

      attr_reader :source, :tokens
      attr_accessor :start, :end

      # @param source [String]                      The source code to parse.
      # @param tokens [Array<Nox::Language::Token>] The tokens to use for parsing.
      # @param filename [String]                    The filename of the source code.
      def initialize(source, tokens, filename: '<memory>')
        @source = source
        @filename = filename
        @lines = @source.split("\n")

        @tokens = tokens
        @index = 0

        @token = @tokens[@index]
      end

      # Creates a new parser instance with the given source code and pre-lexed tokens.
      #
      # @param source   [String]                      The source code to parse.
      # @param tokens   [Array<Nox::Language::Token>] The tokens to use for parsing.
      # @param filename [String]                      The filename of the source code.
      #
      # @return [Nox::Language::Parser]             The new parser instance.
      def self.with_tokens(source, tokens, filename: '<memory>')
        new(source, tokens, filename: filename)
      end

      # Creates a new parser instance with the given source code and pre-lexed tokens.
      #
      # @param source   [String]                    The source code to parse.
      # @param filename [String]                    The filename of the source code.
      #
      # @return [Nox::Language::Parser]             The new parser instance.
      def self.with_source(source, filename: '<memory>')
        new(source, Lexer.new(source).all!, filename: filename)
      end

      # Parse the entire source code, given in the constructor.
      #
      # @return [Nox::Language::AST] The parsed expression tree in the source code.
      def parse
        nodes = iterate_all! do
          next nil if peek(:eof)

          next parse_expression if peek(TOP_LEVEL_TYPES)

          unexpected_token(TOP_LEVEL_TYPES)
        end

        AST.new(nodes)
      end

      def render_error(error)
        puts %(#{'error'.red}: #{error.message.bold})

        render_error_section(error.location) if error.location.is_a?(Location)
      end

      private

      CONTROL_TYPES = %i[
        return
        if
        unless
      ].freeze

      EXPRESSION_TYPES = [
        *CONTROL_TYPES,
        :name,
        :string,
        :number,
        :true,
        :false,
        :nil,
        :lparen,
        :dash
      ].freeze

      TOP_LEVEL_TYPES = [
        *EXPRESSION_TYPES,
        :def,
        :class,
        :struct
      ].freeze

      CLASS_LEVEL_TYPES = [
        *EXPRESSION_TYPES,
        :def
      ].freeze

      OPERATORS = %i[
        plus
        dash
        asterisk
        slash
      ].freeze

      OPERATOR_PRECEDENCE = {
        '+': 1,
        '-': 2,
        '*': 3,
        '/': 4
      }.freeze

      # Peeks the current token's type.
      #
      # @param type [String|Symbol|Array<String|Symbol>] The type to peek for.
      #
      # @return [Boolean] `true` if the current token's type matches the given type, `false` otherwise.
      def peek(type, offset: 0)
        type = [type] if type.is_a?(Symbol) || type.is_a?(String)
        type = type.map(&:to_s)

        token = @tokens[@index + offset]
        type.include?(token.type.to_s)
      end

      # Peeks the current token's value.
      #
      # @param value [String|Symbol|Array<String|Symbol>] The value to peek for.
      #
      # @return [Boolean] `true` if the current token's value matches the given value, `false` otherwise.
      def peek_value(value, offset: 0)
        value = [value] if value.is_a?(Symbol) || value.is_a?(String)
        value = value.map(&:to_s)

        token = @tokens[@index + offset]
        value.include?(token.value.to_s)
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
      # @return [Location] The location of the token.
      def location_of(token)
        line = 1
        column = 1

        (0...token.start).each do |idx|
          column += 1

          if @source[idx] == "\n"
            line += 1
            column = 1
          end
        end

        Location.new(line: line, column: column, file: @filename)
      end

      # Returns the line of the given line number.
      #
      # @param line [Integer] The line number to get the line of.
      #
      # @return [String] The line of the given line number.
      def line(line)
        @lines[line]
      end

      # Returns the lines of the given line numbers.
      #
      # @param lines [Array<Integer>] The line numbers to get the lines of.
      #
      # @return [Array<String>] The lines of the given line numbers.
      def lines(lines)
        lines = [lines] unless lines.is_a?(Array)
        lines.map { |l| line(l) }
      end

      # Raises an unexpected token error.
      #
      # @param expected   [String] The expected token type.
      # @param message    [String] An optional error message to pass.
      def unexpected_token(expected, message: nil)
        raise UnexpectedTokenError.new(expected, @token, location_of(@token), message)
      end

      # Renders an error section with the given token location to the console.
      #
      # @param location [Location] The location to render the error section at.
      def render_error_section(location)
        highlighted_line = ''.rjust(location.column - 1) + '^'.red

        render_code_section('')
        render_code_section(location.line)
        render_code_section('', content: highlighted_line)
      end

      # Renders a code section with the given line number and content to the console.
      #
      # @param line     [Integer] The line number to render.
      # @param content  [String]  The content to render.
      def render_code_section(line, content: nil)
        content ||= line(line - 1) if line.is_a?(Integer)

        puts "#{line.to_s.rjust(4)} | #{content}"
      end

      # Parses a list of argument definitions. These are used in function- and method-definitions.
      #
      # @return [Array<ArgumentDefinition>] The parsed argument definitions.
      def parse_argument_definitions
        consume_wrapped!(left: :'(', right: :')') do
          arguments = []

          loop do
            definition = parse_argument_definition
            arguments << definition unless definition.nil?

            break unless peek(:comma)

            skip!
          end

          arguments
        end
      end

      # Parses a single argument definition. These are used in function- and method-definitions.
      #
      # @return [ArgumentDefinition] The parsed argument definition.
      def parse_argument_definition
        return nil unless peek(:name)

        name = consume!(type: :name).value
        consume!(type: :colon, error: 'Expected colon between argument name and type')

        type = parse_type

        definition = ArgumentDefinition.new
        definition.name = name
        definition.type = type

        definition
      end

      # Parses a list of argument values. These are used in function- and method-invocations.
      #
      # @return [Array<Expression>] The parsed argument values.
      def parse_arguments
        consume_wrapped!(left: :'(', right: :')') { parse_values }
      end

      # Parses a list of expressions within the top-level namespace, function or method.
      #
      # @return [Array<Expression>] The parsed expressions.
      def parse_expressions
        iterate_all! do
          next nil if peek(:eof)
          next nil unless peek(EXPRESSION_TYPES)

          parse_expression
        end
      end

      # Parses a single expression within the top-level namespace, function or method.
      #
      # @return [Expression] The parsed expression.
      def parse_expression
        expression = parse_subexpression

        # If the expression is followed by an operator, nest it within a new operator expression
        return parse_operator_expression(left: expression) if peek(OPERATORS)

        # If the expression is followed by an opening parenthesis, nest it within a new invocation expression
        return parse_function_invocation(action: expression) if peek(:lparen)

        expression
      end

      # Parses a subexpression within the top-level namespace, function or method.
      #
      # @return [Expression] The parsed sub-expression.
      def parse_subexpression
        # If the expression is wrapped in parentheses, parse it as a nested expression
        return parse_nested_expression if nested_expression?

        # If the expression contains a literal, parse it as a literal expression
        return parse_literal_expression if literal?

        # If the expression is a dash, parse it as a negative subexpression
        return parse_negative_expression if peek(:dash)

        # If the expression is a struct definition, parse it as a struct definition
        return parse_struct_definition if peek(:struct)

        # If the expression is a class definition, parse it as a class definition
        return parse_class_definition if peek(:class)

        # If the expression is a method definition, parse it as a method definition
        return parse_method_definition if peek(:def)

        # If the expression starts with a name, parse it as a named expression
        return parse_named_expression if peek(:name)

        # If the consumed token is a control token, parse it as a control expression
        return parse_control_expression if peek(CONTROL_TYPES)

        unexpected_token(EXPRESSION_TYPES)
      end

      # Parses a nested expression. A nested expression is an expression that is wrapped in parentheses.
      #
      # @return [Expression] The parsed sub-expression.
      def parse_nested_expression
        consume_wrapped!(left: :'(', right: :')') { parse_expression }
      end

      # Parses an expression that references a named method, function or variable.
      #
      # @return [Expression] The parsed expression.
      def parse_named_expression
        target = consume!(type: :name)

        # If the next token is a left parenthesis, parse it as a function invocation
        return parse_function_invocation(action: target) if peek(:lparen)

        # If the next token is a dot, parse it as a method invocation
        return parse_method_invocation(target: target) if peek(:dot)

        # If the next token is a colon, parse it as a variable declaration
        return parse_variable_declaration(name: target) if peek(:colon)

        # If the next token is an equal sign, parse it as an assignment
        return parse_assignment(target: target) if peek(:equal)

        # If the next token is an operator, parse it as an operator expression
        return parse_operator_expression(left: target) if peek(OPERATORS)

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

        expression = Return.new
        expression.value = parse_expression

        expression
      end

      # Parses a conditional expression.
      #
      # @return [Conditional] The parsed conditional expression.
      def parse_conditional_expression
        # Consume the conditional token
        type = consume!(type: %i[if unless]).type

        return parse_if_condition_expression if type == :if
        return parse_unless_condition_expression if type == :unless

        unexpected_token(%i[if unless])
      end

      # Parses an `if`-conditional expression.
      #
      # @return [IfConditional] The parsed conditional expression.
      def parse_if_condition_expression
        expression = IfConditional.new
        expression.condition = parse_expression
        expression.then = parse_expressions
        expression.else_if << parse_else_if_condition_expression while peek_next(%i[else if])
        expression.else = parse_expressions if consume(:else)

        consume!(type: :end, error: 'Expected end of conditional statement')

        expression
      end

      # Parses an `else if`-conditional expression.
      #
      # @return [ElseIfConditional] The parsed conditional expression.
      def parse_else_if_condition_expression
        consume!(type: :else)
        consume!(type: :if)

        expression = ElseIfConditional.new
        expression.condition = parse_expression
        expression.then = parse_expressions

        expression
      end

      # Parses an `unless`-conditional expression.
      #
      # @return [UnlessConditional] The parsed conditional expression.
      def parse_unless_condition_expression
        expression = UnlessConditional.new
        expression.condition = parse_expression
        expression.then = parse_expressions
        expression.else = parse_expressions if consume(:else)

        consume!(type: :end, error: 'Expected end of conditional statement')

        expression
      end

      # Parses a single function invocation expression.
      #
      # @param action [Expression|Token] The action to be invoked.
      #
      # @return [Call] The parsed function invocation.
      def parse_function_invocation(action:)
        Call.new(nil, action.value, parse_arguments)
      end

      # Parses a single method invocation expression.
      #
      # @param target [Expression|Token] The target object.
      #
      # @return [Call] The parsed method invocation.
      def parse_method_invocation(target:)
        # Skip dot symbol
        skip!

        name = consume!(type: %i[name]).value

        Call.new(target.value, name, parse_arguments)
      end

      # Parses a single variable declaration expression.
      #
      # @param name [String] The name of the variable.
      #
      # @return [VariableDeclaration] The parsed variable declaration.
      def parse_variable_declaration(name:)
        # Skip colon symbol
        skip!

        expression = VariableDeclaration.new
        expression.name = name.value
        expression.type = parse_type

        if peek(:equal)
          skip!
          expression.value = parse_expression
        end

        expression
      end

      # Parses a single literal value expression.
      #
      # @return [Literal] The parsed literal value.
      def parse_literal_expression
        parse_value
      end

      # Parses a negative subexpression.
      #
      # @return [Call] The parsed literal value.
      def parse_negative_expression
        # Consume the negative symbol
        consume!(type: :dash)

        expression = parse_expression

        return Call.new(expression, :'-', -1.int32) unless expression.is_a?(NumberLiteral)

        # If the expression is a numeric literal, negate it's value directly.
        # This won't work if the returned expression is a sub-expression which results to a numeric literal.
        expression.value *= -1

        expression
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

      # Parses a single operator expression.
      #
      # @return [OperatorExpression] The parsed operator expression.
      def parse_operator_expression(left: nil, right: nil)
        left ||= parse_expression
        operator = consume!(type: OPERATORS).value
        right ||= parse_expression

        # If neither side of the expression is a sub-expression, parse them together in sequence.
        return left.call(operator, right) unless left.is_a?(Call) || right.is_a?(Call)

        left_precedence = parse_operator_precedence(left, operator)
        right_precedence = parse_operator_precedence(right, operator)

        # If the RHS has a higher precedence than the LHS, encase it within the left-hand expression.
        return left.call(operator, right) if left_precedence <= right_precedence

        right.first_arg.call(right.action, left.call(operator, right.target))
      end

      def parse_operator_precedence(expression, operator)
        operator = expression.action if expression.is_a?(Call)

        OPERATOR_PRECEDENCE[operator.to_sym] || 0
      end

      # Parses a single struct definition.
      #
      # @return [StructDefinition] The parsed struct definition.
      #
      # @see StructDefinition
      def parse_struct_definition
        consume!(value: :struct)
        name = consume!(type: :name, error: 'Expected struct name in struct definition').value

        expression = StructDefinition.new
        expression.name = name
        expression.expressions = parse_member_expressions

        consume!(type: :end, error: 'Expected \'end\' after struct definition')

        expression
      end

      # Parses a single class definition.
      #
      # @return [MethodDefinition] The parsed method definition.
      #
      # @see MethodDefinition
      def parse_class_definition
        consume!(value: :class)
        name = consume!(type: :name, error: 'Expected class name in class definition').value

        expression = ClassDefinition.new
        expression.name = name
        expression.expressions = parse_member_expressions

        consume!(type: :end, error: 'Expected \'end\' after class definition')

        expression
      end

      # Parses a list of expressions within the current class-level scope.
      #
      # @return [Array<Expression>] The parsed expressions.
      def parse_member_expressions
        iterate_all! do
          next nil if peek(:eof)
          next nil unless peek(CLASS_LEVEL_TYPES)

          parse_expression
        end
      end

      # Parses a single method definition.
      #
      # @return [MethodDefinition] The parsed method definition.
      #
      # @see MethodDefinition
      def parse_method_definition
        consume!(value: :def)
        name = consume!(type: :name, error: 'Expected method name in signature').value

        expression = MethodDefinition.new
        expression.name = name
        expression.arguments = parse_argument_definitions
        expression.return = parse_return_type

        expression.expressions = parse_expressions

        consume!(type: :end, error: 'Expected \'end\' after method definition')

        expression
      end

      # Parses a method return type.
      #
      # @return [Type] The parsed type.
      def parse_return_type
        return Void.new unless peek(:colon)

        consume!(type: :colon)

        parse_type
      end

      # Parses a single type definition expression.
      #
      # @return [TypeDefinition] The parsed type definition.
      def parse_type
        type = consume!(type: :name).value

        Scalar.new(type)
      end

      # Parses zero-or-more values from an expression, separated by commas.
      #
      # @return [Array<Expression>] The parsed values.
      def parse_values
        iterate_all! do |index|
          next nil if peek(:rparen)

          # We shouldn't break on the first iteration, as we haven't read any values yet.
          next nil if !peek(:comma) && index.positive?

          # If the next token is a comma, consume it.
          consume!(type: :comma) if peek(:comma) && index.positive?

          parse_expression
        end
      end

      # Parses a single value from an expression.
      #
      # @return [Literal] The parsed value.
      def parse_value
        return parse_string_value if peek(:string)
        return parse_number_value if peek(:number)
        return parse_boolean_value if peek(%i[true false])
        return parse_nil_value if peek(:nil)

        unexpected_token(%i[string number true false nil])
      end

      # Parses a single string value from an expression.
      #
      # @return [StringLiteral] The parsed string value.
      def parse_string_value
        value = consume!(type: :string).value

        StringLiteral.new(value)
      end

      # Parses a single number value from an expression.
      #
      # @return [NumberLiteral] The parsed number value.
      def parse_number_value
        value = consume!(type: :number).value

        return value.int8 if ByteLiteral.can_contain?(value)
        return value.uint8 if UnsignedByteLiteral.can_contain?(value)
        return value.int16 if ShortLiteral.can_contain?(value)
        return value.uint16 if UnsignedShortLiteral.can_contain?(value)
        return value.int32 if WordLiteral.can_contain?(value)
        return value.uint32 if UnsignedWordLiteral.can_contain?(value)
        return value.int64 if LongLiteral.can_contain?(value)
        return value.uint64 if UnsignedLongLiteral.can_contain?(value)
        return value.float32 if FloatLiteral.can_contain?(value)
        return value.float64 if DoubleLiteral.can_contain?(value)

        raise "Number out of range (#{value})"
      end

      # Parses a single boolean value from an expression.
      #
      # @return [BooleanLiteral] The parsed boolean value.
      def parse_boolean_value
        value = consume!(type: %i[true false]).value
        value = value.is_a?(String) && value.casecmp?('true')

        BooleanLiteral.new(value)
      end

      # Parses a single `nil` value from an expression.
      #
      # @return [NilLiteral] The parsed `nil` value.
      def parse_nil_value
        consume!(type: :nil).value

        NilLiteral.new
      end

      # Determines whether the current token is a nested expression (contained within parentheses).
      #
      # @return [Boolean]
      def nested_expression?
        peek(:lparen)
      end

      # Determines whether the current token is a literal expression.
      #
      # @return [Boolean]
      def literal?
        peek(%i[string number true false nil])
      end
    end
  end
end
