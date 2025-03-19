# frozen_string_literal: true

module Lume
  class Analyzer
    module Errors
      # Defines that a method was referenced without being defined on the class instance:
      #
      #   class Foo { }
      #
      #   let a = new Foo()
      #   let b = a.bar()  # This will raise an UndefinedMethod error
      #
      # The error will specialize when encountering a missing operator overload:
      #
      #   class Foo { }
      #
      #   let a = new Foo()
      #   let b = a + 1  # This will raise an UndefinedMethod error, with a specialized error message
      class UndefinedMethod < Lume::LumeDiagnostic
        attr_reader :location

        # @param node [Lume::MIR::MethodCall] The invocation that triggered the error.
        # @param name [String, nil]           The name of the class instance.
        def initialize(node, name: nil)
          action = node.action
          name ||= node.class_instance_name

          message = if Lume::Parser::OPERATORS.include?(action.to_sym)
            "Operator '#{action}' cannot be applied to type '#{name}'."
          else
            "Method '#{action}' does not exist on the type '#{name}'."
          end

          super(message, type: Lume::ERROR)

          attach_location(node.location)
        end
      end
    end
  end
end
