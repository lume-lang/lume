# frozen_string_literal: true

module Lume
  class Analyzer
    module Errors
      # Defines that an instance method was called without an instance:
      #
      #   class MyClass
      #     public fn instance_method()
      #       print("Instance method called")
      #     end
      #   end
      #
      #   MyClass.instance_method # This will raise a InstanceMethodWithoutInstance error
      class InstanceMethodWithoutInstance < Lume::LumeDiagnostic
        attr_reader :location

        def initialize(node)
          message = "Method #{node.action} cannot be called without an instance. Qualify with an instance instead."

          super(message, type: Lume::ERROR)

          attach_location(node.location)
        end
      end
    end
  end
end
