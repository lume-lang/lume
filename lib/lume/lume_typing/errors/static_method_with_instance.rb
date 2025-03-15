# frozen_string_literal: true

module Lume
  module Typing
    module Errors
      # Defines that a static method was called on an instance:
      #
      #   class MyClass
      #     public static fn static_method()
      #       print("Static method called")
      #     end
      #   end
      #
      #   instance = MyClass.new
      #   instance.static_method # This will raise a StaticMethodWithInstance error
      class StaticMethodWithInstance < Lume::LumeDiagnostic
        attr_reader :location

        def initialize(node)
          message = "Member #{node.action} cannot be called on an instance. Qualify with a type instead."

          super(message, type: Lume::ERROR)

          attach_location(node.location)
        end
      end
    end
  end
end
