# frozen_string_literal: true

module Lume
  module Typing
    # Defines a type error, which is returned when a type constraint fails.
    #
    # Type errors **are not exceptions** and are not meant to be raised or caught. They are meant
    # to report errors to the type checker, which can then relay them to the user.
    class TypeError < Lume::LumeDiagnostic
      attr_accessor :message, :location

      def initialize(message, location: nil)
        super(message, type: Lume::ERROR)

        @location = location
      end
    end

    # Defines a result of a constraint.
    #
    # Constraints can return either a successful result, with or without a cast suggestion or a failure.
    class ConstraintResult
      attr_reader :suggestion, :errors

      def initialize(success:, suggestion: nil, errors: [])
        @success = success
        @suggestion = suggestion
        @errors = errors
      end

      def self.success(suggestion = nil)
        new(success: true, suggestion: suggestion)
      end

      def self.failure(errors = [])
        new(success: false, errors: errors)
      end

      # Determines whether the constraint was solved.
      #
      # @return [Boolean] `true` if the constraint was solved, `false` otherwise.
      def success?
        @success
      end

      # Determines whether the constraint failed.
      #
      # @return [Boolean] `true` if the constraint failed, `false` otherwise.
      def failure?
        !success?
      end

      # Determines whether the constraint returned a suggestion.
      #
      # @return [Boolean] `true` if the constraint returned a suggestion, `false` otherwise.
      def suggestion?
        !@suggestion.nil?
      end
    end

    # Defines an abstract type constraint, which limits which values can be assigned to a given type.
    #
    # A constraint is a condition that must be satisfied by a type in order for it to be considered valid. Given
    # an instance of a Constraint, the `solve` method is called to determine if the constraint will hold or fail.
    #
    # For example, given a `TypeIs` constraint, which limits types to the same type as the given type:
    #
    #   TypeIs.new(String).solve(String) # => true
    #   TypeIs.new(String).solve(Integer) # => false
    #
    # If the `solve` method returns `false`, the `report` method is invoked on the constraint, which generates
    # error message(s) describing the failure.
    #
    # @see Constraint#solve
    # @see Constraint#report
    class Constraint
      include Lume::MIR

      attr_reader :expected

      # Initializes a new constraint.
      #
      # @param checker [TypeChecker] The type checker that is used to solve the constraint.
      # @param expected [Type] The type that is expected by the constraint.
      def initialize(checker, expected)
        @checker = checker
        @expected = expected
      end

      # Tries to solve the constraint.
      #
      # If the method returns `true`, the constraint is satisfied. If not, the constraint is broken, forcing the
      # type checker to invoke `report` on the constraint, to describe the failure.
      #
      # @param actual [Type] The actual type to of the expression.
      #
      # @return [ConstraintResult]
      def solve(actual)
        raise NotImplementedError, %(
          "Constraint::solve must be implemented by subclasses. Implementation missing on #{self.class.name}"
        )
      end

      # If the constraint fails, describes the error, so that the type checker can report it to the user.
      #
      # @param actual [Type] The erroneous type which failed the constraint.
      #
      # @return [Array<TypeError>] Error messages describing the failure.
      def report(actual)
        raise NotImplementedError, %(
          "Constraint::report must be implemented by subclasses. Implementation missing on #{self.class.name}"
        )
      end

      # Constructs a constraint which is compatible with the given type.
      #
      # @param checker [TypeChecker] The type checker that is used to solve the constraint.
      # @param type [Type] The type to construct a constraint for.
      #
      # @return [Constraint] A constraint which is compatible with the given type.
      def self.construct_for(checker, type)
        case type
        when NamedType then TypeIs.new(checker, type)
        when Union then OneOf.new(checker, type)
        end
      end
    end

    # A constraint for a type to be equal to another.
    #
    # This constraint does not handle subtype relationships, but it does attempt to handle
    # automatic casting of integer values.
    class TypeIs < Constraint
      def solve(actual)
        # If the types are equal, the constraint is satisfied.
        return ConstraintResult.success if expected == actual

        # We can cast integer values automatically, if the width of the expected type is greater
        # than the width of the actual type.
        #
        # We don't allow casting a type to a smaller width, as it'd lose precision.
        return ConstraintResult.success(expected) if upcastable?(actual, expected)

        ConstraintResult.failure
      end

      def report(actual)
        [TypeError.new("Expected #{expected}, got #{actual}")]
      end

      # Determines whether a type can be upcasted into another type.
      #
      # @param from [Type] The type to check.
      # @param to [Type] The type to check.
      #
      # @return [Boolean]
      def upcastable?(from, to)
        # It's only possible to update on integer scalar types, so we need to sort out any
        # non-integer scalar types.
        return false unless castable?(to) && castable?(from)

        # We can cast integer values automatically, if the width of the expected type is greater
        # than the width of the actual type.
        #
        # We don't allow casting a type to a smaller width, as it'd lose precision.
        return true if to.width > from.width

        # We can also cast integer values to types of the same width, only if the signedness of the expected
        # type is different from the signedness of the actual type. This is to prevent the value from being
        # truncated or extended incorrectly.
        #
        # As an example, if we cast an unsigned 8-bit value (`255`) to a signed 8-bit value, the value would
        # be extended incorrectly to `-1`.
        return true if to.width == from.width && to.signed? != from.signed?

        false
      end

      # Determines whether the given type can be casted.
      #
      # @param type [Type] The type to check.
      #
      # @return [Boolean]
      def castable?(type)
        type.is_a?(NamedType) && type.integer?
      end
    end

    # A constraint for a type to be one of a list of types.
    #
    # This constraint creates a subset of other constraints, which best fit each individual type within the set.
    class OneOf < Constraint
      def solve(actual)
        # Iterate over all the types within the union and create constraints for each type.
        constraints = expected.types.map { |type| Constraint.construct_for(@checker, type) }

        # If any of the constraints can be satisfied, then the union itself can be solved.
        return ConstraintResult.success if constraints.any? { |constraint| constraint.solve(actual).success? }

        ConstraintResult.failure
      end

      def report(actual)
        [TypeError.new("Expected #{expected}, got #{actual}")]
      end
    end
  end
end
