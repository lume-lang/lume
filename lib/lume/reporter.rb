# frozen_string_literal: true

module Lume
  # The Reporter class is an abstract base class for reporting diagnostics to the user.
  class Reporter
    # Reports the given error to the user via the standard output (`stdout`).
    #
    # @param diagnostic [StandardError] The error to report.
    #
    # @return [void]
    def report(diagnostic)
      raise NotImplementedError, "Reporter#report must be implemented. Missing implementation on #{self.class.name}"
    end
  end
end
