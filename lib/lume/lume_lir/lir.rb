# frozen_string_literal: true

module Lume
  module LIR
    # Defines a module which contains zero-or-more graphs, for each function or method.
    class Module
      attr_accessor :functions

      def initialize
        @functions = {}
      end
    end

    # Represents a directed graph of control flow within a single function or method.
    class Function
      # Defines the parameter- and return-type for the function.
      attr_accessor :parameters, :return_type

      # Defines all the nodes within the function.
      attr_accessor :nodes

      def initialize
        @nodes = []
      end

      def add_start_block
        @nodes << Node.new(:entry)
      end
    end

    # Represents a single node within a directed graph of control flow.
    class Node
      attr_accessor :name, :instructions

      def initialize(name = nil)
        @name = name unless name.nil?
        @instructions = []
      end

      # Determines whether the node contains any instructions of the given type(s).
      #
      # @param type [Class] The type(s) of instructions to find.
      #
      # @return [Boolean] `true` if the node contains instructions of the given type, `false` otherwise.
      def any?(*type)
        type.flatten.any? { |t| @instructions.any? { |instr| instr.is_a?(t) } }
      end

      # Determines whether the node contains a branching statement.
      #
      # @return [Boolean] `true` if the node contains a branching statement, `false` otherwise.
      def branch?
        any?(Return, Goto)
      end
    end

    # Represents a single instruction within a block.
    class Instruction
      # Defines the LLVM register where the node is declared.
      #
      # The LLVM register usually refer to some LLVM value.
      attr_accessor :register
    end

    # Defines an allocation instruction, which allocates memory for a variable.
    class Allocate < Instruction
      def initialize(type)
        super()

        @type = type
      end
    end

    # Defines an assignment instruction, which assigns a value to a variable.
    class Assign < Instruction
      def initialize(target, value)
        super()

        @target = target
        @value = value
      end
    end

    # Defines an unconditional branch instruction, which transfers control to a specified node.
    class Goto < Instruction
      def initialize(register)
        super()

        @register = register
      end
    end

    # Defines a conditional branch instruction, which transfers control to a specified node.
    class Branch < Instruction
      attr_accessor :condition, :if_true, :if_false

      def initialize(condition, if_true, if_false)
        super()

        @condition = condition
        @if_true = if_true
        @if_false = if_false
      end
    end

    # Defines a return instruction, which returns some value and transfers control to a specified node.
    class Return < Instruction
      attr_accessor :value

      def initialize(value)
        super()

        @value = value
      end
    end
  end
end
