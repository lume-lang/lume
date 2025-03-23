# frozen_string_literal: true

require 'lume/lume_mir/mir'

module Lume
  class NodeVisitor # :nodoc:
    private

    # Handle an intrinsic operation call.
    #
    # @param call [Lume::MIR::MethodCall] The method call representing the operation.
    #
    # @return [LLVM::Instruction] The LLVM IR representation of the operation.
    def builtin_op(call)
      raise "Cannot handle non-intrinsic operation: #{call.full_name}" unless call.intrinsic?

      args = call.arguments

      lhs = visit(call.instance)
      rhs = visit(args.last)

      case call.action.to_sym
      when :+ then builtin_op_add(lhs, rhs)
      when :- then builtin_op_sub(lhs, rhs)
      when :* then builtin_op_mul(lhs, rhs)
      when :/ then builtin_op_div(lhs, rhs)
      when :== then builtin_op_eq(lhs, rhs)
      else raise "Unsupported intrinsic operation: #{call.action}"
      end
    end

    # Performs an intrinsic addition operation on two expressions.
    #
    # @param lhs [LLVM::Instruction] The left-hand side expression.
    # @param rhs [LLVM::Instruction] The right-hand side expression.
    #
    # @return [LLVM::Instruction] The LLVM IR representation of the operation.
    def builtin_op_add(lhs, rhs)
      @builder.int_add(lhs, rhs)
    end

    # Performs an intrinsic subtraction operation on two expressions.
    #
    # @param lhs [LLVM::Instruction] The left-hand side expression.
    # @param rhs [LLVM::Instruction] The right-hand side expression.
    #
    # @return [LLVM::Instruction] The LLVM IR representation of the operation.
    def builtin_op_sub(lhs, rhs)
      @builder.int_sub(lhs, rhs)
    end

    # Performs an intrinsic multiplication operation on two expressions.
    #
    # @param lhs [LLVM::Instruction] The left-hand side expression.
    # @param rhs [LLVM::Instruction] The right-hand side expression.
    #
    # @return [LLVM::Instruction] The LLVM IR representation of the operation.
    def builtin_op_mul(lhs, rhs)
      @builder.int_mul(lhs, rhs)
    end

    # Performs an intrinsic division operation on two expressions.
    #
    # @param lhs [LLVM::Instruction] The left-hand side expression.
    # @param rhs [LLVM::Instruction] The right-hand side expression.
    #
    # @return [LLVM::Instruction] The LLVM IR representation of the operation.
    def builtin_op_div(lhs, rhs)
      @builder.int_div(lhs, rhs)
    end

    # Performs an intrinsic equality comparison operation on two expressions.
    #
    # @param lhs [LLVM::Instruction] The left-hand side expression.
    # @param rhs [LLVM::Instruction] The right-hand side expression.
    #
    # @return [LLVM::Instruction] The LLVM IR representation of the operation.
    def builtin_op_eq(lhs, rhs)
      @builder.int_eq(lhs, rhs)
    end
  end
end
