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

      # Determine whether to use integer or floating-point operations
      type = if call.instance.expression_type.integer?
        LLVM::IntType
      else
        LLVM::RealType
      end

      case call.action.to_sym
      when :+ then builtin_op_add(type, lhs, rhs)
      when :- then builtin_op_sub(type, lhs, rhs)
      when :* then builtin_op_mul(type, lhs, rhs)
      when :/ then builtin_op_div(type, lhs, rhs)
      when :== then builtin_op_eq(type, lhs, rhs)
      when :!= then builtin_op_neq(type, lhs, rhs)
      else raise "Unsupported intrinsic operation: #{call.action}"
      end
    end

    # Performs an intrinsic addition operation on two expressions.
    #
    # @param type [Class] The type of the operands.
    # @param lhs [LLVM::Instruction] The left-hand side expression.
    # @param rhs [LLVM::Instruction] The right-hand side expression.
    #
    # @return [LLVM::Instruction] The LLVM IR representation of the operation.
    def builtin_op_add(type, lhs, rhs)
      return @builder.int_add(lhs, rhs) if type == LLVM::IntType
      return @builder.float_add(lhs, rhs) if type == LLVM::RealType

      raise "Unsupported type for addition: #{type}"
    end

    # Performs an intrinsic subtraction operation on two expressions.
    #
    # @param type [Class] The type of the operands.
    # @param lhs [LLVM::Instruction] The left-hand side expression.
    # @param rhs [LLVM::Instruction] The right-hand side expression.
    #
    # @return [LLVM::Instruction] The LLVM IR representation of the operation.
    def builtin_op_sub(type, lhs, rhs)
      return @builder.int_sub(lhs, rhs) if type == LLVM::IntType
      return @builder.float_sub(lhs, rhs) if type == LLVM::RealType

      raise "Unsupported type for subtraction: #{type}"
    end

    # Performs an intrinsic multiplication operation on two expressions.
    #
    # @param type [Class] The type of the operands.
    # @param lhs [LLVM::Instruction] The left-hand side expression.
    # @param rhs [LLVM::Instruction] The right-hand side expression.
    #
    # @return [LLVM::Instruction] The LLVM IR representation of the operation.
    def builtin_op_mul(type, lhs, rhs)
      return @builder.int_mul(lhs, rhs) if type == LLVM::IntType
      return @builder.float_mul(lhs, rhs) if type == LLVM::RealType

      raise "Unsupported type for multiplication: #{type}"
    end

    # Performs an intrinsic division operation on two expressions.
    #
    # @param type [Class] The type of the operands.
    # @param lhs [LLVM::Instruction] The left-hand side expression.
    # @param rhs [LLVM::Instruction] The right-hand side expression.
    #
    # @return [LLVM::Instruction] The LLVM IR representation of the operation.
    def builtin_op_div(type, lhs, rhs)
      return @builder.int_div(lhs, rhs) if type == LLVM::IntType
      return @builder.float_div(lhs, rhs) if type == LLVM::RealType

      raise "Unsupported type for division: #{type}"
    end

    # Performs an intrinsic equality comparison operation on two expressions.
    #
    # @param type [Class] The type of the operands.
    # @param lhs [LLVM::Instruction] The left-hand side expression.
    # @param rhs [LLVM::Instruction] The right-hand side expression.
    #
    # @return [LLVM::Instruction] The LLVM IR representation of the operation.
    def builtin_op_eq(type, lhs, rhs)
      return @builder.int_eq(lhs, rhs) if type == LLVM::IntType
      return @builder.float_eq(lhs, rhs) if type == LLVM::RealType

      raise "Unsupported type for equality comparison: #{type}"
    end

    # Performs an intrinsic non-equality comparison operation on two expressions.
    #
    # @param type [Class] The type of the operands.
    # @param lhs [LLVM::Instruction] The left-hand side expression.
    # @param rhs [LLVM::Instruction] The right-hand side expression.
    #
    # @return [LLVM::Instruction] The LLVM IR representation of the operation.
    def builtin_op_neq(type, lhs, rhs)
      return @builder.int_neq(lhs, rhs) if type == LLVM::IntType
      return @builder.float_neq(lhs, rhs) if type == LLVM::RealType

      raise "Unsupported type for inequality comparison: #{type}"
    end
  end
end
