# frozen_string_literal: true

require 'lume/lume_mir/mir'

module Lume
  class Compiler
    # Defines an abstraction layer for building LLVM IR.
    #
    # This class provides a high-level interface for constructing LLVM IR code.
    # It encapsulates the complexity of the LLVM IR language and provides a more
    # intuitive and user-friendly API for generating LLVM IR code.
    class Builder
      # Initializes a new instance of the Builder class.
      #
      # @param mod [LLVM::Module] The LLVM module to which the builder will be attached.
      def initialize(mod)
        @module = mod
        @inner = LLVM::Builder.new

        @functions = {}
        @block_stack = []
      end

      # Disposes of the builder and its associated resources.
      #
      # @return [void]
      def dispose
        @inner.dispose
      end

      # Allocates a block of memory on the heap.
      #
      # @param count  [Integer]  Amount of bytes to allocate.
      #
      # @return       [LLVM::Value] Pointer to the allocated memory block.
      def allocate(count)
        malloc(count)
      end

      # Loads the value of an existing variable.
      #
      # @param variable [LLVM::Value] The variable to assign to.
      #
      # @return [LLVM::Instruction] The value of the variable.
      def load(variable)
        in_current_block { @inner.load(variable) }
      end

      # Assigns the value of an existing variable.
      #
      # @param variable [LLVM::Value] The variable to assign to.
      # @param value    [LLVM::Value] The value to assign.
      #
      # @return [LLVM::Value] The variable reference.
      def store(variable, value)
        in_current_block { @inner.store(value, variable) }

        variable
      end

      # Allocates and declares a new variable on the stack.
      #
      # @param name   [String]      The name of the variable.
      # @param type   [LLVM::Type]  The type of the variable.
      # @param value  [LLVM::Value] The initial value of the variable. Defaults to `nil` for no value.
      #
      # @return       [LLVM::Instruction] The declared variable.
      def declare_var(name, type, value: nil)
        in_current_block do
          variable = @inner.alloca(type, name)

          # If a value is provided, store it in the variable
          store(variable, value) if value

          variable
        end
      end

      # Declares a new function with the given name, argument types, and return type.
      #
      # Declared functions are just like normal functions, but they exist without a body. They are used to
      # declare functions that are defined elsewhere, such as the C standard library functions or forward declarations.
      #
      # @param name           [String]            The name of the function.
      # @param argument_types [Array<LLVM::Type>] The types of the function's arguments.
      # @param return_type    [LLVM::Type]        The type of the function's return value.
      #
      # @return [LLVM::Function] The defined function.
      def declare_function(name, argument_types, return_type, *)
        raise "Function '#{name}' already declared." if func_registered?(name)

        @functions[name] = @module.functions.add(name, argument_types, return_type, *)
      end

      # Defines a new function with the given name, argument types, and return type.
      #
      # @param name           [String]            The name of the function.
      # @param argument_types [Array<LLVM::Type>] The types of the function's arguments.
      # @param return_type    [LLVM::Type]        The type of the function's return value.
      # @param block          [Proc]              The block of code to execute within the function context.
      #
      # @return [LLVM::Function] The defined function.
      def define_function(name, argument_types, return_type, *, &)
        # If the function hasn't been forward declared yet, do so now.
        declare_function(name, argument_types, return_type, *) unless func_registered?(name)

        # Execute the block within the function context
        in_func_block(@functions[name], &)
      end

      # Invokes a function with the given name and arguments.
      #
      # @param target [String, LLVM::Function] The name of the function to invoke.
      # @param args [Array<LLVM::Value>] The arguments to pass to the function.
      #
      # @return [LLVM::Value] The result of the function invocation.
      def invoke(target, args = [])
        # If the target is a string, it should be referencing an existing function
        target = @functions[target.to_s] if target.is_a?(String) || target.is_a?(Symbol)

        raise ArgumentError, "Function not found: #{target}" if target.nil?

        in_current_block { @inner.call(target, *args) }
      end

      # Returns a value from the current function or method.
      #
      # @param value [LLVM::Value] The value to return.
      #
      # @return [LLVM::Instruction] The return instruction.
      def ret(value)
        in_current_block { @inner.ret(value) }
      end

      # Allocates a block of memory on the heap.
      #
      # @param count  [Integer]  Amount of bytes to allocate.
      #
      # @return       [LLVM::Value] Pointer to the allocated memory block.
      def malloc(count)
        size_arg = LLVM.i(64, count)

        invoke(:malloc, [size_arg])
      end

      # Frees an existing block of memory on the heap.
      #
      # @param ptr [LLVM::Value] Pointer to the memory block to free.
      #
      # @return [LLVM::Instruction] The free instruction.
      def free(ptr)
        invoke(:free, [ptr])
      end

      # Creates a new 1-bit integer literal.
      #
      # @param value [Integer] The value of the literal. Defaults to `0`.
      #
      # @return [LLVM::Value] The result of the comparison.
      def bool(value = 0)
        LLVM.i(1, value)
      end

      # Creates a new integer literal.
      #
      # @param width [Integer] The width of the integer in bits.
      # @param value [Integer] The value of the literal. Defaults to `0`.
      #
      # @return [LLVM::Value] The result of the comparison.
      def int(width, value = 0)
        LLVM.i(width, value)
      end

      # Creates a new 8-bit signed integer literal.
      #
      # @param value [Integer] The value of the literal. Defaults to `0`.
      #
      # @return [LLVM::Value] The result of the comparison.
      def i8(value = 0)
        LLVM.i(8, value)
      end

      # Creates a new 8-bit unsigned integer literal.
      #
      # @param value [Integer] The value of the literal. Defaults to `0`.
      #
      # @return [LLVM::Value] The result of the comparison.
      def u8(value = 0)
        LLVM.i(8, value)
      end

      # Creates a new 16-bit signed integer literal.
      #
      # @param value [Integer] The value of the literal. Defaults to `0`.
      #
      # @return [LLVM::Value] The result of the comparison.
      def i16(value = 0)
        LLVM.i(16, value)
      end

      # Creates a new 16-bit unsigned integer literal.
      #
      # @param value [Integer] The value of the literal. Defaults to `0`.
      #
      # @return [LLVM::Value] The result of the comparison.
      def u16(value = 0)
        LLVM.i(16, value)
      end

      # Creates a new 32-bit signed integer literal.
      #
      # @param value [Integer] The value of the literal. Defaults to `0`.
      #
      # @return [LLVM::Value] The result of the comparison.
      def i32(value = 0)
        LLVM.i(32, value)
      end

      # Creates a new 32-bit unsigned integer literal.
      #
      # @param value [Integer] The value of the literal. Defaults to `0`.
      #
      # @return [LLVM::Value] The result of the comparison.
      def u32(value = 0)
        LLVM.i(32, value)
      end

      # Creates a new 64-bit signed integer literal.
      #
      # @param value [Integer] The value of the literal. Defaults to `0`.
      #
      # @return [LLVM::Value] The result of the comparison.
      def i64(value = 0)
        LLVM.i(64, value)
      end

      # Creates a new 64-bit unsigned integer literal.
      #
      # @param value [Integer] The value of the literal. Defaults to `0`.
      #
      # @return [LLVM::Value] The result of the comparison.
      def u64(value = 0)
        LLVM.i(64, value)
      end

      # Creates a new 32-bit, single-precision floating-point literal.
      #
      # @param value [Float] The value of the literal. Defaults to `0.0`.
      #
      # @return [LLVM::Value] The result of the comparison.
      def f32(value = 0.0)
        LLVM.Float(value)
      end

      # Creates a new 64-bit, double-precision floating-point literal.
      #
      # @param value [Float] The value of the literal. Defaults to `0.0`.
      #
      # @return [LLVM::Value] The result of the comparison.
      def f64(value = 0.0)
        LLVM.Double(value)
      end

      # Gets the LLVM IR type for an 1-bit boolean type.
      #
      # @return [LLVM::Type] The type.
      def bool_type
        LLVM::Int1
      end

      # Gets the LLVM IR type for an integer.
      #
      # @param width [Integer] The width of the integer type.
      #
      # @return [LLVM::Type] The type.
      def int_type(width)
        LLVM::Type.i(width)
      end

      # Gets the LLVM IR type for an 8-bit signed integer.
      #
      # @return [LLVM::Type] The type.
      def i8_type
        LLVM::Int8
      end

      # Gets the LLVM IR type for an 16-bit signed integer.
      #
      # @return [LLVM::Type] The type.
      def i16_type
        LLVM::Int16
      end

      # Gets the LLVM IR type for an 32-bit signed integer.
      #
      # @return [LLVM::Type] The type.
      def i32_type
        LLVM::Int32
      end

      # Gets the LLVM IR type for an 64-bit signed integer.
      #
      # @return [LLVM::Type] The type.
      def i64_type
        LLVM::Int64
      end

      # Gets the LLVM IR type for a floating-point number.
      #
      # @param width [Integer] The width of the floating-point type.
      #
      # @return [LLVM::Type] The type.
      def float_type(width)
        case width
        when 32 then LLVM::Float
        when 64 then LLVM::Double
        else raise ArgumentError, "Unsupported floating-point width: #{width}"
        end
      end

      # Gets the LLVM IR type for a single-precision floating-point number.
      #
      # @return [LLVM::Type] The type.
      def f32_type
        LLVM::Float
      end

      # Gets the LLVM IR type for a double-precision floating-point number.
      #
      # @return [LLVM::Type] The type.
      def f64_type
        LLVM::Double
      end

      # Gets the LLVM IR type for a pointer type.
      #
      # @param type [LLVM::Type] The type to be pointed to.
      #
      # @return [LLVM::Type] The pointer type.
      def ptr(type = nil)
        LLVM.Pointer(type)
      end

      # Creates a new void type.
      #
      # @return [LLVM::Type] The void type.
      def void
        LLVM.Void
      end

      # Creates a new void-pointer type.
      #
      # @return [LLVM::Type] The void-pointer type.
      def void_ptr
        ptr(void)
      end

      # Compares the equality of two integer values.
      #
      # @param lhs [LLVM::Value] The left-hand side operand.
      # @param rhs [LLVM::Value] The right-hand side operand.
      #
      # @return [LLVM::Value] The result of the comparison.
      def int_eq(lhs, rhs)
        in_current_block { @inner.icmp(:eq, lhs, rhs) }
      end

      # Compares the unequality of two integer values.
      #
      # @param lhs [LLVM::Value] The left-hand side operand.
      # @param rhs [LLVM::Value] The right-hand side operand.
      #
      # @return [LLVM::Value] The result of the comparison.
      def int_neq(lhs, rhs)
        in_current_block { @inner.icmp(:ne, lhs, rhs) }
      end

      # Compares whether the left-hand side operand is greater than the right-hand side operand.
      #
      # @param lhs [LLVM::Value] The left-hand side operand.
      # @param rhs [LLVM::Value] The right-hand side operand.
      #
      # @return [LLVM::Value] The result of the comparison.
      def int_gt(lhs, rhs)
        in_current_block { @inner.icmp(:gt, lhs, rhs) }
      end

      # Compares whether the left-hand side operand is greater than or equal to the right-hand side operand.
      #
      # @param lhs [LLVM::Value] The left-hand side operand.
      # @param rhs [LLVM::Value] The right-hand side operand.
      #
      # @return [LLVM::Value] The result of the comparison.
      def int_ge(lhs, rhs)
        in_current_block { @inner.icmp(:ge, lhs, rhs) }
      end

      # Compares whether the left-hand side operand is less than the right-hand side operand.
      #
      # @param lhs [LLVM::Value] The left-hand side operand.
      # @param rhs [LLVM::Value] The right-hand side operand.
      #
      # @return [LLVM::Value] The result of the comparison.
      def int_lt(lhs, rhs)
        in_current_block { @inner.icmp(:lt, lhs, rhs) }
      end

      # Compares whether the left-hand side operand is less than or equal to the right-hand side operand.
      #
      # @param lhs [LLVM::Value] The left-hand side operand.
      # @param rhs [LLVM::Value] The right-hand side operand.
      #
      # @return [LLVM::Value] The result of the comparison.
      def int_le(lhs, rhs)
        in_current_block { @inner.icmp(:le, lhs, rhs) }
      end

      # Adds two integer values.
      #
      # @param lhs [LLVM::Value] The left-hand side operand.
      # @param rhs [LLVM::Value] The right-hand side operand.
      #
      # @return [LLVM::Value] The result of the addition.
      def int_add(lhs, rhs)
        in_current_block { @inner.add(lhs, rhs) }
      end

      # Subtracts two integer values.
      #
      # @param lhs [LLVM::Value] The left-hand side operand.
      # @param rhs [LLVM::Value] The right-hand side operand.
      #
      # @return [LLVM::Value] The result of the subtraction.
      def int_sub(lhs, rhs)
        in_current_block { @inner.sub(lhs, rhs) }
      end

      # Multiplies two integer values.
      #
      # @param lhs [LLVM::Value] The left-hand side operand.
      # @param rhs [LLVM::Value] The right-hand side operand.
      #
      # @return [LLVM::Value] The result of the multiplication.
      def int_mul(lhs, rhs)
        in_current_block { @inner.mul(lhs, rhs) }
      end

      # Divides two integer values.
      #
      # @param lhs [LLVM::Value] The left-hand side operand.
      # @param rhs [LLVM::Value] The right-hand side operand.
      #
      # @return [LLVM::Value] The result of the division.
      def int_div(lhs, rhs)
        in_current_block { @inner.div(lhs, rhs) }
      end

      # Adds two floating-point values.
      #
      # @param lhs [LLVM::Value] The left-hand side operand.
      # @param rhs [LLVM::Value] The right-hand side operand.
      #
      # @return [LLVM::Value] The result of the addition.
      def float_add(lhs, rhs)
        in_current_block { @inner.fadd(lhs, rhs) }
      end

      # Subtracts two floating-point values.
      #
      # @param lhs [LLVM::Value] The left-hand side operand.
      # @param rhs [LLVM::Value] The right-hand side operand.
      #
      # @return [LLVM::Value] The result of the subtraction.
      def float_sub(lhs, rhs)
        in_current_block { @inner.fsub(lhs, rhs) }
      end

      # Multiplies two floating-point values.
      #
      # @param lhs [LLVM::Value] The left-hand side operand.
      # @param rhs [LLVM::Value] The right-hand side operand.
      #
      # @return [LLVM::Value] The result of the multiplication.
      def float_mul(lhs, rhs)
        in_current_block { @inner.fmul(lhs, rhs) }
      end

      # Divides two floating-point values.
      #
      # @param lhs [LLVM::Value] The left-hand side operand.
      # @param rhs [LLVM::Value] The right-hand side operand.
      #
      # @return [LLVM::Value] The result of the division.
      def float_div(lhs, rhs)
        in_current_block { @inner.fdiv(lhs, rhs) }
      end

      # Casts an integer value to another integer type.
      #
      # @param value [LLVM::Value] The integer value to cast.
      # @param type [LLVM::Type] The type of the integer value to cast to.
      # @param signed [Boolean] Whether the cast is signed or unsigned. Defaults to `true`.
      #
      # @return [LLVM::Value] The result of the cast.
      def int_to_int(value, type, signed: true)
        if signed
          sign_extend(value, type)
        else
          zero_extend(value, type)
        end
      end

      # Casts an integer value to a floating-point value.
      #
      # @param value [LLVM::Value] The integer value to cast.
      # @param type [LLVM::Type] The type of the floating-point value to cast to. Defaults to 32-bit float.
      #
      # @return [LLVM::Value] The result of the cast.
      def int_to_float(value, type: LLVM::Float)
        in_current_block { @inner.sitofp(value, type) }
      end

      # Casts a floating-point value to an integer.
      #
      # @param value [LLVM::Value] The floating-point value to cast.
      # @param type [LLVM::Type] The type of the integer to cast to. Defaults to 32-bit integer.
      #
      # @return [LLVM::Value] The result of the cast.
      def float_to_int(value, type: LLVM::Int32)
        in_current_block { @inner.fptosi(value, type) }
      end

      # Sign-extends an integer value to a larger integer type.
      #
      # @param value [LLVM::Value] The integer value to sign-extend.
      # @param type [LLVM::Type] The type of the larger integer to sign-extend to.
      #
      # @return [LLVM::Value] The result of the sign-extension.
      def sign_extend(value, type)
        in_current_block { @inner.sext(value, type) }
      end

      # Zero-extends an integer value to a larger integer type.
      #
      # @param value [LLVM::Value] The integer value to zero-extend.
      # @param type [LLVM::Type] The type of the larger integer to zero-extend to.
      #
      # @return [LLVM::Value] The result of the zero-extension.
      def zero_extend(value, type)
        in_current_block { @inner.zext(value, type) }
      end

      # Executes the given block within the builder block of the current function and/or scope.
      #
      # @param block [Proc] The block to execute within the function's builder block.
      #
      # @return [LLVM::Value] The result of the block execution, if any.
      def in_current_block(&block)
        raise ArgumentError, 'Block required' if block.nil?

        block = @block_stack.last

        # Position the builder at the current block.
        @inner.position_at_end(block)

        yield
      end

      # Executes the given block within the builder block of the given function and/or scope.
      #
      # @param function [LLVM::Function] The function to execute the block within.
      # @param block [Proc] The block to execute within the function's builder block.
      #
      # @return [LLVM::Value] The result of the block execution, if any.
      def in_func_block(function, &block)
        raise ArgumentError, 'Block required' if block.nil?

        # Append a new block to the function, where we can push expressions to.
        entry = function.basic_blocks.append('entry')

        # Position the builder at the function's entry block.
        @inner.position_at_end(entry)

        @block_stack.push(entry)

        result = yield(function, function.params)

        @block_stack.pop

        result
      end

      # Determines whether the given function name has been registered.
      #
      # @param name [String] The name of the function.
      #
      # @return [Boolean] `true` if the function has been registered, `false` otherwise.
      def func_registered?(name)
        @functions.include?(name)
      end
    end
  end
end
