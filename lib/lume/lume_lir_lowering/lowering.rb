# frozen_string_literal: true

require 'lume/lume_mir/mir'
require 'lume/lume_mir/loops'
require 'lume/lume_mir/types'
require 'lume/lume_mir/values'
require 'lume/lume_lowering/conditionals'
require 'lume/lume_lowering/loops'

module Lume
  module LIR
    # Generator which lowers all modules from MIR (Middle-Level Intermediate Representation) to LIR
    # (Low-Level Intermediate Representation), which can then be further lowered to LLVM IR.
    class ModuleLower
      # Generates LIR from the given MIR abstract syntax tree (AST).
      #
      # @param mod [Lume::Module] The module to generate LIR from.
      #
      # @return [Lume::LIR::Module] The generated LIR module.
      def generate(mod)
        lir = Lume::LIR::Module.new

        method_lower = MethodLower.new
        functions = mod.mir.find_all(Lume::MIR::FunctionDefinition)

        functions.each do |func|
          lir.functions[func.name] = method_lower.run(func)
        end

        lir
      end
    end

    # Generator which lowers a single method from MIR (Middle-Level Intermediate Representation) to LIR
    # (Low-Level Intermediate Representation), which can then be further lowered to LLVM IR.
    class MethodLower
      # Lowers the given method to LIR.
      #
      # @param method [Lume::MIR::FunctionDefinition] The method to generate LIR from.
      #
      # @return [Lume::LIR::Function] The generated LIR function.
      def run(method)
        @locals = {}
        @locals.compare_by_identity

        @func = Lume::LIR::Function.new
        @current_block = @func.add_start_block

        @func.return_type = type(method.return)
        @func.parameters = method.parameters.map { |param| type(param.type) }
        @func.nodes = [@current_block, *block(method.block)]

        @func
      end

      private

      # Gets the local variable with the given key.
      #
      # @param register [Object] The register to get the local variable for.
      #
      # @return [Lume::LIR::Allocate] The local variable.
      def local(register)
        @locals[register]
      end

      # Sets the local variable with the given key.
      #
      # @param key [Object] The key to set the local variable for.
      # @param register [Lume::LIR::Allocate] The register to set the local variable to.
      def set_local(key, register)
        @locals[key] = register
      end

      # Lowers the given block into LIR.
      #
      # @param block [Block] The block to lower.
      #
      # @return [Array<Lume::LIR::Node>]
      def block(block)
        nodes_and_instructions = block_instr(block)
        nodes = nodes_and_instructions.select { |inst| inst.is_a?(Lume::LIR::Node) }
        instructions = nodes_and_instructions.select { |inst| inst.is_a?(Lume::LIR::Instruction) }

        node = @blocks.last || Lume::LIR::Node.new
        node.instructions = instructions

        [node, *nodes]
      end

      # Lowers the given block into LIR.
      #
      # @param block [Block] The block to lower.
      #
      # @return [Array<Lume::LIR::Instruction>]
      def block_instr(block)
        block.expressions.flat_map { |n| node(n) }
      end

      # Lowers the given node into LIR.
      #
      # @param node [Node] The node to lower.
      #
      # @return [Lume::LIR::Instruction, Array<Lume::LIR::Instruction>]
      def node(node)
        case node
        when Lume::MIR::Literal then literal(node)
        when Lume::MIR::Expression then expression(node)
        when Lume::MIR::Type then type(node)
        else raise "Unsupported node type: #{node.class}"
        end
      end

      # Lowers the given literal into LIR.
      #
      # @param node [Literal] The Literal to lower.
      #
      # @return [Lume::LIR::Literal]
      def literal(node)
        bit_size = node.bytesize * 8

        case node
        when Lume::MIR::NumberLiteral then IntLiteral.new(bits: bit_size, value: node.value)
        when Lume::MIR::RealLiteral then FloatLiteral.new(bits: bit_size, value: node.value)
        when Lume::MIR::BooleanLiteral then BooleanLiteral.new(value: node.value)
        else raise "Unsupported literal type: #{node.class}"
        end
      end

      # Lowers the given type into LIR.
      #
      # @param node [Type] The type to lower.
      #
      # @return [Lume::LIR::Type]
      def type(node)
        return Lume::LIR::Void.new if node.is_a?(Lume::MIR::Void)
        return Lume::LIR::PointerType.new(type(node.of)) if node.is_a?(Lume::MIR::Pointer)

        raise "Unsupported type: #{node.class}" unless node.is_a?(Lume::MIR::NamedType)

        return Lume::LIR::IntType.new(bits: node.width) if node.integer?
        return Lume::LIR::FloatType.new(bits: node.width) if node.floating?
        return Lume::LIR::BooleanType.new if node.boolean?

        raise "Unsupported named type: #{node.name}"
      end

      # Lowers the given expression into LIR.
      #
      # @param node [Expression] The expression to lower.
      #
      # @return [Lume::LIR::Instruction]
      def expression(node)
        case node
        when Lume::MIR::Conditional then conditional_branch(node)
        when Lume::MIR::InfiniteLoop then infinite_loop(node)
        when Lume::MIR::VariableDeclaration then declare(node)
        when Lume::MIR::Variable then ref(node)
        when Lume::MIR::Break then break_loop(node)
        when Lume::MIR::Continue then continue_loop(node)
        when Lume::MIR::Return then ret(node)
        else raise "Unsupported expression type: #{node.class}"
        end
      end

      # Lowers the given conditional into LIR.
      #
      # @param node [Conditional] The conditional to lower.
      #
      # @return [Array<Lume::LIR::Node>]
      def conditional_branch(node)
        old_block = @blocks.last

        merge_node = Lume::LIR::Node.new(:'#if_merge')

        case_branches = node.cases.map do |cond_case|
          case_node = Lume::LIR::Node.new(:'#if_branch')
          case_node.instructions = block_instr(cond_case.block)

          # If the block has no branching instructions, go to the merge node.
          case_node.instructions << Lume::LIR::Goto.new(merge_node) unless cond_case.branch?

          case_node
        end

        case_conditionals = node.cases.each_with_index.map do |cond_case, index|
          next if cond_case.condition.nil?

          conditional = node(cond_case.condition)
          if_true = case_branches[index]
          if_false = case_branches[index + 1] || merge_node

          case_node = Lume::LIR::Node.new(:'#if_cond')
          case_node.instructions << Lume::LIR::Branch.new(conditional, if_true, if_false)

          case_node
        end

        old_block.instructions << Lume::LIR::Goto.new(case_conditionals.first)

        [*case_conditionals, *case_branches, merge_node]
      end

      # Lowers the given infinite loop into LIR.
      #
      # @param node [InfiniteLoop] The infinite loop to lower.
      #
      # @return [Array<Lume::LIR::Node>]
      def infinite_loop(node)
        old_block = @blocks.last

        loop_node = Lume::LIR::Node.new(:'#loop_body')
        loop_exit = Lume::LIR::Node.new(:'#loop_exit')

        old_block.instructions << Lume::LIR::Goto.new(loop_node)

        set_local(node, [loop_node, loop_exit])

        loop_node.instructions = block_instr(node.block)
        loop_node.instructions << Lume::LIR::Goto.new(loop_node) unless node.block.branch?

        [loop_node, loop_exit]
      end

      # Lowers the given variable declaration into LIR.
      #
      # @param node [VariableDeclaration] The variable declaration to lower.
      #
      # @return [Lume::LIR::Allocate]
      def declare(node)
        type = type(node.type)
        value = node(node.value)

        allocation = Lume::LIR::Allocate.new(type)
        assign = Lume::LIR::Assign.new(allocation, value)

        set_local(node, allocation)

        [allocation, assign]
      end

      # Lowers the given variable reference into LIR.
      #
      # @param node [Variable] The variable reference to lower.
      #
      # @return [Lume::LIR::Reference]
      def ref(node)
        local(node.reference)
      end

      # Lowers the given break statement into LIR.
      #
      # @param node [Break] The break statement to lower.
      #
      # @return [Lume::LIR::Goto]
      def break_loop(node)
        loop = node.loop
        _, loop_exit = local(loop)

        Lume::LIR::Goto.new(loop_exit)
      end

      # Lowers the given continue statement into LIR.
      #
      # @param node [Continue] The continue statement to lower.
      #
      # @return [Lume::LIR::Goto]
      def continue_loop(node)
        loop = node.loop
        loop_start, = local(loop)

        Lume::LIR::Goto.new(loop_start)
      end

      # Lowers the given expression into LIR.
      #
      # @param node [Return] The Expression to lower.
      #
      # @return [Lume::LIR::Return]
      def ret(expression)
        value = node(expression.value)

        Lume::LIR::Return.new(value)
      end
    end
  end
end
