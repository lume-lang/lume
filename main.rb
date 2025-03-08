# frozen_string_literal: true

require 'pp'
require_relative 'lib/nox'

# SOURCE = %(
# struct String
#   def String(value: String)
#     self.value = value
#   end
# end
# )

SOURCE = '2 * 3 + 4'

# 2 * (3 + (4))
# (2 * (3)) + 4

parser = Nox::Language::Parser.with_source(SOURCE)
tree = parser.parse

pp tree

# analyzer = Nox::Analyzer.with_tree(tree)
# analyzer.add_default_passes!
# analyzer.analyze!
#
# compiler = Nox::Compiler.new
# compiler.add_ast!(tree)
#
# compiler.compile!
# compiler.optimize!
# compiler.finalize!
# compiler.dump!

# ret = compiler.evaluate.to_i
# puts "Return code: #{ret}"

# compiler.emit('output.o')

# compiler.finish
