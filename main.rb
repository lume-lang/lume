# frozen_string_literal: true

require 'pp'
require_relative 'lib/nox'

SOURCE = %(
struct String
  def String(value: String)
    self.value = value
  end
end
)

parser = Nox::Language::Parser.with_source(SOURCE)
# pp parser.tokens

tree = parser.parse
pp tree.nodes

analyzer = Nox::Analyzer.with_tree(tree)
analyzer.add_default_passes!
analyzer.analyze!

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
