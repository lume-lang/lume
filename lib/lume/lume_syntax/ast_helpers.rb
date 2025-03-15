# frozen_string_literal: true

require 'lume/lume_syntax/ast'

class Integer # :nodoc:
  def int8
    Lume::Syntax::ByteLiteral.new(self)
  end

  def uint8
    Lume::Syntax::UnsignedByteLiteral.new(self)
  end

  def int16
    Lume::Syntax::ShortLiteral.new(self)
  end

  def uint16
    Lume::Syntax::UnsignedShortLiteral.new(self)
  end

  def int32
    Lume::Syntax::WordLiteral.new(self)
  end

  def uint32
    Lume::Syntax::UnsignedWordLiteral.new(self)
  end

  def int64
    Lume::Syntax::LongLiteral.new(self)
  end

  def uint64
    Lume::Syntax::UnsignedLongLiteral.new(self)
  end
end

class Float # :nodoc:
  def float32
    Lume::Syntax::FloatLiteral.new(self)
  end

  def float64
    Lume::Syntax::DoubleLiteral.new(self)
  end
end

class String # :nodoc:
  def var
    Lume::Syntax::VariableReference.new(self)
  end

  def string
    Lume::Syntax::StringLiteral.new(self)
  end
end
