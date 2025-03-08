# frozen_string_literal: true

class Integer # :nodoc:
  include Lume::Language

  def int8
    ByteLiteral.new(self)
  end

  def uint8
    UnsignedByteLiteral.new(self)
  end

  def int16
    ShortLiteral.new(self)
  end

  def uint16
    UnsignedShortLiteral.new(self)
  end

  def int32
    WordLiteral.new(self)
  end

  def uint32
    UnsignedWordLiteral.new(self)
  end

  def int64
    LongLiteral.new(self)
  end

  def uint64
    UnsignedLongLiteral.new(self)
  end
end

class Float # :nodoc:
  include Lume::Language

  def float32
    FloatLiteral.new(self)
  end

  def float64
    DoubleLiteral.new(self)
  end
end

class String # :nodoc:
  include Lume::Language

  def var
    VariableReference.new(self)
  end
end
