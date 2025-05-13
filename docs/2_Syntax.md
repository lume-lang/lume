# Syntax

## Source Text

SourceCharacter ::
  - `U+0009`
  - `U+000A`
  - `U+000D`
  - `U+0020`-`U+FFFF`

## Whitespace

Whitespace ::
  - "Horizontal Tab" (`U+0009`)
  - "Space" (`U+0020`)

## Line Terminators

LineTerminator ::
  - "New Line" (`U+000A`)
  - "Carriage Return" (`U+000D`) [lookahead != "New Line (U+000A)"]
  - "Carriage Return" (`U+000D`) "New Line (U+000A)"

## Comments

Comments ::
  - Comment
  - DocComment

CommentChar :: SourceCharacter but not LineTerminator

Comment ::
  - "//" CommentChar*

DocComment ::
  - "///" CommentChar*

## Tokens

Token ::
- Symbol
- Name
- Literal

Symbol :: one of
- Operator
- Punctuation

Operator :: one of
- + - * `/` = == != < > && ||
- == != <= >= += -= *= `/=` ++ --

Punctuation :: one of
- ! & | : ( ) [ ] { }

Letter :: one of
- `A` `B` `C` `D` `E` `F` `G` `H` `I` `J` `K` `L` `M`
- `N` `O` `P` `Q` `R` `S` `T` `U` `V` `W` `X` `Y` `Z`
- `a` `b` `c` `d` `e` `f` `g` `h` `i` `j` `k` `l` `m`
- `n` `o` `p` `q` `r` `s` `t` `u` `v` `w` `x` `y` `z`

Digit :: one of
- `0`	`1`	`2`	`3`	`4`	`5`	`6`	`7`	`8`	`9`

BinaryDigit :: one of
- `0`	`1`

OctalDigit :: one of
- `0`	`1`	`2`	`3`	`4`	`5`	`6`	`7`

HexDigit :: one of
- `0`	`1`	`2`	`3`	`4`	`5`	`6`	`7`	`8`	`9`
- `A` `B` `C` `D` `E` `F`
- `a` `b` `c` `d` `e` `f`

## Names

Name :: NameStart NameContinue* [lookahead != NameContinue]

NameStart ::
- Letter
- _

NameContinue ::
- Letter
- Digit
- _

## Literals

Literal :: one of
- BooleanLiteral
- IntegerLiteral
- RealLiteral
- StringLiteral
- ArrayLiteral

### Boolean values

BooleanLiteral :: one of `true` `false`

### Integer values

IntegerLiteral ::
- IntegerPart
- `0d` Digit+
- `0o` OctalDigit+
- `0x` HexDigit+
- `0b` BinaryDigit+

IntegerPart ::
- Sign? 0
- Sign? NonZeroDigit Digit*

NonZeroDigit :: Digit but not `0`

Sign :: one of `+` `-`

An {IntegerLiteral} must be specified without a decimal point or exponent, but may be negative (ex. `-12`). It must not have a leading `0`.

An {IntegerLiteral} can be either a decimal (ex. `123`, `0d123`), octal (ex. `0o777`), hexadecimal (ex. `0xFF`), or binary (ex. `0b1010`). The form of the sequence is determined by the prefix: `0d` for decimal, `0o` for octal, `0x` for hexadecimal, and `0b` for binary. If no prefix is specified, the literal is interpreted as decimal. Prefixes are case-insensitive.

An {IntegerLiteral} must not be followed by a decimal point or exponent. If either `.` or {ExponentIndicator} follows, it must be interpreted as a {RealLiteral}.

### Real values

RealLiteral ::
- IntegerPart FractionPart ExponentPart? [lookahead != {Digit, `.`, NameStart}]
- IntegerPart FractionPart [lookahead != {Digit, `.`, NameStart}]
- IntegerPart ExponentPart [lookahead != {Digit, `.`, NameStart}]

FractionPart :: `.` Digit+

ExponentPart :: ExponentIndicator Sign? Digit+

ExponentIndicator :: one of `e` `E`

Sign :: one of `+` `-`

A {RealLiteral} can be either a fraction (ex. `0.5`) or an exponent (ex. `1e10`)  or a fractional exponent (ex. `1.73123e8`) and may be negative. Like {IntegerLiteral}, it also must not have a leading `0`.

### String values

StringLiteral :: one of `"` StringCharacter+ `"`

StringCharacter :: SourceCharacter but not `"`

### Array values

ArrayLiteral :: `[` Expression* `]`

## Statements

Statement :: one of
- VariableDeclaration
- Loop
- Break
- Continue
- Return
- Conditional
- Expression

StatementDelimiter :: `;`

### Variable declarations

VariableDeclaration ::
- MutableVariableDeclaration
- ImmutableVariableDeclaration

MutableVariableDeclaration ::
- `let` Name `=` Expression StatementDelimiter
- `let` Name `:` Type StatementDelimiter
- `let` Name `:` Type `=` Expression StatementDelimiter

ImmutableVariableDeclaration ::
- `const` Name `=` Expression StatementDelimiter
- `const` Name `:` Type `=` Expression StatementDelimiter

### Loops

Loop :: one of
- InfiniteLoop
- IteratorLoop
- PredicateLoop

InfiniteLoop :: `loop` Block

IteratorLoop :: `for` Name `in` Expression Block

PredicateLoop :: `while` Expression Block

Break :: `break` StatementDelimiter

Continue :: `continue` StatementDelimiter

### Returns

Return :: `return` Expression? StatementDelimiter

### Conditionals

Conditional ::
- IfConditional
- UnlessConditional

IfConditional ::
- `if` Expression `{` Expression ElseIfConditional* ElseConditional? `}`

UnlessConditional ::
- `unless` Expression `{` Expression ElseConditional? `}`

ElseIfConditional :: `} else if` Expression `{` Expression

ElseConditional :: `} else {` Expression

## Expressions

Expression :: one of
- Literal
- Invocation
- OperatorExpression
- Variable
- Assignment
- SelfReference

### Invocations

Invocation :: one of
- StaticInvocation
- InstanceInvocation

StaticInvocation ::
- Name `(` Arguments `)`

InstanceInvocation ::
- Variable `.` Name `(` Arguments `)`

Arguments ::
- `(` Argument* `)`

Argument ::
- Name `:` Expression

### Operators

OperatorExpression :: Expression Operator Expression

### Variables

Variable :: Name

### Assignment

Assignment :: Expression `=` Expression

### Self reference

SelfReference :: `self`

## Types

NamespaceSeparator :: `::`

NamespaceSegment :: TypeReference NamespaceSeparator

Type :: NamespaceSeparator? NamespaceSegment* TypeReference

TypeReference :: one of
- ScalarType
- ArrayType
- GenericType

ScalarType :: Name

ArrayType :: `[` Type `]`

GenericType :: Name `<` Name* `>`

## Items

Item :: one of
- Import
- Namespace
- Function
- Structure
- Implementation
- Enum
- Alias
- Trait
- TraitUse

### TypeParameters

TypeParameter :: Name

TypeParameters :: `<` TypeParameter* `>`

### Visibility

Visibility :: one of
- `pub`

### Imports

Import :: `import` NamespacePath+ `(` Name+ `)`

NamespacePath :: Name NamespaceSeparator

### Namespaces

Namespace :: `namespace` NamespacePath+

### Functions

Function :: one of
- FunctionDefinition
- ExternalFunctionDefinition

FunctionDefinition :: Visibility? `fn` Name TypeParameters? Parameters? ReturnType? Block

ExternalFunctionDefinition :: Visibility? `fn` `external` Name TypeParameters? Parameters? ReturnType?

#### Parameters

Parameters :: `(` Parameter* `)`

Parameter :: one of
- SelfParam
- NamedParam

SelfParam :: `self`

NamedParam :: Name `:` Type

#### Return types

ReturnArrow :: `->`

ReturnType :: ReturnArrow Type

#### Blocks

Block :: `{` Statement* `}`

### Structures

### Implementations

### Enumerations

### Type aliases

### Traits
