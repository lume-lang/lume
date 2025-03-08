*Name* ::
  *NameStart* *NameContinue*<sub>list,opt</sub> [lookahead != *NameContinue*]

*NameStart* ::
  *Letter*
  _

*NameContinue* ::
  *Letter*
  *Digit*
  _

*Letter* :: **one of**
  **A**	**B**	**C**	**D**	**E**	**F**	**G**	**H**	**I**	**J**	**K**	**L**	**M**
  **N**	**O**	**P**	**Q**	**R**	**S**	**T**	**U**	**V**	**W**	**X**	**Y**	**Z**
  **a**	**b**	**c**	**d**	**e**	**f**	**g**	**h**	**i**	**j**	**k**	**l**	**m**
  **n**	**o**	**p**	**q**	**r**	**s**	**t**	**u**	**v**	**w**	**x**	**y**	**z**

*Digit* :: **one of**
  **0**	**1**	**2**	**3**	**4**	**5**	**6**	**7**	**8**	**9**

*Literal* :: **one of**
  *BooleanLiteral* *IntegerLiteral* *RealLiteral* *StringLiteral* *NilLiteral*

*BooleanLiteral* :: **one of**
  `true` `false`

*IntegerLiteral* ::
  `true` `false`

*RealLiteral* ::
  `true` `false`

*StringLiteral* ::
  " *StringCharacter*<sub>list</sub> "

*StringCharacter* ::
  U+0009
  U+000A
  U+000D
  U+0020 – U+FFFF

*NilLiteral* ::
  `nil`

*Operator* ::
  `+` `-` `*` `/` `=` `==` `!=` `<` `>` `<=` `>=` `&&` `||` `!`

*Punctuation* ::
  `,` `.` `:` `(` `)` `[` `]` `{` `}`

*Expression* :: **one of**
  *OperatorExpression*
  *MethodInvocation*
  *FunctionInvocation*
  *VariableReference*
  *Literal*

*OperatorExpression*
  *Expression* *Operator* *Expression*

*MethodInvocation*
  *VariableReference* . *Name* *Arguments*

*FunctionInvocation*
  *Name* *Arguments*

*VariableReference*
  *Name*

*Arguments* ::
  ( *Argument*<sub>list</sub> )

*Argument* ::
  *Name* : *Value*
