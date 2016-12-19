package com.example


import scala.util.parsing.combinator._


object SchemeParser extends JavaTokenParsers {

  import SchemeInterpreter._

  val value: Parser[ValueT] = stringLiteral ^^ (x => StringLiteral(x.tail.init)) |
                              floatingPointNumber ^^ (x => Num(BigDecimal(x)))

  val expression: Parser[SExpression] = value ^^ (x => Value(x)) |
                                  """[^()\s]+""".r ^^ (x => Symbol(x)) |
                                  combination

  val combination: Parser[SCombination] = "(" ~> rep(expression) <~ ")" ^^ (x => SCombination(x))

  val program: Parser[List[SExpression]] = rep(expression)

  def parse(source: String) = parseAll(program, source).get
}
