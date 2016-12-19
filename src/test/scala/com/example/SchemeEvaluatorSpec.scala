package com.example


import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll


class SchemeEvaluatorSpec extends FlatSpec with Matchers with BeforeAndAfterAll {

  import SchemeInterpreter._
  import SchemeEvaluator._


  def check(input: SExpression, output: SExpression) {
  
    eval(globalCtx, input)._2 shouldBe(output)
  }

  def checkContext(input: SExpression, key: String, value: Option[SExpression]) {
  
    eval(globalCtx, input)._1.lookUp(key) shouldBe(value)
  }

  "A Scheme Evaluator" should "evaluate the not expression successfully" in {
    // we use true to stand for "#t" in scheme, and false for "#f"
    check(SCombination(List(Symbol("not"), Value(Bool(true)))), Value(Bool(false)))
    check(SCombination(List(Symbol("not"), Value(Bool(false)))), Value(Bool(true)))
    check(SCombination(List(Symbol("not"), SCombination(List(Symbol("="), Value(Num(1)), Value(Num(1)))))), Value(Bool(false)))
  }

  it should "evaluate the list expression successfully" in {
    check(SCombination(List(Symbol("list"), Value(StringLiteral("hello")),Value(StringLiteral("world")))), SList(List(Value(StringLiteral("hello")), Value(StringLiteral("world")))))
    check(SCombination(List(Symbol("list"), Value(Num(1)),Value(Num(2)))), SList(List(Value(Num(1)), Value(Num(2)))))
    check(SCombination(List(Symbol("list"))), SList(List()))
  }

  it should "evaluate the car expression successfully" in {

    check(SCombination(List(Symbol("car"), SCombination(List(Symbol("list"), Value(Num(1)), Value(Num(2)))))), Value(Num(1)))

  }

  it should "evaluate the cdr expression successfully" in {

    check(SCombination(List(Symbol("cdr"), SCombination(List(Symbol("list"), Value(Num(1)), Value(Num(2)), Value(Num(3)))))), SList(List(Value(Num(2)), Value(Num(3)))))

    check(SCombination(List(Symbol("cdr"), SCombination(List(Symbol("list"), Value(Num(1)))))), SList(List()))
  }

  it should "evaluate the null expression successfully" in {
    check(SCombination(List(Symbol("null?"), SCombination(List(Symbol("list"))))), Value(Bool(true)))
    check(SCombination(List(Symbol("null?"), SCombination(List(Symbol("list"), Value(Num(1)))))), Value(Bool(false)))
  }

  it should "evaluate the arthimetic expression successfully" in {
    check(SCombination(List(Symbol("+"), Value(Num(1)), Value(Num(1)))), Value(Num(2)))
    check(SCombination(List(Symbol("+"), Value(Num(1)), SCombination(List(Symbol("+"), SCombination(List(Symbol("+"), Value(Num(1)), Value(Num(1)))), Value(Num(1)))))), Value(Num(4)))
    check(SCombination(List(Symbol("-"), Value(Num(1)), Value(Num(2)), Value(Num(3)))), Value(Num(-4)))
    check(SCombination(List(Symbol("*"), Value(Num(1)), Value(Num(2)), Value(Num(3)))), Value(Num(6)))
    check(SCombination(List(Symbol("/"), Value(Num(6)), Value(Num(2)), Value(Num(2)))), Value(Num(1.5)))
  }

  it should "evaluate the combination expression successfully" in {
    
    check(SCombination(List(Symbol(">"), Value(Num(3)), Value(Num(2)), Value(Num(1)))), Value(Bool(true)))
    check(SCombination(List(Symbol("<="), Value(Num(1)), Value(Num(2)), Value(Num(3)))), Value(Bool(true)))
  }

  it should "evaluate the cons expression successfully" in {
    check(SCombination(List(Symbol("cons"), Value(Num(1)), Value(Num(2)))), SList(List(Value(Num(1)), Value(Num(2)))))
    check(SCombination(List(Symbol("cons"), SCombination(List(Symbol("cons"), Value(StringLiteral("hello")), Value(StringLiteral("world")))), Value(Num(2)))), SList(List(SList(List(Value(StringLiteral("hello")), Value(StringLiteral("world")))), Value(Num(2)))))
  }

  it should "evaluate the if expression successfully" in {
    check(SCombination(List(Symbol("if"), Value(Bool(true)), Value(Num(1)))),  Value(Num(1)))
    check(SCombination(List(Symbol("if"), Value(Bool(false)), Value(Num(1)))),  SNull())
    check(SCombination(List(Symbol("if"), SCombination(List(Symbol("<"), Value(Num(2)), Value(Num(1)))), Value(Num(10)), Value(Num(11)))),  Value(Num(11)))
  }

  it should "evaluate the cond expression successfully" in {
    check(SCombination(List(Symbol("cond"), SCombination(List(Value(Bool(true)), Value(Num(1)))))),  Value(Num(1)))
    check(SCombination(List(Symbol("cond"), SCombination(List(Value(Bool(false)), Value(Num(1)))), SCombination(List(Value(Bool(true)), Value(Num(2)))))),  Value(Num(2)))
    check(SCombination(List(Symbol("cond"), SCombination(List(Value(Bool(false)), Value(Num(1)))), SCombination(List(Value(Bool(false)), Value(Num(2)))))),  SNull())
  }

  it should "evaluate the define expression successfully" in {
    checkContext(SCombination(List(Symbol("define"), Symbol("name"), Value(StringLiteral("yy")))), "name",  Some(Value(StringLiteral("yy"))))
    checkContext(SCombination(List(Symbol("define"), Symbol("name"), Value(StringLiteral("yue")))), "name",  Some(Value(StringLiteral("yue"))))
    checkContext(SCombination(List(Symbol("define"), Symbol("name"), Value(StringLiteral("yue")))), "age",  None)
    checkContext(SCombination(List(Symbol("define"), SCombination(List(Symbol("add"), Symbol("a"), Symbol("b"))), SCombination(List(Symbol("+"), Symbol("a"), Symbol("b"))) )), "add", Some(SFunc(List(StringLiteral("a"), StringLiteral("b")), List(SCombination(List(Symbol("+"), Symbol("a"), Symbol("b")))))))
  }

  it should "evaluate the let expression successfully" in {
    check(SCombination(List(Symbol("let"), SCombination(List(SCombination(List(Symbol("a"), Value(Num(1)))), SCombination(List(Symbol("b"), Value(Num(2)))))), SCombination(List(Symbol("+"), Symbol("a"), Symbol("b"))))),  Value(Num(3)))
  }

  it should "evaluate the lambda expression successfully" in {
    check(SCombination(List(Symbol("lambda"), SCombination(List(Symbol("x"))), SCombination(List(Symbol("*"), Symbol("x"), Symbol("x"))))), SFunc(List(StringLiteral("x")), List(SCombination(List(Symbol("*"), Symbol("x"), Symbol("x"))))))
  }

}


