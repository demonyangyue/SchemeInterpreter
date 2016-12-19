package com.example

import scala.util.parsing.combinator._

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll

class SchemeParserSpec extends FlatSpec with Matchers with BeforeAndAfterAll {

  import SchemeInterpreter._
  import SchemeParser._


  def check[A](parser: Parser[A])(input: String)(expected: A) {
  
      parseAll(parser, input) match {
        case Success(x, _) => x shouldBe expected
        case _ => fail("parse failed")
      }
  }

  "A Scheme Parser" should "parse the string literals successfully" in {

      check(SchemeParser.value) ("\"Hello world\"") (StringLiteral("Hello world"))
      check(SchemeParser.value) ("\"\"") (StringLiteral(""))
  }

  
  it should "parse the number successfully" in {

      check(SchemeParser.value) ("1234.0") (Num(1234.0))
      check(SchemeParser.value) ("0.0") (Num(0.0))
  }

  it should "parse expressions successfully" in {
  
      check(SchemeParser.expression) ("+") (Symbol("+"))
      //check(scheme.expression) ("+ ") (Symbol("+"))
      check(SchemeParser.expression) ("1") (Value(Num(1)))
      check(SchemeParser.expression) ("(+ 1 1)") (SCombination(List(Symbol("+"), Value(Num(1)), Value(Num(1)))))
  }

  it should "parse combinations successfully" in {
      check(SchemeParser.combination) ("(+ 1 1)") (SCombination(List(Symbol("+"), Value(Num(1)), Value(Num(1)))))
      check(SchemeParser.combination) ("(list 1 1)") (SCombination(List(Symbol("list"), Value(Num(1)), Value(Num(1)))))
      check(SchemeParser.combination) ("(define x 1)") (SCombination(List(Symbol("define"), Symbol("x"), Value(Num(1)))))
  }

  it should "parse scheme code successfully" in {
  
      check(SchemeParser.program) ("(+ 1 2)") (List(SCombination(List(Symbol("+"), Value(Num(1.0)), Value(Num(2.0))))))
      check(SchemeParser.program) ("(+ 1 (+ (+ 1 1) 1))") (List(SCombination(List(Symbol("+"), Value(Num(1)), SCombination(List(Symbol("+"), SCombination(List(Symbol("+"), Value(Num(1)), Value(Num(1)))), Value(Num(1))))))))
      check(SchemeParser.program) ("(let ((a 1)) a)") (List(SCombination(List(Symbol("let"), SCombination(List(SCombination(List(Symbol("a"), Value(Num(1)))))), Symbol("a")))))
      check(SchemeParser.program) ("(define (add a b)(+ a b))") (List(SCombination(List(Symbol("define"), SCombination(List(Symbol("add"), Symbol("a"), Symbol("b"))), SCombination(List(Symbol("+"), Symbol("a"), Symbol("b")))))))
      check(SchemeParser.program) ("(define (adder value)(lambda (x) (+ x value)))") (List(SCombination(List(Symbol("define"), SCombination(List(Symbol("adder"), Symbol("value"))), SCombination(List(Symbol("lambda"), SCombination(List(Symbol("x"))), SCombination(List(Symbol("+"), Symbol("x"), Symbol("value")))))))))

  }
}
