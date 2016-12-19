package com.example

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll

// integration test for scheme interpreter
class Schemepec extends FlatSpec with Matchers with BeforeAndAfterAll {

  import SchemeInterpreter._
  import SchemeEvaluator._


  "A Scheme Interpreter" should "evaluate the basic expressions successfully" in {

    Scheme.run(globalCtx, "(+ 1 1)")._2 shouldBe(Value(Num(2)))

    Scheme.run(globalCtx, "(+ 1 (+ 1 1))")._2 shouldBe(Value(Num(3)))

    Scheme.run(globalCtx, "(= 3 (+ (+ 1 1) 1))")._2 shouldBe(Value(Bool(true)))

    Scheme.run(globalCtx, "(not (> 1 2))")._2 shouldBe(Value(Bool(true)))

    Scheme.run(globalCtx, "(list 1 2)")._2 shouldBe(SList(List(Value(Num(1)), Value(Num(2)))))

    Scheme.run(globalCtx, "(car (cdr (list 1 2)))")._2 shouldBe(Value(Num(2)))

    Scheme.run(globalCtx, "(cdr (list 1))")._2 shouldBe(SList(List()))
  }

  it should "evaluate the function expressions successfully" in {

    Scheme.run(Scheme.run(globalCtx, ("(define (add a b) (+ a b))"))._1, "(add 1 1)")._2 shouldBe(Value(Num(2)))
    
    Scheme.run(Scheme.run(globalCtx, ("(define (fact x) (if (= x 0) 1 (* x (fact (- x 1)))))"))._1, "(fact 10)")._2 shouldBe(Value(Num(3628800)))
  }

    Scheme.run(Scheme.run(globalCtx, ("(define (map f l) (if (not (null? l)) (cons (f (car l)) (map f (cdr l)))))"))._1, "(map (lambda (x) (* x x)) (list 1 2 3))")._2 shouldBe(SList(List(Value(Num(1)), Value(Num(4)), Value(Num(9)))))
}  
