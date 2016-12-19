package com.example


import SchemeParser._
import SchemeInterpreter._
import SchemeEvaluator._

object Scheme extends App {
  
  // evaluate a single line input
  def run(ctx: Context, input: String): (Context, SExpression) = if (input.isEmpty) (ctx, Value(StringLiteral(""))) else eval(ctx, parse(input).head)

  
  def _listToString(ls: List[SExpression]) = {
    def ltos(ls: List[SExpression]): String = ls match {
      case List()   => ""
      case Value(Num(v)) :: t   => v.toString + ", " + ltos(t)
      case Value(Bool(v)) :: t  => v.toString + ", " + ltos(t)
      case Value(StringLiteral(v)) :: t  => v.toString + ", " + ltos(t)
      case SList(l) :: t        => "(" + ltos(l) + "), " + ltos(t)
      case _ :: t               => ltos(t)
    }
    "(" + ltos(ls) + ")"
  }
  // evalute the input lines from STDIN or a file
  def loop(ctx: Context, input: String): Unit = {

    try {
      val (newCtx, result) = run(ctx, input)

      result match {
        case Value(Num(v))    => println(v)
        case Value(StringLiteral(v))   => println(v)
        case Value(Bool(v))   => println(v)
        case SList(l)         => println(_listToString(l))
        case _                => println("null")
      }
      loop(newCtx, readLine())
    } catch {
      case e: Exception => println("Exit!")
    }
  }

  loop(globalCtx, readLine())
}
