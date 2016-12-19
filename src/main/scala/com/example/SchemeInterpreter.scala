package com.example

import language.higherKinds
import language.implicitConversions

object SchemeInterpreter {

  sealed trait ValueT
  case class Num(get: BigDecimal) extends ValueT
  case class Bool(get: Boolean) extends ValueT
  case class StringLiteral(get: String) extends ValueT

  sealed trait SExpression 
  //diffent kinds of SExpression
  case class Value(get: ValueT) extends SExpression

  case class SNull() extends SExpression
  case class SList(get: List[SExpression]) extends SExpression
  case class Symbol(get: String) extends SExpression

  case class SProc(f: ((Context, List[SExpression]) => (Context, SExpression))) extends SExpression
  case class SFunc(args: List[ValueT], body: List[SExpression]) extends SExpression

  case class SCombination(get: List[SExpression]) extends SExpression

  def eval(ctx: Context, expr: SExpression): (Context, SExpression) = expr match {
    case SNull()       => throw new IllegalStateException("invalid interpreter state")
    case SCombination(List())     => throw new IllegalStateException("invalid combination")
    case SCombination(h :: t)     =>
      eval(ctx, h) match {
        case (_, SProc(f))             => f(ctx, t)
        case (nCtx, SFunc(args, body)) => {
          if (args.length != t.length) throw new IllegalArgumentException("invalid number of arguments")
          val newCtx = (args zip t).foldLeft(nCtx.expand())((acc, av) => bindArg(acc, av._1, av._2))
          evalCombination(newCtx, body)
        }
        case (nCtx, expr)             => (nCtx, expr)
      }
    case SProc(f)          => (ctx, SProc(f))
    case SFunc(args, body) => throw new IllegalArgumentException("invalid function call")
    case v @ Value(_)     => (ctx, v)
    case l @ SList(_)      => (ctx, l)
    case Symbol(s)        =>
      ctx.lookUp(s) match {
        case Some(e)  => (ctx, e)
        case None     => throw new IllegalArgumentException("unbound symbol '" + s +"'")
    }
  }


  // bind argument in a new context
  private def bindArg(ctx: Context, arg: ValueT, expr: SExpression) = arg match {
    case StringLiteral(n)  => ctx.addEntry(n -> eval(ctx, expr)._2)
    case _        => throw new IllegalArgumentException
  }

  // Eval a combination (a list of expressions), return the value of the last one
  def evalCombination(ctx: Context, comb: List[SExpression]): (Context, SExpression) = comb match {
    case List() => (ctx, SNull())
    case h :: t => {
      val (nCtx, res) = eval(ctx, h)
      t.length match {
        case 0 => (ctx, res)
        //case 1 => eval(nCtx, t.head)
        case _ => evalCombination(nCtx, t)
      }
    }
  }
}

