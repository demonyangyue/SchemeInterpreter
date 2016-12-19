package com.example

import language.higherKinds
import language.implicitConversions

object SchemeEvaluator {
  import SchemeInterpreter._

  //evaluate function arguments
  def buildList(l: List[SExpression]): List[ValueT] = {
    def error = throw new IllegalArgumentException("define args")
    l match {
      case Nil => Nil
      case Symbol(x) :: t => StringLiteral(x) :: buildList(t)
      case Value(x) :: t => x :: buildList(t)
      case _ => error
    }
  }

  def aritFun(op: ((BigDecimal, BigDecimal) => BigDecimal))               (ctx: Context, comb: List[SExpression]) = {
    
    def error = throw new IllegalArgumentException("arthimetic error")

    comb.map(eval(ctx, _)._2) match {
      // we use the first element as initialization value
      case Value(Num(first))::t => 
        val result = t.foldLeft(first)((acc, e) =>
          e match {
            case Value(Num(v)) => op(acc, v)
            case _ => error
          })
        (ctx, Value(Num(result)))
      case _ => error
    }
  }

  def compFun(op: ((BigDecimal, BigDecimal) => Boolean))               (ctx: Context, comb: List[SExpression]) = {
  
    def error = throw new IllegalArgumentException("comparison error")

    comb.map(eval(ctx, _)._2) match {
      // we use the first element as initialization value
      case Value(Num(first))::t => 
        // we need to save previous element in the result state
        val result = t.foldLeft((true, first))((acc, e) =>
          e match {
            case Value(Num(v)) => (acc._1 && op(acc._2, v), v)
            case _ => error
          })
        (ctx, Value(Bool(result._1)))
      case _ => error
    }
  }

  def _not(ctx: Context, comb: List[SExpression]): (Context, SExpression) = {
    def error = throw new IllegalArgumentException("not")
    comb match {
      case h::Nil => eval(ctx, h) match {
        case(_, Value(Bool(v))) => (ctx, Value(Bool(!v)))
        case _ => error
      }
      case _ => error
    }
  }

  def _if(ctx: Context, comb: List[SExpression]): (Context, SExpression) = {
    def error = throw new IllegalArgumentException("if")
    comb match {
      case h::t => eval(ctx, h)._2 match {
        case Value(Bool(true)) => eval(ctx, t.head)
        case Value(Bool(false)) => if (t.tail.isEmpty) (ctx, SNull()) else eval(ctx, t.tail.head)
        case _ => error
      }
      case _ => error
    }
  
  }

  def _cond(ctx: Context, comb: List[SExpression]): (Context, SExpression) = {
    def error = throw new IllegalArgumentException("cond")
    

    def exec(comb: List[SExpression]) = comb match {
      case Symbol("else") :: posExpr :: Nil =>
        Some(eval(ctx, posExpr)._2)
      case condExpr :: posExpr :: Nil       => eval(ctx, condExpr)._2 match {
        case Value(Bool(true))  => Some(eval(ctx, posExpr)._2)
        case Value(Bool(false)) => None
        case _                  => error
      }
      case _                                => error
    }
    // we evaluate the list of Combinations recursively
    def evalCombinations(comb: List[SExpression]): SExpression = comb match {
      case SCombination(c) :: t => exec(c) match {
        case Some(e) => e
        case None => evalCombinations(t)
      }
      case _ => SNull()
    }

    (ctx, evalCombinations(comb))
  }

  def _define(ctx: Context, comb: List[SExpression]): (Context, SExpression) = {
  
    def error = throw new IllegalArgumentException("define error")

    def getStr(expr: SExpression) = expr match {
      case Symbol(n)      => n
      case Value(StringLiteral(n)) => n
      case _              => error
    }

    comb match {
      // variable definition
      case Symbol(x) :: y :: Nil => {
        val (newCtx, res) = eval(ctx, y)
        (newCtx.addEntry(x -> res), SNull())
      }
      //function definition
      case SCombination(args) :: body => {
        val fname = getStr(args.head)
        val arguments = buildList(args.tail)
        (ctx.addEntry(fname -> SFunc(arguments, body)), SNull())
      }
      case _ => error
    }
  }

  def _cons(ctx: Context, comb: List[SExpression]): (Context, SExpression) = {
  
    def error = throw new IllegalArgumentException("cons error")
    comb match {
      // cons expression should be consist of two parts
      case x::y::Nil => (eval(ctx, x)._2, eval(ctx, y)._2) match {
        case (expr1, SNull()) => (ctx, SList(List(expr1)))
        case (expr1, SList(l2)) => (ctx, SList(expr1 :: l2))
        case (expr1, expr2) =>  (ctx, SList(List(expr1, expr2)))
      }
      case _ => error
    }
  }

  def _list(ctx: Context, comb: List[SExpression]): (Context, SExpression) = {
    (ctx, SList(comb.map( x => eval(ctx, x)._2)))
  }

  def _car(ctx: Context, comb: List[SExpression]): (Context, SExpression) = {
  
    def error = throw new IllegalArgumentException("car")
    comb match {
      case h::t => eval(ctx,h)._2 match {
        case SList(x::y) => (ctx, x)
        case _ => error
      }
      case _ => error
    }
  }

  def _cdr(ctx: Context, comb: List[SExpression]): (Context, SExpression) = {
  
    def error = throw new IllegalArgumentException("car")
    comb match {
      case h::t => eval(ctx,h)._2 match {
        case SList(List()) => (ctx, SList(List()))
        case SList(x::y) => (ctx, SList(y))
        case _ => error
      }
      case _ => error
    }
  }

  def _null(ctx: Context, comb: List[SExpression]): (Context, SExpression) = {
    def error = throw new IllegalArgumentException("null")
    comb match {
      case h :: Nil => eval(ctx, h)._2 match {
        case SList(List()) => (ctx, Value(Bool(true)))
        case _ => (ctx, Value(Bool(false)))
      }
      case _ => error
    }
  }

  def _let(ctx: Context, comb: List[SExpression]): (Context, SExpression) = {
  
    def error = throw new IllegalArgumentException("let")

    def bindLocal(ctx: Context, binds: List[SExpression]): Context = binds match {
      case List()         => ctx
      case SCombination(c) :: t   => c match {
        case Symbol(v) :: expr :: Nil =>
          bindLocal(ctx.addEntry(v -> eval(ctx, expr)._2), t)
        case _                        => error
      }
      case _              => error
    }

    comb match {
      case SCombination(args) :: body :: Nil => {
        val newCtx = bindLocal(ctx.expand, args)  
        eval(newCtx, body)
      }
      case _ => error
    }
  }

  def _lambda(ctx: Context, comb: List[SExpression]): (Context, SExpression) = {
    def error = throw new IllegalArgumentException("lambda")
    comb match {
      case SCombination(args) :: body => (ctx, SFunc(buildList(args), body))
      case _ => error
    }
  }

  val globalCtx = Context(List(Map(

        ("+" ->       SProc(aritFun(_+_) _)),
        ("-" ->       SProc(aritFun(_-_) _)),
        ("*" ->       SProc(aritFun(_*_) _)),
        ("/" ->       SProc(aritFun(_/_) _)),

        ("=" ->       SProc(compFun(_==_) _)),
        (">" ->       SProc(compFun(_>_) _)),
        ("<" ->       SProc(compFun(_<_) _)),
        (">=" ->      SProc(compFun(_>=_) _)),
        ("<=" ->      SProc(compFun(_<=_) _)),

        ("not" ->     SProc(_not)),
        ("if" ->      SProc(_if)),
        ("cond" ->    SProc(_cond)),
        ("define" ->  SProc(_define)),
        ("cons" ->    SProc(_cons)),
        ("list" ->    SProc(_list)),
        ("car" ->     SProc(_car)),
        ("cdr" ->     SProc(_cdr)),
        ("null?" ->   SProc(_null)),
        ("let" ->     SProc(_let)),
        ("lambda" ->  SProc(_lambda)),

        ("true" ->    Value(Bool(true))),
        ("false" ->   Value(Bool(false)))

    )))
}

