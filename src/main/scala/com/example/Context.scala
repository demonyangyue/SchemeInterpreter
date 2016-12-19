package com.example

import SchemeInterpreter._

case class Context(val get: List[Map[String, SExpression]]) {
  // used to record global and local variables
  def addEntry(entry: (String, SExpression)): Context = get match {
    // if the key already exists, then we will overwrite the value
    case h::t   => Context((h + entry) :: t)
    case List() => throw new IllegalArgumentException
  }

  def expand(): Context = Context(Map[String, SExpression]() :: get)

  def lookUp(s: String): Option[SExpression] = 
    get find (_ contains s) map (_(s))

  override def equals(that: Any) = that match {
    case Context(that) => get == that
    case _ => false
  }
}

object Context {
  def apply() = new Context(List(Map[String, SExpression]()))
}
