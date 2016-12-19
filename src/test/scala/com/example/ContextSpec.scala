package com.example

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll
import SchemeInterpreter._

class ContextSpec extends FlatSpec with Matchers with BeforeAndAfterAll {

  "context" should "find corresponding entry" in {
    val context = Context()
    val newContext = context.addEntry("name", Symbol("yy"))
    newContext.lookUp("name").get shouldBe Symbol("yy")
    newContext.lookUp("age") shouldBe None
  }

  it should "expand successfully" in {
    val context = Context().addEntry("name", Symbol("yang"))
    val newContext = context.expand().addEntry("name", Symbol("yue"))
    newContext.get.length shouldBe 2
    newContext.lookUp("name").get shouldBe Symbol("yue")
  }
}
