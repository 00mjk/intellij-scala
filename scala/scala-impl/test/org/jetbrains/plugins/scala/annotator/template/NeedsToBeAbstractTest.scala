package org.jetbrains.plugins.scala
package annotator
package template

/**
  * Pavel Fatin
  */
class NeedsToBeAbstractTest extends AnnotatorTestBase {

  def testFine(): Unit = {
    assertNothing(messages("class C"))
    assertNothing(messages("class C {}"))
    assertNothing(messages("trait T"))
    assertNothing(messages("trait T {}"))
    assertNothing(messages("abstract class C"))
    assertNothing(messages("abstract class C {}"))
    assertNothing(messages("abstract class C { def f }"))
    assertNothing(messages("trait T { def f }"))
  }

  def testSkipNew(): Unit = {
    assertMatches(messages("trait T { def f }; new Object with T")) {
      case Error("Object", "Object creation impossible, since  member f: Unit in Holder.T is not defined") :: Nil =>
    }
  }

  def testSkipObject(): Unit = {
    assertMatches(messages("trait T { def f }; object O extends T")) {
      case Error("O", "Object creation impossible, since  member f: Unit in Holder.T is not defined") :: Nil =>
    }
  }

  def testUndefinedMember(): Unit = {
    val message = this.message("Class", "C", "f: Unit", "Holder.C")

    assertMatches(messages("class C { def f }")) {
      case Error("C", `message`) :: Nil =>
    }
  }

  def testUndefinedInheritedMember(): Unit = {
    val message = this.message("Class", "C", "f: Unit", "Holder.T")

    assertMatches(messages("trait T { def f }; class C extends T")) {
      case Error("C", `message`) :: Nil =>
    }

    assertMatches(messages("trait T { def f }; class C extends T {}")) {
      case Error("C", `message`) :: Nil =>
    }
  }

  def testNeedsToBeAbstractPlaceDiffer(): Unit = {
    val message = this.message("Class", "C", "b: Unit", "Holder.B")
    val reversedMessage = this.message("Class", "C", "a: Unit", "Holder.A")

    assertMatches(messages("trait A { def a }; trait B { def b }; class C extends A with B {}")) {
      case Error("C", `message`) :: Nil =>
      case Error("C", `reversedMessage`) :: Nil =>
    }
  }

  def testObjectOverrideDef(): Unit = {
    assertMatches(messages("trait A { def a }; class D extends A { object a };")) {
      case Nil =>
    }
  }

  private def message(params: String*) =
    ScalaBundle.message("member.implementation.required", params: _*)
}