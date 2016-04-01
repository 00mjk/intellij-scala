package org.jetbrains.plugins.scala.failed.typeInference

import org.jetbrains.plugins.scala.PerfCycleTests
import org.jetbrains.plugins.scala.lang.typeInference.TypeInferenceTestBase
import org.junit.experimental.categories.Category

/**
  * @author Nikolay.Tropin
  */
@Category(Array(classOf[PerfCycleTests]))
class ContravarianceTest extends TypeInferenceTestBase {
  def testScl4123() = {
    val text =
      s"""object Test {
        |  class A
        |  class C
        |  class B extends C
        |
        |  class Z[-T] //in case of covariant or invariant, all is ok
        |
        |  def goo[A, BB >: A](x: A): Z[BB] = new Z[BB]
        |  val zzzzzz = goo(new B) //here type is Z[Any], according to the compiler it's Z[B]
        |  ${START}zzzzzz$END
        |}
        |
        |//Test.Z[B]""".stripMargin
    doTest(text)
  }

  def testSCL10110(): Unit ={
    doTest(
      s"""
         |object Error {
         |
         |  class Foo[T](x: T)
         |
         |  class LolArray[T](val arr: Array[Foo[T]])
         |
         |  class LolImmutableHashMap[T](val arr: immutable.HashMap[Int, Foo[T]])
         |
         |  //Full example with various collections in corresponded ticket
         |  def main(args: Array[String]) {
         |    val lolArray = new LolArray(${START}Array(new Foo(1))$END) // false error ( Array invariant )
         |    val lolImmutableHashMap = new LolImmutableHashMap(immutable.HashMap(1 -> new Foo(1))) // works ( mutable.HashMap covariant )
         |
         |    //    val lolArrayExplicit1 = new LolArray[Int](Array(new Foo(1))) // works
         |    //    val lolArrayExplicit2 = new LolArray(Array[Foo[Int]](new Foo(1))) // works
         |  }
         |}
         |
         |//Array[Error.Foo[NotInferedT]]
       """.stripMargin)
  }
}
