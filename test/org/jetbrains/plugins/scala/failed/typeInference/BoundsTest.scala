package org.jetbrains.plugins.scala.failed.typeInference

import org.jetbrains.plugins.scala.PerfCycleTests
import org.jetbrains.plugins.scala.lang.typeInference.TypeInferenceTestBase
import org.junit.experimental.categories.Category

/**
  * @author Alefas
  * @since 23/03/16
  */
@Category(Array(classOf[PerfCycleTests]))
class BoundsTest extends TypeInferenceTestBase {
  override def folderPath: String = super.folderPath + "bugs5/"

  def testSCL4373(): Unit = doTest() //blinking test

  def testSCL5215(): Unit = doTest()

  def testSCL7085(): Unit = doTest()

  def testSCL9755(): Unit = {
    val text =
      s"""object IntelliJ {
        |  trait Base[T]
        |  case object StringExample extends Base[String]
        |
        |  implicit val baseStringEvidence = StringExample
        |
        |  def apply[T: Base](semantic: Base[T]): T = semantic match {
        |    case StringExample => $START"string"$END
        |  }
        |}
        |//T""".stripMargin
    doTest(text)
  }

  def testSCL5183(): Unit = {
    doTest(
      s"""
        |class D
        |def foo[Q >: List[T], T >: D](): Q = List(new D)
        |
        |val x = foo()
        |
        |${START}x$END
        |//List[D]
      """.stripMargin)
  }
}
