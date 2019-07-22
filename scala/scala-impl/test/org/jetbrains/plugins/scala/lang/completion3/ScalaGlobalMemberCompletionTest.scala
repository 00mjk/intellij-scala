package org.jetbrains.plugins.scala
package lang
package completion3

import com.intellij.codeInsight.completion.CompletionType
import org.jetbrains.plugins.scala.base.libraryLoaders.{LibraryLoader, SourcesLoader}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.completion.lookups.ScalaLookupItem
import org.junit.Assert.{assertEquals, assertTrue}

/**
  * @author Alexander Podkhalyuzin
  */
class ScalaGlobalMemberCompletionTest extends ScalaCodeInsightTestBase {

  import ScalaCodeInsightTestBase._

  override def getTestDataPath: String =
    s"${super.getTestDataPath}globalMember"

  override def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+ SourcesLoader(getTestDataPath)

  def testGlobalMember1(): Unit = doCompletionTest(
    fileText =
      s"""
         |class TUI {
         |  rawObj$CARET
         |}
      """.stripMargin,
    resultText =
      """
        |import rawObject.RawObject1
        |
        |class TUI {
        |  RawObject1.rawObject()
        |}
      """.stripMargin,
    item = "rawObject",
    time = 2
  )

  def testGlobalMember2(): Unit = doCompletionTest(
    fileText =
      s"""
         |class TUI {
         |  globalVal$CARET
         |}
      """.stripMargin,
    resultText =
      """
        |import rawObject.RawObject2
        |
        |class TUI {
        |  RawObject2.globalValue
        |}
      """.stripMargin,
    item = "globalValue",
    time = 2
  )

  def testGlobalMember3(): Unit = doCompletionTest(
    fileText =
      s"""
         |class TUI {
         |  globalVar$CARET
         |}
      """.stripMargin,
    resultText =
      """
        |import rawObject.RawObject3
        |
        |class TUI {
        |  RawObject3.globalVariable
        |}
      """.stripMargin,
    item = "globalVariable",
    time = 2
  )

  def testGlobalMember4(): Unit = doCompletionTest(
    fileText =
      s"""
         |class TUI {
         |  patternVal$CARET
         |}
      """.stripMargin,
    resultText =
      """
        |import rawObject.RawObject4
        |
        |class TUI {
        |  RawObject4.patternValue
        |}
      """.stripMargin,
    item = "patternValue",
    time = 2
  )

  def testGlobalMember5(): Unit = doCompletionTest(
    fileText =
      s"""
         |class TUI {
         |  patternVar$CARET
         |}
      """.stripMargin,
    resultText =
      """
        |import rawObject.RawObject5
        |
        |class TUI {
        |  RawObject5.patternVariable
        |}
      """.stripMargin,
    item = "patternVariable", time = 2
  )

  def testGlobalMember6(): Unit = doCompletionTest(
    fileText =
      s"""
         |import rawObject.RawObject6.importedDef
         |
        |class TUI {
         |  importDe$CARET
         |}
      """.stripMargin,
    resultText =
      """
        |import rawObject.RawObject6.importedDef
        |
        |class TUI {
        |  importedDef()
        |}
        |
      """.stripMargin,
    item = "importedDef",
    time = 2
  )

  def testGlobalMember7(): Unit = checkNoCompletion(
    fileText =
      s"""
         |class TUI {
         |  imposToR$CARET
         |}
       """.stripMargin,
    time = 2,
    completionType = CompletionType.BASIC
  ) {
    _ => true
  }

  def testGlobalMemberJava(): Unit = doCompletionTest(
    fileText =
      s"""
         |class TUI {
         |  activeCoun$CARET
         |}
      """.stripMargin,
    resultText =
      s"""
         |class TUI {
         |  Thread.activeCount()$CARET
         |}
      """.stripMargin,
    item = "activeCount",
    time = 2
  )

  def testGlobalMember8(): Unit = checkNoCompletion(
    fileText =
      s"""
         |object BlahBlahBlahContainer {
         |  private def doSmthPrivate() {}
         |  def doSmthPublic() {}
         |}
         |
         |class Test {
         |  def test() {
         |    dsp$CARET
         |  }
         |}
       """.stripMargin,
    item = "doSmthPrivate",
    time = 2
  )

  def testGlobalMember9(): Unit = {
    configureTest(
      fileText =
        s"""
           |object BlahBlahBlahContainer {
           |  private def doSmthPrivate() {}
           |  def doSmthPublic() {}
           |}
           |
           |class Test {
           |  def test() {
           |    dsp$CARET
           |  }
           |}
       """.stripMargin,
      time = 3
    )

    val condition = lookupItems.exists {
      hasLookupString(_, "doSmthPrivate")
    }
    assertTrue(condition)
  }

  def testGlobalMemberInherited(): Unit = {
    configureTest(
      fileText =
        s"""
           |class Base {
           |  def zeeGlobalDefInherited = 0
           |  val zeeGlobalValInherited = 0
           |}
           |
           |object D1 extends Base {
           |  def zeeGlobalDef = 0
           |  def zeeGlobalVal = 0
           |}
           |
           |package object D2 extends Base
           |
           |class Test {
           |  def test() {
           |    zeeGlobal$CARET
           |  }
           |}
       """.stripMargin,
      time = 3
    )

    val actual = lookupItems.filterBy[ScalaLookupItem]
      .map { lookup =>
        s"${lookup.containingClass.name}.${lookup.getLookupString}"
      }.toSet

    val expected = Set(
      "D1.zeeGlobalDef",
      "D1.zeeGlobalVal",
      "D2.zeeGlobalDefInherited",
      "D2.zeeGlobalValInherited"
    )
    assertEquals(expected, actual)
  }

  def testCompanionObjectConversion(): Unit = doCompletionTest(
    fileText =
      s"""
         |class Foo[T]
         |
         |class Bar[T] {
         |  def bar(): Unit = {}
         |}
         |
         |class BarDouble {
         |  def bar(): Unit = {}
         |}
         |
         |object Foo {
         |  implicit def toBar[T](foo: Foo[T]): Bar[T] = new Bar[T]()
         |
         |  implicit def toBarDouble(foo: Foo[Double]): BarDouble = new BarDouble()
         |}
         |
         |val foo = new Foo[Boolean]()
         |foo.$CARET
       """.stripMargin,
    resultText =
      s"""
         |class Foo[T]
         |
         |class Bar[T] {
         |  def bar(): Unit = {}
         |}
         |
         |class BarDouble {
         |  def bar(): Unit = {}
         |}
         |
         |object Foo {
         |  implicit def toBar[T](foo: Foo[T]): Bar[T] = new Bar[T]()
         |
         |  implicit def toBarDouble(foo: Foo[Double]): BarDouble = new BarDouble()
         |}
         |
         |val foo = new Foo[Boolean]()
         |foo.bar()$CARET
       """.stripMargin,
    item = "bar"
  )

  def testCompanionObjectConversion2(): Unit = doCompletionTest(
    fileText =
      s"""
         |class Foo[T]
         |
         |class Bar[T] {
         |  def bar(): Unit = {}
         |}
         |
         |class BarDouble {
         |  def bar(): Unit = {}
         |}
         |
         |object Conversions {
         |  implicit def toBar[T](foo: Foo[T]): Bar[T] = new Bar[T]()
         |
         |  implicit def toBarDouble(foo: Foo[Double]): BarDouble = new BarDouble()
         |}
         |
         |val foo = new Foo[Double]()
         |foo.$CARET
       """.stripMargin,
    resultText =
      s"""
         |import Conversions.toBar
         |
         |class Foo[T]
         |
         |class Bar[T] {
         |  def bar(): Unit = {}
         |}
         |
         |class BarDouble {
         |  def bar(): Unit = {}
         |}
         |
         |object Conversions {
         |  implicit def toBar[T](foo: Foo[T]): Bar[T] = new Bar[T]()
         |
         |  implicit def toBarDouble(foo: Foo[Double]): BarDouble = new BarDouble()
         |}
         |
         |val foo = new Foo[Double]()
         |foo.bar()$CARET
       """.stripMargin,
    item = "bar",
    time = 2
  )

  def testImportObjectConversion(): Unit = doCompletionTest(
    fileText =
      s"""
         |class Foo[T]
         |
         |class Bar[T] {
         |  def bar(): Unit = {}
         |}
         |
         |trait BarConversions {
         |  implicit def toBar[T](foo: Foo[T]): Bar[T] = new Bar[T]()
         |}
         |
         |class BarDouble {
         |  def bar(): Unit = {}
         |}
         |
         |trait BarDoubleConversions {
         |  implicit def toBarDouble(foo: Foo[Double]): BarDouble = new BarDouble()
         |}
         |
         |object Conversions extends BarConversions with BarDoubleConversions
         |
         |val foo = new Foo[Boolean]()
         |foo.$CARET
       """.stripMargin,
    resultText =
      s"""
         |import Conversions.toBar
         |
         |class Foo[T]
         |
         |class Bar[T] {
         |  def bar(): Unit = {}
         |}
         |
         |trait BarConversions {
         |  implicit def toBar[T](foo: Foo[T]): Bar[T] = new Bar[T]()
         |}
         |
         |class BarDouble {
         |  def bar(): Unit = {}
         |}
         |
         |trait BarDoubleConversions {
         |  implicit def toBarDouble(foo: Foo[Double]): BarDouble = new BarDouble()
         |}
         |
         |object Conversions extends BarConversions with BarDoubleConversions
         |
         |val foo = new Foo[Boolean]()
         |foo.bar()$CARET
       """.stripMargin,
    item = "bar",
    time = 2
  )

  def testImportObjectConversion2(): Unit = doCompletionTest(
    fileText =
      s"""
         |class Foo[T]
         |
         |class Bar[T] {
         |  def bar(): Unit = {}
         |}
         |
         |trait BarConversions {
         |  implicit def toBar[T](foo: Foo[T]): Bar[T] = new Bar[T]()
         |}
         |
         |class BarDouble {
         |  def bar(): Unit = {}
         |}
         |
         |trait BarDoubleConversions {
         |  implicit def toBarDouble(foo: Foo[Double]): BarDouble = new BarDouble()
         |}
         |
         |object Conversions extends BarConversions with BarDoubleConversions
         |
         |val foo = new Foo[Double]()
         |foo.$CARET
       """.stripMargin,
    resultText =
      s"""
         |import Conversions.toBar
         |
         |class Foo[T]
         |
         |class Bar[T] {
         |  def bar(): Unit = {}
         |}
         |
         |trait BarConversions {
         |  implicit def toBar[T](foo: Foo[T]): Bar[T] = new Bar[T]()
         |}
         |
         |class BarDouble {
         |  def bar(): Unit = {}
         |}
         |
         |trait BarDoubleConversions {
         |  implicit def toBarDouble(foo: Foo[Double]): BarDouble = new BarDouble()
         |}
         |
         |object Conversions extends BarConversions with BarDoubleConversions
         |
         |val foo = new Foo[Double]()
         |foo.bar()$CARET
       """.stripMargin,
    item = "bar",
    time = 2
  )

  def testImportStringInterpolator(): Unit = doCompletionTest(
    fileText =
      s"""
         |object StringInterpol {
         |  implicit class Xy(val sc: StringContext) extends AnyVal {
         |    def xy(args: Any*): BigDecimal = BigDecimal(sc.parts.head)
         |  }
         |}
         |
         |object Test {
         |  xy$CARET"abc"
         |}
       """.stripMargin,
    resultText =
      s"""
         |import StringInterpol.Xy
         |
         |object StringInterpol {
         |  implicit class Xy(val sc: StringContext) extends AnyVal {
         |    def xy(args: Any*): BigDecimal = BigDecimal(sc.parts.head)
         |  }
         |}
         |
         |object Test {
         |  xy$CARET"abc"
         |}
        """.stripMargin,
    item = "xy",
    time = 2
  )
}
