package org.jetbrains.plugins.scala.annotator.gutter

import java.io.File

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzerSettings
import com.intellij.codeInsight.daemon.impl.DaemonCodeAnalyzerImpl
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.markup.SeparatorPlacement
import com.intellij.openapi.project.Project
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase
import com.intellij.testFramework.fixtures.impl.JavaCodeInsightTestFixtureImpl
import org.jetbrains.plugins.scala.{ScalaFileType, TypecheckerTests}
import org.jetbrains.plugins.scala.extensions.using
import org.jetbrains.plugins.scala.util.TestUtils
import org.junit.Assert._
import org.junit.experimental.categories.Category

import scala.collection.JavaConverters._
import scala.io.Source

/**
 * Pavel.Fatin, 14.01.2010
 */
@Category(Array(classOf[TypecheckerTests]))
abstract class LineMarkerTestBase extends LightCodeInsightFixtureTestCase {
  val marker = "// -"

  protected override def getBasePath = TestUtils.getTestDataPath + "/methodSeparator/"


  override def setUp(): Unit = {
    super.setUp()
    myFixture.setTestDataPath(getBasePath)
  }

  def doTest(): Unit = {
    val path = getBasePath + getTestName(false) + ".test"
    val input = using(Source.fromFile(new File(path)))(_.getLines().mkString("\n"))
    myFixture.configureByText(ScalaFileType.INSTANCE, input.replaceAll(marker, ""))

    DaemonCodeAnalyzerSettings.getInstance.SHOW_METHOD_SEPARATORS = true
    myFixture.asInstanceOf[JavaCodeInsightTestFixtureImpl].doHighlighting()

    val expected = getSeparatorsFrom(input)
    val actual = getSeparatorsFrom(myFixture.getEditor, myFixture.getProject)
    assertEquals(expected.mkString(", "), actual.mkString(", "))
  }

  def getSeparatorsFrom(text: String) = {
    for{(line, i) <- text.split("\n").zipWithIndex
      if line.contains(marker)} yield i + 1
  }

  def getSeparatorsFrom(editor: Editor, project: Project) = {
    val separators =
      for{
        each <- DaemonCodeAnalyzerImpl.getLineMarkers(editor.getDocument, project).asScala
        if each.separatorPlacement == SeparatorPlacement.TOP
        index = editor.getDocument.getLineNumber(each.getElement.getTextRange.getStartOffset)
      } yield index + 1

    separators.sortWith(_ < _)
  }
}
