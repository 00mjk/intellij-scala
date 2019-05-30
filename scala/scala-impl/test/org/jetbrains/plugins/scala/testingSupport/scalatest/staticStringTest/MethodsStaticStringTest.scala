package org.jetbrains.plugins.scala.testingSupport.scalatest.staticStringTest

import org.jetbrains.plugins.scala.testingSupport.scalatest.ScalaTestTestCase

/**
  * @author Roman.Shein
  * @since 02.07.2015.
  */
trait MethodsStaticStringTest extends ScalaTestTestCase {
  val methodsClassName = "ScalaTestMethodsTest"
  val methodsFileName = methodsClassName + ".scala"

  addSourceFile(methodsFileName,
    s"""
      |import org.scalatest._
      |
      |class $methodsClassName extends PropSpec {
      |  property(" testName ".trim + "1") {
      |  }
      |
      |  property("TeStNaMe2".toLowerCase) {
      |  }
      |
      |  property("testName3Suffix".stripSuffix("Suffix")) {
      |  }
      |
      |  property("prefixtestName4".stripPrefix("prefix")) {
      |  }
      |
      |  property("junktestName5".substring(4)) {
      |  }
      |
      |  property("JunktestName6Junk".substring(4, 13)) {
      |  }
      |
      |  property("testReplace7".replace("Replace", "Name")) {
      |  }
      |}
      |
    """.stripMargin.trim)

  def testTrim() = {
    assertConfigAndSettings(createTestFromLocation(3, 7, methodsFileName), methodsClassName, "testName1")
  }

  def testToLowerCase() = {
    assertConfigAndSettings(createTestFromLocation(6, 7, methodsFileName), methodsClassName, "testname2")
  }

  def testStripSuffix() = {
    assertConfigAndSettings(createTestFromLocation(9, 7, methodsFileName), methodsClassName, "testName3")
  }

  def testStripPrefix() = {
    assertConfigAndSettings(createTestFromLocation(12, 7, methodsFileName), methodsClassName, "testName4")
  }

  def testSubstring1() = {
    assertConfigAndSettings(createTestFromLocation(15, 7, methodsFileName), methodsClassName, "testName5")
  }

  def testSubstring2() = {
    assertConfigAndSettings(createTestFromLocation(18, 7, methodsFileName), methodsClassName, "testName6")
  }

  def testReplace() = {
    assertConfigAndSettings(createTestFromLocation(21, 7, methodsFileName), methodsClassName, "testName7")
  }
}
