package org.jetbrains.plugins.scala.testingSupport.utest.scala2_10.utest_0_4_3

import org.jetbrains.plugins.scala.base.libraryLoaders.{QuasiQuotesLoader, ThirdPartyLibraryLoader, UTestLoader}
import org.jetbrains.plugins.scala.debugger.{ScalaVersion, Scala_2_10}
import org.jetbrains.plugins.scala.testingSupport.utest.UTestTestCase

/**
  * @author Roman.Shein
  * @since 02.09.2015.
  */
abstract class UTestTestBase_2_10_0_4_3 extends UTestTestCase {

  override implicit val version: ScalaVersion = Scala_2_10

  override protected def additionalLibraries: Seq[ThirdPartyLibraryLoader] = {
    implicit val module = getModule
    Seq(UTestLoader("0.4.3"), QuasiQuotesLoader())
  }

  override protected val testSuiteSecondPrefix = ""
}
