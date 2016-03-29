package org.jetbrains.plugins.scala.failed.resolve

import org.jetbrains.plugins.scala.lang.psi.api.base.ScReferenceElement
import org.junit.Assert._

/**
  * @author Nikolay.Tropin
  */
class UpdateMethodTest extends FailedResolveTest("updateMethod") {
  def testSCL5739() = {
    findReferenceAtCaret() match {
      case ref: ScReferenceElement =>
        val variants = ref.multiResolve(false)
        assertTrue(s"Single resolve expected, was: ${variants.length}", variants.length == 1)

        val elementFile = variants(0).getElement.getContainingFile
        assertTrue(s"Should resolve to the same file, was: ${elementFile.getName}", elementFile == ref.getContainingFile)
    }
  }
}
