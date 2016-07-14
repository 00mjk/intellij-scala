package org.jetbrains.plugins.scala
package lang.psi.stubs

import com.intellij.psi.stubs.{DefaultStubBuilder, StubElement}
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.stubs.impl.ScFileStubImpl

/**
 * User: Alexander Podkhalyuzin
 * Date: 12.02.2010
 */
class ScalaFileStubBuilder extends DefaultStubBuilder {
  protected override def createStubForFile(file: PsiFile): StubElement[_ <: PsiElement] =
    new ScFileStubImpl(file.getViewProvider
      .getPsi(ScalaLanguage.Instance).asInstanceOf[ScalaFile])
}