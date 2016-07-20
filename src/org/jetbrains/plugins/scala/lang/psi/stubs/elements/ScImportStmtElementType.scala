package org.jetbrains.plugins.scala
package lang
package psi
package stubs
package elements

import com.intellij.lang.ASTNode
import com.intellij.psi.PsiElement
import com.intellij.psi.stubs.{StubElement, StubInputStream, StubOutputStream}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.ScImportStmt
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.imports.ScImportStmtImpl
import org.jetbrains.plugins.scala.lang.psi.stubs.impl.ScImportStmtStubImpl

/**
  * User: Alexander Podkhalyuzin
  * Date: 18.06.2009
  */
class ScImportStmtElementType[Func <: ScImportStmt]
  extends ScStubElementType[ScImportStmtStub, ScImportStmt]("import statement") {
  override def serialize(stub: ScImportStmtStub, dataStream: StubOutputStream): Unit = {
    dataStream.writeName(stub.getImportText)
  }

  override def deserialize(dataStream: StubInputStream, parentStub: StubElement[_ <: PsiElement]): ScImportStmtStub =
    new ScImportStmtStubImpl(parentStub, this, dataStream.readName.toString)

  override def createStub(psi: ScImportStmt, parentStub: StubElement[_ <: PsiElement]): ScImportStmtStub =
    new ScImportStmtStubImpl(parentStub, this, psi.getText)

  override def createElement(node: ASTNode): ScImportStmt = new ScImportStmtImpl(node)

  override def createPsi(stub: ScImportStmtStub): ScImportStmt = new ScImportStmtImpl(stub)
}