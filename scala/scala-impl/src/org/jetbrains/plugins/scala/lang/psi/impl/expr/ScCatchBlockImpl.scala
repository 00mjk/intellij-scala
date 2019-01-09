package org.jetbrains.plugins.scala
package lang
package psi
package impl
package expr

import com.intellij.lang.ASTNode
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.annotator.ScCatchBlockAnnotator
import org.jetbrains.plugins.scala.lang.psi.api.expr._

/** 
 * Author: Alexander Podkhalyuzin
 * Date: 06.03.2008
 */
class ScCatchBlockImpl(node: ASTNode) extends ScExpressionImplBase(node) with ScCatchBlock with ScCatchBlockAnnotator {

  def getLeftParenthesis: Option[PsiElement] = {
    val leftParenthesis = findChildByType[PsiElement](ScalaTokenTypes.tLPARENTHESIS)
    Option(leftParenthesis)
  }

  def getRightParenthesis: Option[PsiElement] = {
    val rightParenthesis = findChildByType[PsiElement](ScalaTokenTypes.tRPARENTHESIS)
    Option(rightParenthesis)
  }

  override def toString: String = "CatchBlock"
}