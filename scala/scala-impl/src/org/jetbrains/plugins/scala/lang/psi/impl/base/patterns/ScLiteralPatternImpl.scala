package org.jetbrains.plugins.scala
package lang
package psi
package impl
package base
package patterns

import com.intellij.lang.ASTNode
import com.intellij.psi._
import org.jetbrains.plugins.scala.lang.psi.api.ScalaElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns._
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult

/** 
* @author Alexander Podkhalyuzin
* Date: 28.02.2008
*/

class ScLiteralPatternImpl(node: ASTNode) extends ScalaPsiElementImpl(node) with ScPatternImpl with ScLiteralPattern {
  override def isIrrefutableFor(t: Option[ScType]): Boolean = false

  override def accept(visitor: PsiElementVisitor) {
    visitor match {
      case visitor: ScalaElementVisitor => super.accept(visitor)
      case _ => super.accept(visitor)
    }
  }

  override def toString: String = "LiteralPattern"

  override def `type`(): TypeResult = getLiteral.`type`()
}