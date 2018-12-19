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
import org.jetbrains.plugins.scala.lang.psi.types.result._

/**
* @author Alexander Podkhalyuzin
* Date: 28.02.2008
*/

class ScInfixPatternImpl(node: ASTNode) extends ScalaPsiElementImpl (node) with ScInfixPattern {
  override def isIrrefutableFor(t: Option[ScType]): Boolean =
    ScConstructorPattern.isIrrefutable(t, operation, left :: rightOption.toList)

  override def accept(visitor: PsiElementVisitor) {
    visitor match {
      case visitor: ScalaElementVisitor => super.accept(visitor)
      case _ => super.accept(visitor)
    }
  }

  override def toString: String = "InfixPattern"

  override def `type`(): TypeResult = {
    this.expectedType match {
      case Some(x) => Right(x)
      case _ => Failure("cannot define expected type")
    }
  }
}