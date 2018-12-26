package org.jetbrains.plugins.scala.lang.psi.util

import com.intellij.psi.PsiConstantEvaluationHelper.AuxEvaluator
import com.intellij.psi.PsiElement
import com.intellij.psi.impl.ConstantExpressionEvaluator
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScInterpolatedStringLiteral, ScLiteral}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScPatternDefinition}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.ScalaElementType
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScMember, ScObject}

/**
  * @author Alexander Podkhalyuzin
  */

class ScalaConstantExpressionEvaluator extends ConstantExpressionEvaluator {
  def computeExpression(expression: PsiElement, throwExceptionOnOverflow: Boolean,
                        auxEvaluator: AuxEvaluator): AnyRef = computeConstantExpression(expression, throwExceptionOnOverflow)

  private def isInFinalClass(member: ScMember): Boolean = {
    val containing = member.containingClass
    containing.isInstanceOf[ScClass] && containing.hasModifierProperty("final")
  }

  private def isInFinalClass(member: ScReferencePattern): Boolean = {
    val containing = member.containingClass
    containing.isInstanceOf[ScClass] && containing.hasModifierProperty("final")
  }

  def computeConstantExpression(expression: PsiElement, throwExceptionOnOverflow: Boolean = false): AnyRef = {
    expression match {
      case patternDef: ScPatternDefinition => patternDef.expr.map(computeConstantExpression(_)).orNull
      case fun: ScFunctionDefinition
        if isInFinalClass(fun) || fun.hasModifierProperty("final") ||
          fun.containingClass.isInstanceOf[ScObject] || fun.containingClass == null =>
        fun.body.map(computeConstantExpression(_)).orNull
      case pattern: ScReferencePattern
        if pattern.isVal && (isInFinalClass(pattern) ||
          pattern.getModifierList.hasModifierProperty("final") || pattern.containingClass.isInstanceOf[ScObject] ||
          pattern.containingClass == null) =>
        computeConstantExpression(pattern.nameContext)
      case ref: ScReferenceExpression => computeConstantExpression(ref.resolve())
      case interpolated: ScInterpolatedStringLiteral =>
        val children = interpolated.children.toList
        val interpolatedPrefix = children.headOption
        if (interpolatedPrefix.exists(prefix => prefix.getNode.getElementType != ScalaElementType.INTERPOLATED_PREFIX_LITERAL_REFERENCE || prefix.getText != "s")) null
        else {
          val quotes = children(1).getText
          children.foldLeft(("", false)) {
            case ((acc, expectingRef), child) =>
              def evaluateHelper(expr: ScExpression): Option[(String, Boolean)] = Option(computeConstantExpression(expr)).map { it => (acc + it, false) }

              child match {
                case block: ScBlockExpr => block.lastExpr.flatMap(evaluateHelper).getOrElse(return null)
                case ref: ScReferenceExpression if expectingRef => evaluateHelper(ref).getOrElse(return null)
                case _ if child.getNode.getElementType == ScalaTokenTypes.tINTERPOLATED_STRING_INJECTION => (acc, true)
                case _ if (child.getText != quotes) && child.getNode.getElementType == ScalaTokenTypes.tINTERPOLATED_STRING => (acc + child.getText, false)
                case _ => (acc, false)
              }
          }._1
        }
      case l: ScLiteral => l.getValue
      case p: ScParenthesisedExpr => p.innerElement match {
        case Some(e) => computeConstantExpression(e)
        case _ => null
      }
      case infix: ScInfixExpr =>
        val left = computeConstantExpression(infix.left)
        val right = computeConstantExpression(infix.right)
        if (left == null || right == null) null
        else LiteralEvaluationUtil.evaluateConstInfix(left, right, true, infix.operation.refName, value => Some(value)).orNull.asInstanceOf[AnyRef]
      case prefix: ScPrefixExpr =>
        val expr = computeConstantExpression(prefix.operand)
        if (expr == null) null
        else LiteralEvaluationUtil.evaluateConstPrefix(expr, prefix.operation.refName).orNull.asInstanceOf[AnyRef]
      case _ => null
    }
  }
}