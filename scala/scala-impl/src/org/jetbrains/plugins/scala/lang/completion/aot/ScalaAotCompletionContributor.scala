package org.jetbrains.plugins.scala
package lang
package completion
package aot

import com.intellij.codeInsight.completion._
import com.intellij.codeInsight.lookup._
import com.intellij.psi.PsiElement
import com.intellij.util.ProcessingContext
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScFieldId, ScPrimaryConstructor}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScFunctionExpr
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScParameter, ScParameterClause, ScParameterType, ScParameters}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScValueDeclaration, ScVariableDeclaration}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory

import scala.collection.mutable

/**
  * @author Pavel Fatin
  */
final class ScalaAotCompletionContributor extends ScalaCompletionContributor {

  import ScalaAotCompletionContributor._

  //noinspection ConvertExpressionToSAM
  extend(
    new ParameterCompletionProvider {

      override protected def createConsumer(resultSet: CompletionResultSet, position: PsiElement): Consumer = new TypedConsumer(resultSet) {

        override protected def createInsertHandler(itemText: String): InsertHandler = new InsertHandler(itemText) {

          override def handleInsert(decorator: Decorator)
                                   (implicit context: InsertionContext): Unit = {
            super.handleInsert(decorator)
            inReplaceMode { context =>
              context.getEditor.getCaretModel.moveToOffset(context.getTailOffset)
            }
          }

          override protected def handleReplace(implicit context: InsertionContext): Unit = {
            inReplaceMode { context =>
              for {
                parameter <- position.parentOfType(classOf[ScParameter])

                typeElement <- findTypeElement(parameter)
                bound = typeElement.getTextRange.getEndOffset

                identifier <- findIdentifier(parameter)
                origin = identifier.getTextRange.getEndOffset

                length = bound - origin
              } context.offsetMap(InsertionContext.TAIL_OFFSET) += length
            }

            super.handleReplace

            val delta = itemText.indexOf(Delimiter) + Delimiter.length
            context.offsetMap(CompletionInitializationContext.START_OFFSET) += delta
          }

          private def inReplaceMode(action: InsertionContext => Unit)
                                   (implicit context: InsertionContext): Unit =
            context.getCompletionChar match {
              case Lookup.REPLACE_SELECT_CHAR => action(context)
              case _ =>
            }
        }
      }
    },
    functionType = classOf[ScFunction]
  )

  extend(
    new ParameterCompletionProvider {

      override protected def createConsumer(resultSet: CompletionResultSet, position: PsiElement): Consumer = new TypedConsumer(resultSet)

      override protected def createElement(text: String,
                                           context: PsiElement,
                                           child: PsiElement): ScParameter =
        ScalaPsiElementFactory.createClassParamClausesWithContext(text.parenthesize(), context)
          .params.head
    },
    functionType = classOf[ScPrimaryConstructor]
  )

  extend(
    (resultSet: CompletionResultSet, _: PsiElement) => new UntypedConsumer(resultSet),
    functionType = classOf[ScFunctionExpr]
  )

  extend(
    CompletionType.BASIC,
    identifierWithParentPattern(classOf[ScFieldId]),
    new CompletionProvider[ScValueDeclaration] {

      override protected def addCompletions(resultSet: CompletionResultSet,
                                            prefix: String)
                                           (implicit parameters: CompletionParameters,
                                            context: ProcessingContext): Unit =
        positionFromParameters.getContext.getContext.getContext match {
          case _: ScVariableDeclaration | _: ScValueDeclaration => super.addCompletions(resultSet, prefix)
          case _ =>
        }

      override protected def createElement(text: String,
                                           context: PsiElement,
                                           child: PsiElement): ScValueDeclaration =
        ScalaPsiElementFactory.createDeclarationFromText(ScalaKeyword.VAL + " " + text, context, child)
          .asInstanceOf[ScValueDeclaration]

      override protected def findTypeElement(declaration: ScValueDeclaration): Option[ScTypeElement] =
        declaration.typeElement

      override protected def findContext(element: ScValueDeclaration): PsiElement =
        super.findContext(element).getContext.getContext

      override protected def createConsumer(resultSet: CompletionResultSet, position: PsiElement): Consumer = new UntypedConsumer(resultSet) {

        private val consumed = mutable.Set.empty[String]

        override protected def consume(lookupElement: LookupElement, itemText: String): Unit = {
          if (consumed.add(itemText)) super.consume(lookupElement, itemText)
        }
      }
    }
  )

  private def extend(provider: ParameterCompletionProvider,
                     functionType: Class[_ <: ScalaPsiElement]): Unit = extend(
    CompletionType.BASIC,
    identifierPattern.withParents(classOf[ScParameter], classOf[ScParameterClause], classOf[ScParameters], functionType),
    provider
  )
}

object ScalaAotCompletionContributor {

  private trait ParameterCompletionProvider extends CompletionProvider[ScParameter] {

    override protected def createElement(text: String,
                                         context: PsiElement,
                                         child: PsiElement): ScParameter =
      ScalaPsiElementFactory.createParamClausesWithContext(text.parenthesize(), context, child)
        .params.head

    override protected def findTypeElement(parameter: ScParameter): Option[ScParameterType] =
      parameter.paramType
  }

}
