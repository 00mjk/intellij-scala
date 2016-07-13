package org.jetbrains.plugins.scala.codeInspection.unusedInspections

import com.intellij.codeInspection.{LocalInspectionTool, LocalQuickFixAndIntentionActionOnPsiElement, ProblemsHolder}
import com.intellij.psi.{PsiElement, PsiElementVisitor}

/**
  * Created by Svyatoslav Ilinskiy on 13.07.16.
  */
trait HighlightingPassInspection extends LocalInspectionTool {
  override final def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitor = {
    if (isOnTheFly) PsiElementVisitor.EMPTY_VISITOR //highlighting pass should take care of that
    else {
      new PsiElementVisitor {
        override def visitElement(element: PsiElement): Unit = {
          invoke(element).foreach { info =>
            holder.registerProblem(info.element, info.message, info.fixes: _*)
          }
        }
      }
    }
  }

  def invoke(element: PsiElement): Seq[ProblemInfo]

  def shouldProcessElement(elem: PsiElement): Boolean
}

case class ProblemInfo(element: PsiElement, message: String, fixes: Seq[LocalQuickFixAndIntentionActionOnPsiElement])
