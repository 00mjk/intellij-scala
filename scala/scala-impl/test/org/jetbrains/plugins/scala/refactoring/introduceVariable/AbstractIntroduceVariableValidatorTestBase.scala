package org.jetbrains.plugins.scala.refactoring.introduceVariable

import com.intellij.openapi.editor.{Editor, SelectionModel}
import com.intellij.openapi.fileEditor.{FileEditorManager, OpenFileDescriptor}
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil.{findCommonParent, getParentOfType}
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.actions.ActionTestBase
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlock, ScExpression}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateBody
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil.{getExpression, getTypeElement}
import org.jetbrains.plugins.scala.lang.refactoring.util._
import org.jetbrains.plugins.scala.util.TestUtils._

abstract class AbstractIntroduceVariableValidatorTestBase(kind: String)
  extends ActionTestBase("introduceVariable", "validator", kind) {

  import AbstractIntroduceVariableValidatorTestBase._

  override def transform(testName: String, data: Array[String]): String = {
    super.transform(testName, data)

    val psiFile = createScalaFileFrom(data)
    processFile(psiFile)
  }

  private def processFile(file: PsiFile): String = {
    var fileText = file.getText

    var startOffset = fileText.indexOf(BEGIN_MARKER)
    val replaceAllOccurrences = startOffset < 0

    fileText = if (replaceAllOccurrences) {
      startOffset = fileText.indexOf(ALL_MARKER)

      myOffset = startOffset - 1
      removeMarker(fileText, ALL_MARKER, startOffset)
      fileText.substring(0, startOffset) + fileText.substring(startOffset + ALL_MARKER.length)
    } else {
      removeBeginMarker(fileText, startOffset)
    }

    val endOffset = fileText.indexOf(END_MARKER)
    fileText = removeEndMarker(fileText, endOffset)

    val fileEditorManager = FileEditorManager.getInstance(myProject)
    val editor = fileEditorManager.openTextEditor(new OpenFileDescriptor(myProject, file.getVirtualFile, 0), false)
    editor.getSelectionModel.setSelection(startOffset, endOffset)

    try {
      val maybeValidator = getValidator(file)(myProject, editor)
      maybeValidator.toSeq
        .flatMap(_.findConflicts(getName(fileText), replaceAllOccurrences))
        .map(_._2)
        .toSet[String]
        .mkString("\n")
    } finally {
      fileEditorManager.closeFile(file.getVirtualFile)
    }
  }

  protected def getName(fileText: String): String
}

object AbstractIntroduceVariableValidatorTestBase {
  private val ALL_MARKER = "<all>"

  def getValidator(file: PsiFile)
                  (implicit project: Project, editor: Editor): Option[ScalaValidator] = {
    implicit val selectionModel: SelectionModel = editor.getSelectionModel

    getParentOfType(file.findElementAt(selectionModel.getSelectionStart), classOf[ScExpression], classOf[ScTypeElement]) match {
      case _: ScExpression => getExpression(file).map(getVariableValidator(_, file))
      case _: ScTypeElement => getTypeElement(file).map(getTypeValidator(_, file))
      case _ => None
    }
  }

  import ScalaRefactoringUtil._

  private[this] def getContainerOne(file: PsiFile, length: Int)
                                   (implicit selectionModel: SelectionModel): PsiElement = {
    val origin = file.findElementAt(selectionModel.getSelectionStart)
    val bound = file.findElementAt(selectionModel.getSelectionEnd - 1)

    val commonParentOne = findCommonParent(origin, bound)

    val classes = Seq(classOf[ScalaFile], classOf[ScBlock], classOf[ScTemplateBody])
    (length match {
      case 1 => commonParentOne.parentOfType(classes)
      case _ => commonParentOne.nonStrictParentOfType(classes)
    }).orNull
  }

  private[this] def getVariableValidator(expression: ScExpression, file: PsiFile)
                                        (implicit selectionModel: SelectionModel): ScalaVariableValidator = {
    val occurrences = getOccurrenceRanges(expression, fileEncloser(file).orNull)
    val containerOne = getContainerOne(file, occurrences.length)

    val parent = commonParent(file, occurrences)
    new ScalaVariableValidator(expression, occurrences.isEmpty, enclosingContainer(parent), containerOne)
  }

  private[this] def getTypeValidator(typeElement: ScTypeElement, file: PsiFile)
                                    (implicit selectionModel: SelectionModel): ScalaTypeValidator = {
    val occurrences = getTypeElementOccurrences(typeElement, fileEncloser(file).orNull)
    val containerOne = getContainerOne(file, occurrences.length)

    val parent = findCommonParent(occurrences: _*)
    new ScalaTypeValidator(typeElement, occurrences.isEmpty, enclosingContainer(parent), containerOne)
  }

}
