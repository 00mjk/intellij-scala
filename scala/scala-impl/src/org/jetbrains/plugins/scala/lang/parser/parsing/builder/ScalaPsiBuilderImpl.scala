package org.jetbrains.plugins.scala
package lang
package parser.parsing
package builder

import com.intellij.lang.{PsiBuilder, impl}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.impl.source.resolve.FileContextUtil
import org.jetbrains.plugins.scala.lang.parser.ScalaElementType
import org.jetbrains.plugins.scala.settings.ScalaFeatureSettings

/**
  * @author Alexander Podkhalyuzin
  */
class ScalaPsiBuilderImpl(delegate: PsiBuilder,
                          override val isScala3: Boolean = false)
  extends impl.PsiBuilderAdapter(delegate)
    with ScalaPsiBuilder {

  import lexer.ScalaTokenTypes._
  import util.ScalaUtil._

  implicit def project: Project = getProject

  private val newlinesEnabled = new collection.mutable.Stack[Boolean]

  private lazy val containingFile = Option {
    myDelegate.getUserData(FileContextUtil.CONTAINING_FILE_KEY)
  }

  private lazy val (_isTrailingCommasEnabled, _isIdBindingEnabled) =
    areTrailingCommasAndIdBindingEnabled(containingFile)

  override def isTrailingComma: Boolean = getTokenType match {
    case `tCOMMA` => _isTrailingCommasEnabled
    case _ => false
  }

  override def isIdBinding: Boolean =
    this.invalidVarId || _isIdBindingEnabled

  override lazy val isMetaEnabled: Boolean = containingFile.exists {
    import meta.intellij.psi._
    _.isMetaEnabled
  }

  override def newlineBeforeCurrentToken: Boolean =
    findPreviousNewLineSafe.isDefined

  override def twoNewlinesBeforeCurrentToken: Boolean =
    findPreviousNewLineSafe.exists { text =>
      s"start $text end".split('\n').exists { line =>
        line.forall(StringUtil.isWhiteSpace)
      }
    }

  override final def disableNewlines(): Unit = {
    newlinesEnabled.push(false)
  }

  override final def enableNewlines(): Unit = {
    newlinesEnabled.push(true)
  }

  override final def restoreNewlinesState(): Unit = {
    assert(newlinesEnabled.nonEmpty)
    newlinesEnabled.pop()
  }

  protected final def isNewlinesEnabled: Boolean =
    newlinesEnabled.isEmpty || newlinesEnabled.top

  private def findPreviousNewLineSafe =
    if (isNewlinesEnabled && canStartStatement) this.findPreviousNewLine
    else None

  private def canStartStatement: Boolean = getTokenType match {
    case null => false
    case `kCATCH` |
         `kELSE` |
         `kEXTENDS` |
         `kFINALLY` |
         `kMATCH` |
         `kWITH` |
         `kYIELD` |
         `tCOMMA` |
         `tDOT` |
         `tSEMICOLON` |
         `tCOLON` |
         `tASSIGN` |
         `tFUNTYPE` |
         `tCHOOSE` |
         `tUPPER_BOUND` |
         `tLOWER_BOUND` |
         `tVIEW` |
         `tINNER_CLASS` |
         `tLSQBRACKET` |
         `tRSQBRACKET` |
         `tRPARENTHESIS` |
         `tRBRACE` => false
    case `kCASE` =>
      this.predict { builder =>
        builder.getTokenType match {
          case `kOBJECT` |
               `kCLASS` |
               `tIDENTIFIER` => true
          case _ =>
            false
        }
      }
    case _ => true
  }

  override def error(message: String): Unit = {
    if (!ScalaFeatureSettings.instanceIn(getProject).enabled) {
      return
    }
    super.error(message)
  }
}