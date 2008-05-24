package org.jetbrains.plugins.scala.lang.psi

import com.intellij.extapi.psi.PsiFileBase
import com.intellij.lang.Language
import com.intellij.psi._
import com.intellij.psi.scope.PsiScopeProcessor
import org.jetbrains.plugins.scala.ScalaFileType
import com.intellij.psi.tree.TokenSet
import org.jetbrains.annotations.Nullable
import org.jetbrains.plugins.scala.icons.Icons
import com.intellij.lang.ASTNode
import com.intellij.psi.tree.IElementType
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes
import psi.api.toplevel._
import psi.api.toplevel.typedef._
import psi.api.toplevel.packaging._
import com.intellij.pom.java.LanguageLevel
import com.intellij.lang.StdLanguages
import com.intellij.openapi.fileTypes.StdFileTypes
import com.intellij.util.ArrayUtil
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef._

import _root_.scala.collection.mutable._


class ScalaFile (viewProvider: FileViewProvider) extends PsiFileBase (viewProvider, ScalaFileType.SCALA_FILE_TYPE.getLanguage())
with ScalaPsiElement with ScTypeDefinitionOwner with PsiClassOwner {

  override def getViewProvider = viewProvider
  override def getFileType = ScalaFileType.SCALA_FILE_TYPE
  override def toString = "ScalaFile"


  def getUpperDefs = childrenOfType[ScalaPsiElementImpl] (TokenSets.TMPL_DEF_BIT_SET)

  def setPackageName(name: String) = {}

  def getPackagings: Iterable [ScPackaging] = childrenOfType[ScPackaging] (TokenSets.PACKAGING_BIT_SET)

  def getPackageName = {
    val p = getPackageStatement
    if (p != null) p.getPackageName else ""
  }
  
  def getPackageStatement = findChildByClass(classOf[ScPackageStatement])

  override def getClasses = getTypeDefinitionsArray.map((t: ScTypeDefinition) => t.asInstanceOf[PsiClass])

  def getTopStatements: Array[ScTopStatement] = {
    val res = new ArrayBuffer[ScTopStatement]
    for (child <- getChildren() if child.isInstanceOf[ScTopStatement]) res+=child.asInstanceOf[ScTopStatement]
    return res.toArray
  }

  override def getTypeDefinitions(): Seq[ScTypeDefinition] = getChildren.flatMap (collectTypeDefs)

  override def collectTypeDefs (child: PsiElement) = child match {
    case p: ScPackaging => p.getTypeDefinitions
    case t: ScTypeDefinition => List (t) ++ t.getTypeDefinitions
    case _ => Seq.empty
  }

  def icon = Icons.FILE_TYPE_LOGO

  override def processDeclarations(processor: PsiScopeProcessor,
      state : ResolveState,
      lastParent: PsiElement,
      place: PsiElement): Boolean = {
    import org.jetbrains.plugins.scala.lang.resolve._

    if (lastParent != null) { //we are ascending from this file
      var run = lastParent.getPrevSibling
      while (run != null) {
        if (!run.processDeclarations(processor, state, lastParent, place)) return false
        run = run.getPrevSibling
      }
    }
    true
  }
}
