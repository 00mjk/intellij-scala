package org.jetbrains.plugins.scala.lang.psi.types.api

import com.intellij.psi.PsiTypeParameter
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.lang.psi.types.result._
import org.jetbrains.plugins.scala.lang.psi.types.{NamedType, ScType, ScUndefinedSubstitutor}
import org.jetbrains.plugins.scala.project.ProjectContext

class TypeParameterType private (val typeParameter: TypeParameter)
  extends ValueType with NamedType {

  def typeParameters: Seq[TypeParameter] = typeParameter.typeParameters

  val arguments: Seq[TypeParameterType] = typeParameters.map(new TypeParameterType(_))

  lazy val lowerType: ScType = typeParameter.lowerType

  lazy val upperType: ScType = typeParameter.upperType

  def psiTypeParameter: PsiTypeParameter = typeParameter.psiTypeParameter

  override implicit def projectContext: ProjectContext = psiTypeParameter

  override val name: String = typeParameter.name

  def isInvariant: Boolean = typeParameter.isInvariant

  def isCovariant: Boolean = typeParameter.isCovariant

  def isContravariant: Boolean = typeParameter.isContravariant

  override def equivInner(`type`: ScType, substitutor: ScUndefinedSubstitutor, falseUndef: Boolean): (Boolean, ScUndefinedSubstitutor) =
    (`type` match {
      case that: TypeParameterType => (that.psiTypeParameter eq psiTypeParameter) || {
        (psiTypeParameter, that.psiTypeParameter) match {
          case (myBound: ScTypeParam, thatBound: ScTypeParam) =>
            //TODO this is a temporary hack, so ignore substitutor for now
            myBound.lowerBound.exists(_.equiv(thatBound.lowerBound.getOrNothing)) &&
              myBound.upperBound.exists(_.equiv(thatBound.upperBound.getOrNothing)) &&
              (myBound.name == thatBound.name || thatBound.isHigherKindedTypeParameter || myBound.isHigherKindedTypeParameter)
          case _ => false
        }
      }
      case _ => false
    }, substitutor)

  override def visitType(visitor: TypeVisitor): Unit = visitor.visitTypeParameterType(this)
}

object TypeParameterType {
  def apply(tp: TypeParameter): TypeParameterType =
    new TypeParameterType(tp)

  def apply(psiTp: PsiTypeParameter): TypeParameterType =
    new TypeParameterType(TypeParameter(psiTp))

  object ofPsi {
    def unapply(tpt: TypeParameterType): Option[PsiTypeParameter] = Some(tpt.psiTypeParameter)
  }
}
