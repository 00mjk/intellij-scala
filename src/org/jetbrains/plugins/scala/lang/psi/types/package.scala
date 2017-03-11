package org.jetbrains.plugins.scala.lang.psi

import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiClass, PsiNamedElement, PsiType, PsiTypeParameter}
import org.jetbrains.plugins.scala.decompiler.DecompilerUtil
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement.ElementScope
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScTypeAlias, ScTypeAliasDefinition}
import org.jetbrains.plugins.scala.lang.psi.types.api.ScTypePresentation.shouldExpand
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.{DesignatorOwner, ScProjectionType, ScThisType}
import org.jetbrains.plugins.scala.lang.psi.types.api.{TypeParameterType, _}
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.NonValueType
import org.jetbrains.plugins.scala.lang.refactoring.util.ScTypeUtil.AliasType

import scala.annotation.tailrec
/**
  * @author adkozlov
  */
package object types {

  implicit class ScTypeExt(val scType: ScType) extends AnyVal {
    def equiv(`type`: ScType)(implicit typeSystem: TypeSystem): Boolean = {
      typeSystem.equivalence.equiv(scType, `type`)
    }

    def equiv(`type`: ScType, undefinedSubstitutor: ScUndefinedSubstitutor, falseUndef: Boolean = true)
             (implicit typeSystem: TypeSystem): (Boolean, ScUndefinedSubstitutor) = {
      typeSystem.equivalence.equivInner(scType, `type`, undefinedSubstitutor, falseUndef)
    }

    def conforms(`type`: ScType)
                (implicit typeSystem: TypeSystem): Boolean = {
      typeSystem.conformance.conformsInner(`type`, scType)._1
    }

    def weakConforms(`type`: ScType)
                    (implicit typeSystem: TypeSystem): Boolean = {
      typeSystem.conformance.conformsInner(`type`, scType, checkWeak = true)._1
    }

    def conforms(`type`: ScType,
                 undefinedSubstitutor: ScUndefinedSubstitutor,
                 checkWeak: Boolean = false)
                (implicit typeSystem: TypeSystem): (Boolean, ScUndefinedSubstitutor) = {
      typeSystem.conformance.conformsInner(`type`, scType, substitutor = undefinedSubstitutor, checkWeak = checkWeak)
    }

    def glb(`type`: ScType, checkWeak: Boolean = false)(implicit typeSystem: TypeSystem): ScType = {
      typeSystem.bounds.glb(scType, `type`, checkWeak)
    }

    def lub(`type`: ScType, checkWeak: Boolean = true)(implicit typeSystem: TypeSystem): ScType = {
      typeSystem.bounds.lub(scType, `type`, checkWeak)
    }

    def removeUndefines(): ScType = scType.recursiveUpdate {
      case _: UndefinedType => (true, Any)
      case tp: ScType => (false, tp)
    }

    def toPsiType(noPrimitives: Boolean = false)
                 (implicit elementScope: ElementScope): PsiType = {
      elementScope.typeSystem.bridge.toPsiType(scType, noPrimitives = noPrimitives)
    }

    /**
      * Returns named element associated with type.
      * If withoutAliases is true expands alias definitions first
      *
      * @param withoutAliases need to expand alias or not
      * @return element and substitutor
      */
    def extractDesignated(implicit withoutAliases: Boolean): Option[(PsiNamedElement, ScSubstitutor)] = {
      new DesignatorExtractor(withoutAliases).extractFrom(scType)
    }

    def extractClass(project: Project = null): Option[PsiClass] = {
      extractClassType(project).map(_._1)
    }

    def extractClassType(project: Project = null,
                         visitedAlias: Set[ScTypeAlias] = Set.empty): Option[(PsiClass, ScSubstitutor)] = {
      new ClassTypeExtractor(project).extractFrom(scType)
    }

    @tailrec
    final def removeAliasDefinitions(visited: Set[ScType] = Set.empty, expandableOnly: Boolean = false): ScType = {
      if (visited.contains(scType)) {
        return scType
      }

      var updated = false
      val result = scType.recursiveUpdate {
        `type` => `type`.isAliasType match {
          case Some(AliasType(ta: ScTypeAliasDefinition, _, upper)) if !expandableOnly || shouldExpand(ta) =>
            updated = true
            (true, upper.getOrAny)
          case _ => (false, `type`)
        }
      }
      if (updated) result.removeAliasDefinitions(visited + scType, expandableOnly) else scType
    }

    def extractDesignatorSingleton: Option[ScType] = scType match {
      case desinatorOwner: DesignatorOwner => desinatorOwner.designatorSingletonType
      case _ => None
    }

    def tryExtractDesignatorSingleton: ScType = extractDesignatorSingleton.getOrElse(scType)
  }

  implicit class ScTypesExt(val types: Seq[ScType]) extends AnyVal {
    def glb(checkWeak: Boolean = false)(implicit typeSystem: TypeSystem): ScType = {
      typeSystem.bounds.glb(types, checkWeak)
    }

    def lub(checkWeak: Boolean = true)(implicit typeSystem: TypeSystem): ScType = {
      typeSystem.bounds.glb(types, checkWeak)
    }
  }

  private trait Extractor[T <: PsiNamedElement] {
    def isAppropriate(named: PsiNamedElement, subst: ScSubstitutor): Option[(T, ScSubstitutor)]
    def expandAliases: Boolean
    def project: Project

    def extractFrom(scType: ScType,
                    visitedAliases: Set[ScTypeAliasDefinition] = Set.empty): Option[(T, ScSubstitutor)] = {

      def expandAlias(definition: ScTypeAliasDefinition) = expandAliases && !visitedAliases(definition)

      scType match {
        case nonValueType: NonValueType =>
          extractFrom(nonValueType.inferValueType, visitedAliases)
        case thisType: ScThisType => isAppropriate(thisType.element, ScSubstitutor(thisType))
        case projType: ScProjectionType =>
          val actualSubst = projType.actualSubst
          val actualElement = projType.actualElement
          actualElement match {
            case definition: ScTypeAliasDefinition if expandAlias(definition) =>
              definition.aliasedType.toOption.flatMap { tp =>
                extractFrom(actualSubst.subst(tp), visitedAliases + definition)
              }
            case _ => isAppropriate(actualElement, actualSubst)
          }
        case designatorOwner: DesignatorOwner =>
          designatorOwner.element match {
            case definition: ScTypeAliasDefinition if expandAlias(definition) =>
              definition.aliasedType.toOption.flatMap {
                extractFrom(_, visitedAliases + definition)
              }
            case elem => isAppropriate(elem, ScSubstitutor.empty)
          }
        case parameterizedType: ParameterizedType =>
          extractFrom(parameterizedType.designator, visitedAliases).map {
            case (element, substitutor) => (element, substitutor.followed(parameterizedType.substitutor))
          }
        case stdType: StdType =>
          stdType.asClass(project).flatMap {
            isAppropriate(_, ScSubstitutor.empty)
          }
        case ScExistentialType(quantified, _) =>
          extractFrom(quantified, visitedAliases)
        case TypeParameterType(_, _, _, psiTypeParameter) =>
          isAppropriate(psiTypeParameter, ScSubstitutor.empty)
        case _ => None
      }
    }
  }

  private class DesignatorExtractor(override val expandAliases: Boolean) extends Extractor[PsiNamedElement] {
    override def isAppropriate(named: PsiNamedElement, subst: ScSubstitutor): Option[(PsiNamedElement, ScSubstitutor)] =
      Some(named, subst)

    override def project: Project = DecompilerUtil.obtainProject
  }

  private class ClassTypeExtractor(givenProject: Project) extends Extractor[PsiClass] {
    override def isAppropriate(named: PsiNamedElement, subst: ScSubstitutor): Option[(PsiClass, ScSubstitutor)] =
      named match {
        case _: PsiTypeParameter => None
        case c: PsiClass => Some(c, subst)
        case _ => None
      }

    override def project: Project = Option(givenProject).getOrElse(DecompilerUtil.obtainProject)

    override val expandAliases: Boolean = true
  }

}
