package org.jetbrains.plugins.scala.lang.psi.api.base

import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.result.Success

/**
  * @author adkozlov
  */
package object types {

  implicit class ScTypeElementExt(val typeElement: ScTypeElement) extends AnyVal {
    def calcType = typeElement.getType().getOrAny

    def success(`type`: ScType) = Success(`type`, Some(typeElement))
  }

}
