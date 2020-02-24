package org.jetbrains.plugins.scala
package codeInspection.collections

import org.jetbrains.plugins.scala.codeInspection.InspectionBundle
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

/**
 * Nikolay.Tropin
 * 2014-05-07
 */
class GetGetOrElseInspection extends OperationOnCollectionInspection {
  override def possibleSimplificationTypes: Array[SimplificationType] =
    Array(GetGetOrElse)
}

object GetGetOrElse extends SimplificationType() {

  override def hint: String = InspectionBundle.message("get.getOrElse.hint")

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    expr match {
      case map`.getOnMap`(key)`.getOrElse`(default) =>
        Some(replace(expr).withText(invocationText(map, "getOrElse", key, default)).highlightFrom(map))
      case _ => None
    }
  }

}
