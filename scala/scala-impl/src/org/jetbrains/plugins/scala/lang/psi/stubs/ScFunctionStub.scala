package org.jetbrains.plugins.scala
package lang
package psi
package stubs

import com.intellij.psi.stubs.NamedStub
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.stubs.impl.ScTypeElementOwnerStub

/**
  * User: Alexander Podkhalyuzin
  * Date: 14.10.2008
  */
trait ScFunctionStub[F <: ScFunction] extends NamedStub[F]
  with ScMemberOrLocal
  with ScTypeElementOwnerStub[F] {

  def isImplicit: Boolean

  def isDeclaration: Boolean

  def annotations: Array[String]
}