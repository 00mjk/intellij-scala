package org.jetbrains.plugins.scala
package lang
package dfa
package specials

import org.jetbrains.plugins.scala.dfa.analysis.{NodeInstance, SpecialMethodProcessor, State}
import org.jetbrains.plugins.scala.dfa.cfg

object Defaults {
  def equalsImpl(node: cfg.Call): SpecialMethodProcessor =
    (state: State, controller: NodeInstance.Controller) => {
      ???
    }
}
