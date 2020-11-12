package org.jetbrains.plugins.scala.dfa
package analysis

import org.jetbrains.plugins.scala.dfa.analysis.NodeInstance.Controller


abstract class SpecialMethodProcessor {
  def process(state: State, controller: Controller): DfAny
  def reset(): Unit = ()
}
