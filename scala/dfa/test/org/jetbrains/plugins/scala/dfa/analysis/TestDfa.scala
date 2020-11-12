package org.jetbrains.plugins.scala.dfa
package analysis

object TestDfa {
  def apply[Info](graph: cfg.Graph[Info]): DataFlowAnalysis[Info] =
    new DataFlowAnalysis(graph, Map.empty)
}
