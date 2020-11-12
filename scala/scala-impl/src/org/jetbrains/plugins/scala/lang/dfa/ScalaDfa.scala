package org.jetbrains.plugins.scala.lang.dfa

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.dfa.analysis.DataFlowAnalysis.SpecialMethodProcessorFactories
import org.jetbrains.plugins.scala.dfa.analysis.{DataFlowAnalysis, DfaResult}
import org.jetbrains.plugins.scala.dfa.cfg.CallInfo
import org.jetbrains.plugins.scala.lang.psi.cfg.PsiGraph

object ScalaDfa {
  val specialMethodProcessorFactories: SpecialMethodProcessorFactories = Map(
    CallInfo("equals", ???) -> specials.Defaults.equalsImpl
  )

  def apply(graph: PsiGraph): DataFlowAnalysis[PsiElement] =
    new DataFlowAnalysis(graph, specialMethodProcessorFactories)

  def computeResult(graph: PsiGraph): DfaResult[PsiElement] = {
    val dfa = ScalaDfa(graph)
    dfa.run()
    dfa.result
  }
}
