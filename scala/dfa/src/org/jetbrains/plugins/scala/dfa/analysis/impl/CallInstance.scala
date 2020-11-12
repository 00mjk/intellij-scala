package org.jetbrains.plugins.scala.dfa
package analysis
package impl

import NodeInstance.Controller

private final class CallInstance(override val node: cfg.Call) extends ValueInstance with LinearNodeInstance {
  private var specialMethodProcessor = Option.empty[Option[SpecialMethodProcessor]]

  override protected def linearProcess(state: State, controller: Controller): Unit = {
    val specialProcessor = specialMethodProcessor.getOrElse {
      val maybeProcessor = controller.specialMethodProcessor(node.callInfo, node)
      specialMethodProcessor = Some(maybeProcessor)
      maybeProcessor
    }

    specialProcessor match {
      case Some(processor) =>
        processor.process(state, controller)

      case None =>
        state.current = DfAny.Top
    }
  }

  override def reset(): Unit = {
    specialMethodProcessor = None
  }
}
