package org.jetbrains.plugins.scala
package compiler

import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.Duration

/**
 * Nikolay.Tropin
 * 2014-10-07
 */
trait CompilationProcess {
  def run(): Unit

  final def runSync(): Unit = {
    val result = Promise[Unit]
    addTerminationCallback {
      case Some(error) => throw error
      case None => result.success(())
    }
    run()
    // TODO: we should avoid potential infinite blocking, and handle "long-waiting" case appropriately
    Await.result(result.future, Duration.Inf)
  }

  def stop(): Unit

  def addTerminationCallback(callback: Option[Throwable] => Unit): Unit
}