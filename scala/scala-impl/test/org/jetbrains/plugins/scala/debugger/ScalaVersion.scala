package org.jetbrains.plugins.scala.debugger

import java.lang.management.ManagementFactory

import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.sun.management.UnixOperatingSystemMXBean
import org.jetbrains.plugins.scala.TestFixtureProvider
import org.jetbrains.plugins.scala.base.libraryLoaders.LibraryLoader

import scala.collection.mutable.ListBuffer

/**
 * @author Nikolay.Tropin
 */
sealed trait ScalaVersion {
  val major: String
  val minor: String
}

case object Scala_2_9 extends ScalaVersion {
  override final val major: String = "2.9"
  override final val minor: String = "2.9.3"
}

case object Scala_2_10 extends ScalaVersion {
  override final val major: String = "2.10"
  override final val minor: String = "2.10.7"
}

case object Scala_2_11 extends ScalaVersion {
  override final val major: String = "2.11"
  override final val minor: String = "2.11.12"
}

case object Scala_2_12 extends ScalaVersion {
  override final val major: String = "2.12"
  override final val minor: String = "2.12.3"
}

case object Scala_2_13 extends ScalaVersion {
  override final val major: String = "2.13"
  override final val minor: String = "2.13.0-M4-pre-20d3c21"
}

trait ScalaSdkOwner {
  implicit val version: ScalaVersion

  implicit protected def module: Module

  def project: Project = module.getProject

  protected def librariesLoaders: Seq[LibraryLoader]

  private lazy val myLoaders: ListBuffer[LibraryLoader] = ListBuffer()

  private def printCurrentOpenFileCount(suffix: String): Unit = {
    ManagementFactory.getOperatingSystemMXBean match {
      case x: UnixOperatingSystemMXBean =>
        val open = x.getOpenFileDescriptorCount
        val max = x.getMaxFileDescriptorCount
        println(s"[debug open files] $open of $max")
      case _ =>
    }
  }


  protected def setUpLibraries(): Unit = {
    printCurrentOpenFileCount("before setup libraries")
    librariesLoaders.foreach { loader =>
      myLoaders += loader
      loader.init
    }
  }

  protected def disposeLibraries(): Unit = {
    myLoaders.foreach(_.clean)
    myLoaders.clear()

    printCurrentOpenFileCount("after disposing libraries")
  }

}

// Java compatibility
trait DefaultScalaSdkOwner extends ScalaSdkOwner with TestFixtureProvider {
  override implicit val version: ScalaVersion = Scala_2_10
}
