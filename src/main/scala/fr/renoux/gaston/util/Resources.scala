package fr.renoux.gaston.util

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

object Resources {

  def readNonEmptyLines(path: String): Seq[String] = {
    val target = Paths.get(getClass.getClassLoader.getResource(path).toURI)
    val javaLines = Files.readAllLines(target, StandardCharsets.UTF_8)
    javaLines.asScala.view.map(_.trim).filter(_.nonEmpty).toSeq
  }

}
