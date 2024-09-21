package fr.renoux.gaston

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.input.{InputLoader, InputModel, transcribe}
import fr.renoux.gaston.tools.InputAnonymizer

import java.nio.file.Paths
import scala.io.Source

/** Temporary tool to anonymize. Should be done through the command-line interface for Gaston. */
object InputAnonymizerApp extends App {

  private def stringFromResource(name: String): String = {
    val target = Paths.get(getClass.getClassLoader.getResource(name).toURI)
    val src = Source.fromFile(target.toFile)
    try src.mkString finally src.close()
  }

  // val source: InputModel = InputLoader.fromClassPath("r32023/r32023-from-table.conf").force
  val source: InputModel = InputLoader.fromClassPath("r32023/r32023-full.conf").force
  val tableSource = stringFromResource("r32023/r32023-table.csv")

  val anonymizer = new InputAnonymizer(source)
  assert(transcribe(source).force.toAbstract == transcribe(anonymizer.anonymized).force.toAbstract)

  // println(InputLoader.render(source))
  // println(InputLoader.render(anonymizer.anonymized))
   println(InputLoader.render(anonymizer.anonymizedReordered))
  //println(anonymizer.anonymizeTable(tableSource))

}
