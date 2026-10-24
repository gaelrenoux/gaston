package fr.renoux.gaston

import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.input.{InputLoader, InputModel, InputRenderer, InputTranscription}
import fr.renoux.gaston.tools.InputAnonymizer

/** Temporary tool to anonymize. Should be done through the command-line interface for Gaston. */
object InputAnonymizer {

  @main
  def run(): Unit = {
    assert(InputTranscription.transcribe(source).force.toAbstract == InputTranscription.transcribe(anonymizer.anonymized).force.toAbstract)

    // println(InputLoader.render(source))
    // println(InputLoader.render(anonymizer.anonymized))
    println(InputRenderer.render(anonymizer.anonymizedReordered))
    //println(anonymizer.anonymizeTable(tableSource))
  }

  //  private def stringFromResource(name: String): String = {
  //    val target = Paths.get(getClass.getClassLoader.getResource(name).toURI)
  //    val src = Source.fromFile(target.toFile)
  //    try src.mkString finally src.close()
  //  }
  // lazy val tableSource = stringFromResource("r32023/r32023-table.csv")

  // val source: InputModel = InputLoader.fromClassPath("r32023/r32023-from-table.conf").force
  lazy val source: InputModel = InputLoader.fromClassPath("r32023/r32023-tts2.conf").force

  lazy val anonymizer = new InputAnonymizer(source)
}
