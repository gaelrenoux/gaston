package fr.renoux.gaston.io

import java.io.File

import fr.renoux.gaston.io.UdoConTableReader.Parameters
import fr.renoux.gaston.model.Score
import org.scalatest.{FlatSpec, Matchers}
import pureconfig.loadConfigFromFiles

import scala.io.Source

class UdoConTableReaderSpec extends FlatSpec with Matchers {

  val reader = new UdoConTableReader(Parameters(
    personsStartingIndex = 4,
    topicsIndex = 0,
    maxPlayersIndex = 2,
    minPlayersIndex = None,
    settings = InputSettings(
      weakPreference = Score(1),
      strongPreference = Score(5),
      incompatibilityAntiPreference = Score(-50),
      defaultMin = 3,
      defaultMax = 5
    )
  ))

  behavior of "read"
  it should "read correctly" in {
    val table = Source.fromResource("udocon-table.csv").mkString
    val input = reader.read(table)

    val file = new File(getClass.getResource("/udocon-application.conf").getPath)
    val expected = loadConfigFromFiles[InputRoot](Seq(file.toPath)).right.get

    input should be(expected)
  }

  it should "be rendered correctly" in {
    val table = Source.fromResource("udocon-table.csv").mkString
    val input = reader.read(table)
    val rendered = InputLoader.render(input)

    val evaluated = InputLoader.fromString(rendered).forceToInput
    val expected = InputLoader.fromClassPath("udocon-application.conf").forceToInput

    evaluated should be(expected)
  }

}
