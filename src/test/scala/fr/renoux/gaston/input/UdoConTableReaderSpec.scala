package fr.renoux.gaston.input

import ai.x.diff.DiffShow
import fr.renoux.gaston.model.{Score, Weight}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class UdoConTableReaderSpec extends FlatSpec with Matchers {

  val udoSettings = InputUdoSettings(
    personsStartingIndex = 4,
    topicsIndex = 0,
    maxPlayersIndex = 2,
    minPlayersIndex = None,
    gamemasterWeight = Weight(1.5)
  )

  val settings = InputSettings(
    weakPreference = Score(1),
    strongPreference = Score(5),
    incompatibilityAntiPreference = Score(-50),
    defaultMin = 4,
    defaultMax = 6
  )

  val reader = new UdoConTableReader(udoSettings, settings)

  behavior of "read"
  it should "read correctly" in {
    val table = Source.fromResource("udocon-table.csv").mkString
    val input = reader.read(table)

    val expected = PureConfigLoader.fromClassPath("udocon-table-formatted.conf").forceToInput

    println(DiffShow.diff[InputRoot](input, expected).string)
    input should be(expected)
  }

  it should "be rendered correctly" in {
    val table = Source.fromResource("udocon-table.csv").mkString
    val input = reader.read(table)
    val rendered = PureConfigLoader.render(input)

    val evaluated = PureConfigLoader.fromString(rendered).forceToInput
    val expected = PureConfigLoader.fromClassPath("udocon-table-formatted.conf").forceToInput

    println(DiffShow.diff[InputRoot](input, expected).string)
    evaluated should be(expected)
  }

}
