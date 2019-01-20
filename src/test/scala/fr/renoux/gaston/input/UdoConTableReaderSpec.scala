package fr.renoux.gaston.input

import ai.x.diff.DiffShow
import fr.renoux.gaston.model.{Score, Weight}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class UdoConTableReaderSpec extends FlatSpec with Matchers {

  val udoSettings = InputUdoSettings(
    weakWishValue = Score(1),
    strongWishValue = Score(5),
    personsStartingIndex = 4,
    topicsIndex = 0,
    maxPlayersIndex = 2,
    minPlayersIndex = None,
    gamemasterWeight = Weight(1.5)
  )

  val settings = InputSettings(
    incompatibilityAntiPreference = Score(-50),
    defaultMin = 4,
    defaultMax = 6
  )

  val reader = new UdoConTableReader(udoSettings, settings)

  behavior of "read"
  it should "read correctly" in {
    val table = Source.fromResource("udocon-2017-table.csv").mkString
    val input = reader.read(table)

    val expected = InputLoader.fromClassPath("udocon-2017-from-table.conf").forceToInput

    /* Check a small one first, easier to debug */
    val ib = input.gaston.persons.find(_.name == "Boojum")
    val eb = expected.gaston.persons.find(_.name == "Boojum")
    val smallDiff = DiffShow.diff(ib, eb)
    if (!smallDiff.isIdentical) println(smallDiff.string)
    ib should be(eb)

    /* Check all */
    val diff = DiffShow.diff(input, expected)
    if (!diff.isIdentical) println(diff.string)
    input should be(expected)
  }

  it should "be rendered correctly" in {
    val table = Source.fromResource("udocon-2017-table.csv").mkString
    val input = reader.read(table)
    val rendered = InputLoader.render(input)

    val evaluated = InputLoader.fromString(rendered).forceToInput
    val expected = InputLoader.fromClassPath("udocon-2017-from-table.conf").forceToInput

    val diff = DiffShow.diff[InputRoot](input, expected)
    if (!diff.isIdentical) println(diff.string)
    evaluated should be(expected)
  }

}
