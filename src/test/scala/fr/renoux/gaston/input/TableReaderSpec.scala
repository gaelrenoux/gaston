package fr.renoux.gaston.input

import ai.x.diff.DiffShow
import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.model.{Score, Weight}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class TableReaderSpec extends FlatSpec with Matchers {

  //private val log = Logger[TableReaderSpec]

  val udoSettings = InputTableSettings(
    separator = "\t",
    personsRow = 0,
    otherHeaderRowsCount = 0,
    personsStartingIndex = 4,
    topicIndex = 0,
    maxPersonsIndex = 2,
    minPersonsIndex = None,
    personsCountAdd = 1,
    mandatoryPersonIndex = 1,
    mandatoryPersonRewardWeight = Weight(1.5),
    forbiddenPersonMarker = Some("0"),
    preferencesScoreMapping = Some(Map("1" -> Score(1), "2" -> Score(5), "+++" -> Score( 5.0 ), "++" -> Score( 2.0 ), "+" -> Score( 1.0 )))
  )

  val settings = InputSettings(
    incompatibilityAntiPreference = Score(-1000),
    defaultMinPersonsPerTopic = 4,
    defaultMaxPersonsPerTopic = 6
  )

  val reader = new TableReader(udoSettings, settings)

  behavior of "read"
  it should "read correctly" in {
    val table = Source.fromResource("udocon-2017-table.csv").mkString
    val input = reader.read(table)

    val expected = InputLoader.fromClassPath("udocon-2017-from-table.conf").force

    /* Check a small one first, easier to debug */
    val ib = input.gaston.persons.find(_.name == "Boojum")
    val eb = expected.gaston.persons.find(_.name == "Boojum")
    val smallDiff = DiffShow.diff(ib, eb)
    if (!smallDiff.isIdentical) println(smallDiff.string)
    ib should be(eb)

    /* Check all */
    if (input != expected) {
      printDiff(expected.gaston.settings, input.gaston.settings)
      printDiff(expected.gaston.tableSettings, input.gaston.tableSettings)
      printDiff(expected.gaston.slots, input.gaston.slots)
      printDiff(expected.gaston.topics, input.gaston.topics)
      printDiff(expected.gaston.persons, input.gaston.persons)
      printDiff(expected.gaston.constraints, input.gaston.constraints)
    }
    input should be(expected)
  }

  it should "be rendered correctly" in {
    val table = Source.fromResource("udocon-2017-table.csv").mkString
    val input = reader.read(table)
    val rendered = InputLoader.render(input)

    val evaluated = InputLoader.fromString(rendered).force
    val expected = InputLoader.fromClassPath("udocon-2017-from-table.conf").force

    /* Check all */
    if (input != expected) {
      printDiff(expected.gaston.settings, input.gaston.settings)
      printDiff(expected.gaston.tableSettings, input.gaston.tableSettings)
      printDiff(expected.gaston.slots, input.gaston.slots)
      printDiff(expected.gaston.topics, input.gaston.topics)
      printDiff(expected.gaston.persons, input.gaston.persons)
      printDiff(expected.gaston.constraints, input.gaston.constraints)
    }
    evaluated should be(expected)
  }

  private def printDiff[A: DiffShow](a: A, b: A) = {
    val diff = DiffShow.diff(a, b)
    if (!diff.isIdentical) println(diff.string)
  }

}
