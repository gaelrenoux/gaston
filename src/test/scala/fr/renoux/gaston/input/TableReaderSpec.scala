package fr.renoux.gaston.input

import ai.x.diff.DiffShow
import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.model.{Score, Weight}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class TableReaderSpec extends FlatSpec with Matchers {

  val tableSettings = InputTableSettings(
    separator = "\t",
    personsRow = 0,
    wishesStartRow = 1,
    personsStartCol = 4,
    topicCol = 0,
    maxPersonsCol = 2,
    minPersonsCol = None,
    topicOccurrencesCol = None,
    personsCountAdd = 1,
    mandatoryPersonCol = 1,
    mandatoryPersonWeight = Weight(1.5),
    forbiddenPersonMarker = Some("0"),
    preferencesScoreMapping = Some(Map("1" -> Score(1), "2" -> Score(5)))
  )

  val settings = InputSettings(
    incompatibilityAntiPreference = Score(-1000),
    defaultMinPersonsPerTopic = 4,
    defaultMaxPersonsPerTopic = 6,
    personOnNothingAntiPreference = Score(-100)
  )

  val input = InputModel(
    settings = settings,
    tableSettings = tableSettings,
    slots = Seq(
      Seq(InputSlot("D1-afternoon"), InputSlot("D1-evening", maxTopics = Some(5))),
      Seq(InputSlot("D2-afternoon"), InputSlot("D2-evening", maxTopics = Some(5))),
      Seq(InputSlot("D3-afternoon"))
    ))

  val reader = new TableReader(input)

  behavior of "read"
  it should "read correctly" in {
    val table = Source.fromResource("udocon2017/uc17-table.csv").mkString
    val input = reader.read(table)

    val expected = InputLoader.fromClassPath("udocon2017/uc17-from-table.conf").force

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
      printDiff(expected.gaston.slots.map(_.toList).toList, input.gaston.slots.map(_.toList).toList) //diff can't compare Seq
      printDiff(expected.gaston.topics, input.gaston.topics)
      printDiff(expected.gaston.persons, input.gaston.persons)
      printDiff(expected.gaston.constraints, input.gaston.constraints)
    }
    input.gaston.settings should be(expected.gaston.settings)
    input.gaston.tableSettings should be(expected.gaston.tableSettings)
    input.gaston.slots should be(expected.gaston.slots)
    input.gaston.topics should be(expected.gaston.topics)
    input.gaston.persons should be(expected.gaston.persons)
    input.gaston.constraints should be(expected.gaston.constraints)
    input should be(expected)
  }

  it should "be rendered correctly" in {
    val table = Source.fromResource("udocon2017/uc17-table.csv").mkString
    val input = reader.read(table)
    val rendered = InputLoader.render(input)

    val evaluated = InputLoader.fromString(rendered).force
    val expected = InputLoader.fromClassPath("udocon2017/uc17-from-table.conf").force

    /* Check all */
    if (input != expected) {
      printDiff(expected.gaston.settings, input.gaston.settings)
      printDiff(expected.gaston.tableSettings, input.gaston.tableSettings)
      printDiff(expected.gaston.slots.map(_.toList).toList, input.gaston.slots.map(_.toList).toList) //diff can't compare Seq
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
