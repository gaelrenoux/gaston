package fr.renoux.gaston.input

import ai.x.diff.DiffShow
import eu.timepit.refined.auto._
import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.input.InputRefinements.{NonPosScore, PosWeight}
import fr.renoux.gaston.model.Score
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
    mandatoryPersonWeight = PosWeight(1.5),
    forbiddenPersonMarker = Some("0"),
    preferencesScoreMapping = Some(Map("1" -> Score(1), "2" -> Score(5)))
  )

  val settings = InputSettings(
    incompatibilityAntiPreference = NonPosScore(-1000.0),
    defaultMinPersonsPerTopic = 4,
    defaultMaxPersonsPerTopic = 6,
    personOnNothingAntiPreference = NonPosScore(-100.0)
  )

  val input = InputModel(
    settings = settings,
    tableSettings = tableSettings,
    slots = List(
      List(InputSlot("D1-afternoon"), InputSlot("D1-evening", maxTopics = Some(5))),
      List(InputSlot("D2-afternoon"), InputSlot("D2-evening", maxTopics = Some(5))),
      List(InputSlot("D3-afternoon"))
    ))

  val reader = new TableReader(input)

  behavior of "read"
  it should "read correctly" in {
    val table = Source.fromResource("udocon2017/uc17-table.csv").mkString
    val input = reader.read(table)

    val expected = InputLoader.fromClassPath("udocon2017/uc17-from-table.conf").force

    /* Check a small one first, easier to debug */
    val ib = input.persons.find(_.name.value == "Boojum")
    val eb = expected.persons.find(_.name.value == "Boojum")
    //val smallDiff = DiffShow.diff(ib, eb) //TODO make it work with refined
    //if (!smallDiff.isIdentical) println(smallDiff.string)
    ib should be(eb)

    /* Check all */
    if (input != expected) {
      printDiff(expected.settings, input.settings)
      printDiff(expected.tableSettings, input.tableSettings)
      printDiff(expected.slots.map(_.toList).toList, input.slots.map(_.toList).toList) //diff can't compare Seq
      printDiff(expected.topics, input.topics)
      printDiff(expected.persons, input.persons)
      printDiff(expected.constraints, input.constraints)
    }
    input.settings should be(expected.settings)
    input.tableSettings should be(expected.tableSettings)
    input.slots should be(expected.slots)
    input.topics should be(expected.topics)
    input.persons should be(expected.persons)
    input.constraints should be(expected.constraints)
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
      printDiff(expected.settings, input.settings)
      printDiff(expected.tableSettings, input.tableSettings)
      printDiff(expected.slots.map(_.toList).toList, input.slots.map(_.toList).toList) //diff can't compare Seq
      printDiff(expected.topics, input.topics)
      printDiff(expected.persons, input.persons)
      printDiff(expected.constraints, input.constraints)
    }
    evaluated should be(expected)
  }

  private def printDiff[A: DiffShow](a: A, b: A) = {
    val diff = DiffShow.diff(a, b)
    if (!diff.isIdentical) println(diff.string)
  }

}
