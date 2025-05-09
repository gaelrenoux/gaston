package fr.renoux.gaston.input

//import com.softwaremill.diffx
//import com.softwaremill.diffx.generic.auto.*
//import com.softwaremill.diffx.generic.auto.given

import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.model.{Score, Weight}
import io.github.iltotore.iron.autoRefine
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class TableReaderSpec extends AnyFlatSpec with Matchers {
  // TODO Replace diffx by difflicious

  val tableSettings: InputTableSettings = InputTableSettings(
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

  val settings: InputSettings = InputSettings(
    incompatibilityAntiPreference = NonPosScore(-1000.0),
    defaultMinPersonsPerTopic = 4,
    defaultMaxPersonsPerTopic = 6
  )

  val input: InputModel = InputModel(
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
    val ib = input.persons.find(_.name == "Boojum")
    val eb = expected.persons.find(_.name == "Boojum")
    //val smallDiff = DiffShow.diff(ib, eb) //TODO make it work with refined
    //if (!smallDiff.isIdentical) println(smallDiff.string)
    ib should be(eb)

    /* Check all */
    //    if (input != expected) {
    //      diffx.compare(expected.settings, input.settings)
    //      diffx.compare(expected.tableSettings, input.tableSettings)
    //      diffx.compare(expected.slots.map(_.toList).toList, input.slots.map(_.toList).toList) //diff can't compare Seq
    //      diffx.compare(expected.topics, input.topics)
    //      diffx.compare(expected.persons, input.persons)
    //      diffx.compare(expected.constraints, input.constraints)
    //    }
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
    val rendered = InputRenderer.render(input)
    val evaluated = InputLoader.fromString(rendered).force
    val expected = InputLoader.fromClassPath("udocon2017/uc17-from-table.conf").force

    /* Check all */
    //    if (evaluated != expected) {
    //      println(rendered)
    //      diffx.compare(evaluated.settings, expected.settings)
    //      diffx.compare(evaluated.tableSettings, expected.tableSettings)
    //      diffx.compare(evaluated.slots.map(_.toList), expected.slots.map(_.toList)) //diff can't compare Seq
    //      diffx.compare(evaluated.topics, expected.topics)
    //      diffx.compare(evaluated.persons, expected.persons)
    //      diffx.compare(evaluated.constraints, expected.constraints)
    //    }
    evaluated should be(expected)
  }

}
