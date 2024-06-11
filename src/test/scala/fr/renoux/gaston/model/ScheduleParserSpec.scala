package fr.renoux.gaston.model

import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class ScheduleParserSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.SimpleTestModel.Persons._
  import fr.renoux.gaston.SimpleTestModel.Problems._
  import fr.renoux.gaston.SimpleTestModel.Slots._
  import fr.renoux.gaston.SimpleTestModel.Solutions._
  import fr.renoux.gaston.SimpleTestModel.Topics._

  private implicit val problem: Problem = Complete
  private implicit val context: Context = Context.Default

  val parser = new ScheduleParser()
  val formatted: String = Best.toFormattedString
  println(formatted)

  "parseTopic" should "read a correct topic" in {
    parser.parseRecord(Morning, "Bathing ==> Bianca, Arthur") should be(Right(Record(Morning, Bathing, Set(Bianca, Arthur))))
  }

  "readAllRecords" should "parse a list or records" in {
    val lines = List(
      "    Bathing ==> Bianca, Arthur",
      "    Eating ==> Eric, Daniela, Corwin",
      "    Helping ==> Hercule, Garion, Fiona",
      "  evening:"
    )
    parser.readAllRecords(Morning, 2, lines) should be(Right(
      Set(
        Record(Morning, Bathing, Set(Bianca, Arthur)),
        Record(Morning, Eating, Set(Eric, Daniela, Corwin)),
        Record(Morning, Helping, Set(Hercule, Garion, Fiona))
      ) -> lines.drop(3)
    ))
  }

  "readSlotSchedule" should "parse a slot schedule" in {
    val lines = Best.toFormattedString.linesIterator.toList.tail // drop 'Schedule:' line
    val result: Either[String, (Option[SlotSchedule], List[String])] = parser.readSlotSchedule(0, lines)
    result.left.toOption should be(None)
    val (slotSchedule, linesLeft) = result.toOption.get
    slotSchedule.get.toFormattedString should be(Best.on(AfterNoon).toFormattedString) // AfterNoon is the first slot alphabetically
    slotSchedule should be(Some(Best.on(AfterNoon)))
    linesLeft should be(lines.drop(4))
  }

  "parseFormattedString" should "parse a schedule" in {
    val result = parser.parseFormattedString(Best.toFormattedString)
    result.left.toOption should be(None)
    result.toOption.get.toFormattedString should be(Best.toFormattedString)
    result.toOption.get should be(Best)

    println("result.toOption.get.toFormattedString")
    result.toOption.get.toFormattedString should be(Best.toFormattedString)
  }
}
