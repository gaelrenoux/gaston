package fr.renoux.gaston.model

import fr.renoux.gaston.SimpleTestModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SlotScheduleSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.SimpleTestModel.Persons._
  import fr.renoux.gaston.SimpleTestModel.Problems._
  import fr.renoux.gaston.SimpleTestModel.Slots._
  import fr.renoux.gaston.SimpleTestModel.Solutions._
  import fr.renoux.gaston.SimpleTestModel.Topics._

  implicit val problem: Problem = WithUnassignedTopics

  private val bestMorning = BestWithUnassignedTopics.on(Morning)

  "topics" should "work" in {
    bestMorning.topics should be(Set(Acting, Dancing, Grinding, UnassignedMorning))
  }

  "persons" should "work" in {
    bestMorning.scheduledPersons should be(Set(Arthur, Iago, Hercule, Daniela, Corwin, Bianca, Garion, Fiona))
  }

  "countPersonsByTopic" should "work" in {
    bestMorning.countPersonsByTopic should be(Map(
      Acting -> 3,
      Dancing -> 3,
      Grinding -> 2,
      UnassignedMorning -> 0
    ))
  }

  "unweightedScoresByPerson" should "be correct for a simple case" in {
    val slots = SimpleTestModel.Solutions.BestWithUnassignedTopics.slotSchedulesList.sortBy(_.slot.id).map(_.unweightedScoresByPerson)
    val slot1 = slots.head
    val slot2 = slots.tail.head
    val slot3 = slots.tail.tail.head

    slot1 should be(Map(
      SimpleTestModel.Persons.Arthur -> Score(0.0),
      SimpleTestModel.Persons.Bianca -> Score(1.0),
      SimpleTestModel.Persons.Corwin -> Score(5.0),
      SimpleTestModel.Persons.Daniela -> Score(0.0),
      SimpleTestModel.Persons.Fiona -> Score(5.0),
      // SimpleTestModel.Persons.Eric -> Score(0.0), // missing from this slot
      SimpleTestModel.Persons.Garion -> Score(0.0),
      SimpleTestModel.Persons.Hercule -> Score(1.0),
      SimpleTestModel.Persons.Iago -> Score(5.0),
    ))
    slot2 should be(Map(
      SimpleTestModel.Persons.Arthur -> Score(5.0),
      SimpleTestModel.Persons.Bianca -> Score(0.0),
      SimpleTestModel.Persons.Corwin -> Score(1.0),
      SimpleTestModel.Persons.Daniela -> Score(5.0),
      SimpleTestModel.Persons.Eric -> Score(0.0),
      SimpleTestModel.Persons.Fiona -> Score(1.0),
      SimpleTestModel.Persons.Garion -> Score(5.0),
      SimpleTestModel.Persons.Hercule -> Score(0.0),
      // SimpleTestModel.Persons.Iago -> Score(0.0), // missing from this slot
    ))
    slot3 should be(Map(
      // SimpleTestModel.Persons.Arthur -> Score(0.0), // missing from this slot
      SimpleTestModel.Persons.Bianca -> Score(5.0),
      SimpleTestModel.Persons.Corwin -> Score(0.0),
      SimpleTestModel.Persons.Daniela -> Score(1.0),
      SimpleTestModel.Persons.Eric -> Score(5.0),
      SimpleTestModel.Persons.Fiona -> Score(0.0),
      SimpleTestModel.Persons.Garion -> Score(1.0),
      SimpleTestModel.Persons.Hercule -> Score(5.0),
      SimpleTestModel.Persons.Iago -> Score(0.0),
    ))
  }

}
