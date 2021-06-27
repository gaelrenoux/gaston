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

  implicit val problem: Problem = Complete

  private val bestMorning = Best.on(Morning)

  "topics" should "work" in {
    bestMorning.topics should be(Set(Acting, Dancing, Grinding))
  }

  "persons" should "work" in {
    bestMorning.scheduledPersons should be(Set(Arthur, Iago, Hercule, Daniela, Corwin, Bianca, Garion, Fiona))
  }

  "countPersonsByTopic" should "work" in {
    bestMorning.countPersonsByTopic should be(Map(
      Acting -> 3,
      Dancing -> 3,
      Grinding -> 2
    ))
  }

  "unweightedScoresByPerson" should "be correct for a simple case" in {
    val slots = SimpleTestModel.Solutions.Best.slotSchedulesList.sortBy(_.slot.id).map(_.unweightedScoresByPersonId)
    val slot1 = slots.head
    val slot2 = slots.tail.head
    val slot3 = slots.tail.tail.head

    slot1 should be(Array(
      Score(0.0), // Arthur
      Score(1.0), // Bianca
      Score(5.0), // Corwin
      Score(0.0), // Daniela
      Score(0.0), // Eric
      Score(5.0), // Fiona
      Score(0.0), // Garion
      Score(1.0), // Hercule
      Score(5.0), // Iago
    ))
    slot2 should be(Array(
      Score(5.0), // Arthur
      Score(0.0), // Bianca
      Score(1.0), // Corwin
      Score(5.0), // Daniela
      Score(0.0), // Eric
      Score(1.0), // Fiona
      Score(5.0), // Garion
      Score(0.0), // Hercule
      Score(0.0), // Iago
    ))
    slot3 should be(Array(
      Score(0.0), // Arthur
      Score(5.0), // Bianca
      Score(0.0), // Corwin
      Score(1.0), // Daniela
      Score(5.0), // Eric
      Score(0.0), // Fiona
      Score(1.0), // Garion
      Score(5.0), // Hercule
      Score(0.0), // Iago
    ))
  }

}
