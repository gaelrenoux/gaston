package fr.renoux.gaston.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SlotScheduleSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.SimpleTestModel.Persons._
  import fr.renoux.gaston.SimpleTestModel.Slots._
  import fr.renoux.gaston.SimpleTestModel.Solutions._
  import fr.renoux.gaston.SimpleTestModel.Topics._
  import fr.renoux.gaston.SimpleTestModel.Problems._

  implicit val problem: Problem = Complete

  private val BestMorning = Best.on(Morning)

  "topics" should "work" in {
    BestMorning.topics should be(Set(Acting, Dancing, Grinding))
  }

  "persons" should "work" in {
    BestMorning.scheduledPersons should be(Set(Arthur, Iago, Hercule, Daniela, Corwin, Bianca, Garion, Fiona))
  }

  "countPersonsByTopic" should "work" in {
    BestMorning.countPersonsByTopic should be(Map(
      Acting -> 3,
      Dancing -> 3,
      Grinding -> 2
    ))
  }
}
