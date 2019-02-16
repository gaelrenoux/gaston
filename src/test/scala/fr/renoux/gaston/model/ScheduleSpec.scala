package fr.renoux.gaston.model

import org.scalatest.{FlatSpec, Matchers}

class ScheduleSpec extends FlatSpec with Matchers {

  import fr.renoux.gaston.SimpleTestModel.Persons._
  import fr.renoux.gaston.SimpleTestModel.Problems._
  import fr.renoux.gaston.SimpleTestModel.Slots._
  import fr.renoux.gaston.SimpleTestModel.Solutions._
  import fr.renoux.gaston.SimpleTestModel.Topics._

  implicit val problem: Problem = Complete

  private val Simple = Schedule(
    Morning(
      Acting(Arthur, Bianca),
      Cooking(Corwin, Daniela)
    ),
    AfterNoon(
      Bathing(Arthur, Bianca),
      Dancing(Corwin, Daniela)
    )
  )

  "topicsPerSlot" should "work" in {
    Best.topicsPerSlot should be(Map(
      Morning -> Set(Acting, Dancing, Grinding),
      AfterNoon -> Set(Bathing, Eating, Helping),
      Evening -> Set(Cooking, Fighting, Inking)
    ))
  }

  "topicToSlot" should "work" in {
    Best.topicToSlot should be(Map(
      Acting -> Morning,
      Dancing -> Morning,
      Grinding -> Morning,
      Bathing -> AfterNoon,
      Eating -> AfterNoon,
      Helping -> AfterNoon,
      Cooking -> Evening,
      Fighting -> Evening,
      Inking -> Evening
    ))
  }

  "personsPerSlot" should "work" in {
    Best.personsPerSlot should be(Map(
      Morning -> Set(Arthur, Iago, Hercule, Daniela, Corwin, Bianca, Garion, Fiona),
      AfterNoon -> Set(Bianca, Arthur, Eric, Daniela, Corwin, Hercule, Garion, Fiona),
      Evening -> Set(Corwin, Bianca, Fiona, Eric, Daniela, Iago, Hercule, Garion)
    ))
  }

  "personsPerTopic" should "work" in {
    Best.personsPerTopic should be(Map(
      Acting -> Set(Arthur, Iago, Hercule),
      Dancing -> Set(Daniela, Corwin, Bianca),
      Grinding -> Set(Garion, Fiona),
      Bathing -> Set(Bianca, Arthur),
      Eating -> Set(Eric, Daniela, Corwin),
      Helping -> Set(Hercule, Garion, Fiona),
      Cooking -> Set(Corwin, Bianca),
      Fighting -> Set(Fiona, Eric, Daniela),
      Inking -> Set(Iago, Hercule, Garion)
    ))
  }

  "countPersonsPerTopic" should "work" in {
    Best.countPersonsPerTopic should be(Map(
      Acting -> 3,
      Dancing -> 3,
      Grinding -> 2,
      Bathing -> 2,
      Eating -> 3,
      Helping -> 3,
      Cooking -> 2,
      Fighting -> 3,
      Inking -> 3
    ))
  }

  "personGroups" should "work" in {
    Best.personGroups.toSet should be(Set(
      Set(Arthur, Iago, Hercule),
      Set(Daniela, Corwin, Bianca),
      Set(Garion, Fiona),
      Set(Bianca, Arthur),
      Set(Eric, Daniela, Corwin),
      Set(Hercule, Garion, Fiona),
      Set(Corwin, Bianca),
      Set(Fiona, Eric, Daniela),
      Set(Iago, Hercule, Garion)
    ))
  }

  "merge" should "work with two schedules" in {
    val simple2 = Schedule(
      AfterNoon(
        Bathing(Arthur, Eric), //new person on existing topic
        Eating(Fiona, Garion) //new topic on existing slot
      ),
      Evening( //new slot
        Fighting(Arthur, Bianca)
      )
    )

    val result = Schedule(
      Morning(
        Acting(Arthur, Bianca),
        Cooking(Corwin, Daniela)
      ),
      AfterNoon(
        Bathing(Arthur, Bianca, Eric), //new person on existing topic
        Dancing(Corwin, Daniela),
        Eating(Fiona, Garion) //new topic on existing slot
      ),
      Evening( //new slot
        Fighting(Arthur, Bianca)
      ),
    )

    Simple.merge(simple2) should be(result)
    simple2.merge(Simple) should be(result)
  }

  it should "work with an empty schedule" in {
    Schedule.empty.merge(Best) should be(Best)
    Best.merge(Schedule.empty) should be(Best)
  }

  "addPersonToExistingTopic" should "add someone to an existing topic" in {
    Simple.addPersonToExistingTopic(Acting, Eric) should be(Schedule(
      Morning(
        Acting(Arthur, Bianca, Eric),
        Cooking(Corwin, Daniela)
      ),
      AfterNoon (
        Bathing(Arthur, Bianca),
        Dancing(Corwin, Daniela)
      )
    ))
  }

  it should "not do anything with a non-existing topic" in {
    Simple.addPersonToExistingTopic(Grinding, Eric) should be(Simple)
  }

  "isSound" should "validate a correct schedule" in {
    Simple.isSound should be(true)
    Best.isSound should be(true)
  }

  it should "reject a schedule demanding ubiquity" in {
    Schedule(
      Morning(
        Acting(Arthur, Bianca),
        Cooking(Arthur, Daniela)
      )
    ).isSound should be(false)
  }

}
