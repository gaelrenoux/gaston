package fr.renoux.gaston.model

import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class ScheduleSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.SimpleTestModel.Persons.*
  import fr.renoux.gaston.SimpleTestModel.Problems.*
  import fr.renoux.gaston.SimpleTestModel.Slots.*
  import fr.renoux.gaston.SimpleTestModel.Solutions.*
  import fr.renoux.gaston.SimpleTestModel.Topics.*
  import fr.renoux.gaston.SimpleTestModel.strongPreference

  private implicit val problem: Problem = WithUnassignedTopics
  private implicit val context: Context = Context.Default

  private val Simple = Schedule.from(
    Morning(
      Acting(Arthur, Bianca),
      Cooking(Corwin, Daniela)
    ),
    AfterNoon(
      Bathing(Arthur, Bianca),
      Dancing(Corwin, Daniela)
    )
  )

  "planning" should "work" in {
    BestWithUnassignedTopics.planning should be(Map(
      Morning -> Set(Acting, Dancing, Grinding, UnassignedMorning),
      AfterNoon -> Set(Bathing, Eating, Helping, UnassignedAfternoon),
      Evening -> Set(Cooking, Fighting, Inking, UnassignedEvening)
    ))
  }

  "topicToSlot" should "work" in {
    BestWithUnassignedTopics.topicToSlot should be(Map(
      Acting -> Morning,
      Dancing -> Morning,
      Grinding -> Morning,
      Bathing -> AfterNoon,
      Eating -> AfterNoon,
      Helping -> AfterNoon,
      Cooking -> Evening,
      Fighting -> Evening,
      Inking -> Evening,
      UnassignedMorning -> Morning,
      UnassignedAfternoon -> AfterNoon,
      UnassignedEvening -> Evening
    ))
  }

  /*
  "personsBySlot" should "work" in {
    Best.personsBySlot should be(Map(
      Morning -> Set(Arthur, Iago, Hercule, Daniela, Corwin, Bianca, Garion, Fiona),
      AfterNoon -> Set(Bianca, Arthur, Eric, Daniela, Corwin, Hercule, Garion, Fiona),
      Evening -> Set(Corwin, Bianca, Fiona, Eric, Daniela, Iago, Hercule, Garion)
    ))
  } */

  "personsByTopic" should "work" in {
    BestWithUnassignedTopics.personsByTopic should be(Map(
      Acting -> Set(Arthur, Iago, Hercule),
      Dancing -> Set(Daniela, Corwin, Bianca),
      Grinding -> Set(Garion, Fiona),
      Bathing -> Set(Bianca, Arthur),
      Eating -> Set(Eric, Daniela, Corwin),
      Helping -> Set(Hercule, Garion, Fiona),
      Cooking -> Set(Corwin, Bianca),
      Fighting -> Set(Fiona, Eric, Daniela),
      Inking -> Set(Iago, Hercule, Garion),
      UnassignedMorning -> Set.empty,
      UnassignedAfternoon -> Set.empty,
      UnassignedEvening -> Set.empty
    ))
  }

  "personGroups" should "work" in {
    BestWithUnassignedTopics.personGroups.toSet should be(Set(
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

  "add" should "replace an existing record if one already exists for that slot and topic" in {
    Simple.add(Record(Morning, Acting, Set(Eric, Fiona))) should be(Schedule.from(
      Morning(
        Acting(Eric, Fiona),
        Cooking(Corwin, Daniela)
      ),
      AfterNoon(
        Bathing(Arthur, Bianca),
        Dancing(Corwin, Daniela)
      )
    ))
  }

  it should "add a new record with an existing slot and a new topic" in {
    Simple.add(Record(Morning, Eating, Set(Eric, Fiona))) should be(Schedule.from(
      Morning(
        Acting(Arthur, Bianca),
        Cooking(Corwin, Daniela),
        Eating(Eric, Fiona)
      ),
      AfterNoon(
        Bathing(Arthur, Bianca),
        Dancing(Corwin, Daniela)
      )
    ))
  }

  it should "add a new record with a new slot and a new topic" in {
    Simple.add(Record(Evening, Eating, Set(Eric, Fiona))) should be(Schedule.from(
      Morning(
        Acting(Arthur, Bianca),
        Cooking(Corwin, Daniela)
      ),
      AfterNoon(
        Bathing(Arthur, Bianca),
        Dancing(Corwin, Daniela)
      ),
      Evening(
        Eating(Eric, Fiona)
      )
    ))
  }

  /*
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
  } */

  "addPersonToExistingTopic" should "add someone to an existing topic" in {
    Simple.addPersonToExistingTopic(Morning, Acting, Eric) should be(Schedule.from(
      Morning(
        Acting(Arthur, Bianca, Eric),
        Cooking(Corwin, Daniela)
      ),
      AfterNoon(
        Bathing(Arthur, Bianca),
        Dancing(Corwin, Daniela)
      )
    ))
  }

  it should "not do anything with a non-existing topic" in {
    Simple.addPersonToExistingTopic(Morning, Grinding, Eric) should be(Simple)
  }

  "swapPersons" should "swap two persons on the same slot" in {
    Simple.swapPersons(Morning, Acting -> Arthur, Cooking -> Daniela) should be(Schedule.from(
      Morning(
        Acting(Daniela, Bianca),
        Cooking(Corwin, Arthur)
      ),
      AfterNoon(
        Bathing(Arthur, Bianca),
        Dancing(Corwin, Daniela)
      )
    ))
  }
  /*
    it should "not do anything if the topics are missing" in {
      Simple.swapPersons(Morning, Grinding -> Arthur, Cooking -> Daniela) should be (Simple)
    }

    it should "not do anything if the topics are wrong" in {
      Simple.swapPersons(Morning, Cooking -> Arthur, Cooking -> Daniela) should be (Simple)
    }

  it should "not do anything if the slot is missing" in {
    Simple.swapPersons(Evening, Acting -> Arthur, Cooking -> Daniela) should be(Simple)
  }
  */

  "movePerson" should "move a person from a source topic to a destination topic" in {
    Simple.movePerson(Morning, Acting, Cooking, Arthur) should be(Schedule.from(
      Morning(
        Acting(Bianca),
        Cooking(Corwin, Daniela, Arthur)
      ),
      AfterNoon(
        Bathing(Arthur, Bianca),
        Dancing(Corwin, Daniela)
      )
    ))
  }

  /*
  it should "not do anything if the topics are missing" in {
    Simple.movePerson(Morning, Acting, Grinding, Arthur) should be (Simple)
    Simple.movePerson(Morning, Grinding, Cooking, Arthur) should be (Simple)
  }

  it should "not do anything if the topics are wrong" in {
    Simple.movePerson(Morning, Cooking, Acting, Arthur) should be(Simple)
  }*/
  /*
    it should "not do anything if the slot is missing" in {
      Simple.movePerson(Evening, Acting, Cooking, Arthur) should be(Simple)
    }*/

  "isSound" should "validate a correct schedule" in {
    Simple.isSound should be(true)
    BestWithUnassignedTopics.isSound should be(true)
  }

  it should "reject a schedule demanding ubiquity" in {
    Schedule.from(
      Morning(
        Acting(Arthur, Bianca),
        Cooking(Arthur, Daniela)
      )
    ).isSound should be(false)
  }

  it should "reject a schedule with the same topic several times" in {
    Schedule.from(
      Morning(
        Acting(Arthur),
        Cooking(Corwin)
      ),
      AfterNoon(
        Acting(Corwin),
        Bathing(Arthur)
      )
    ).isSound should be(false)
  }


  "deltaScoreIfSwapPerson" should "work on a simple case" in {
    val schedule = Schedule.from(
      Morning(
        Acting(Arthur, Corwin),
        Dancing(Daniela, Garion)
      )
    )
    val schedule2 = Schedule.from(
      Morning(
        Acting(Arthur, Garion),
        Dancing(Daniela, Corwin)
      )
    )
    val schedule2bis = schedule.swapPersons(Morning, (Acting, Corwin), (Dancing, Garion))
    schedule2bis should be(schedule2)

    // Everyone on schedule 1 is at 0. On schedule 2, only Corwin is at 5. Having three persons less happy after him, his score is divided by 2^3
    (schedule2.score - schedule.score) should be(strongPreference / Weight(8))
    schedule.deltaScoreIfSwapPerson(Morning, (Acting, Corwin), (Dancing, Garion)) should be(Some(strongPreference / Weight(8)))
  }

  "Empty schedule" should "have a zero score on empty problem" in {
    Schedule.empty(0)(Problem.Empty, Context.Default).score should be(Score.Zero)
  }

  it should "have a negative-or-zero score on current problem" in {
    Schedule.empty(0).score should be <= Score.Zero
  }

  "startingUnassignedOrForced" should "return a schedule with all unassigned topics" in {
    implicit val rand: Random = new Random(0)
    val s = Schedule.startingUnassignedOrForced(0)
    println(s.toFormattedString)
    List(Morning, AfterNoon, Evening).foreach { slot =>
      val slotSchedule = s.on(slot)
      slotSchedule.topics.size should be(1)
      slotSchedule.topics.head.isUnassigned should be(true)
      slotSchedule.topics.head.name should be(s"@Unassigned (${slot.name})")
      slotSchedule.scheduledPersons should contain theSameElementsAs slot.personsPresent
    }
  }

}
