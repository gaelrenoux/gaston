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
  import fr.renoux.gaston.SimpleTestModel.{strongPreference, weakPreference}

  given Problem = WithUnassignedTopics
  given Context = Context.Default

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

  "++" should "work with two schedules" in {
    val simple2 = Schedule.from(
      AfterNoon(
        Bathing(Arthur, Eric), //new person on existing topic
        Eating(Fiona, Garion) //new topic on existing slot
      ),
      Evening( //new slot
        Fighting(Arthur, Bianca)
      )
    )

    val result = Schedule.from(
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

    (Simple ++ simple2) should be(result)
    (simple2 ++ Simple) should be(result)
  }

  it should "work with an empty schedule" in {
    (Schedule.empty(0) ++ Simple) should be(Simple)
    (Simple ++ Schedule.empty(0)) should be(Simple)
  }

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

  it should "throw an exception if the slot is missing" in {
    a[NoSuchElementException] shouldBe thrownBy {
      Simple.swapPersons(Evening, Acting -> Arthur, Cooking -> Daniela)
    }
  }

  it should "throw an exception if the topics are missing" in {
    a[NoSuchElementException] shouldBe thrownBy {
      Simple.swapPersons(Morning, Grinding -> Arthur, Cooking -> Daniela)
    }
  }

  // behavior of swapPersons if the source topics are wrong is undetermined

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

  it should "throw an exception if the slot is missing" in {
    a[NoSuchElementException] shouldBe thrownBy {
      Simple.movePerson(Evening, Acting, Cooking, Arthur)
    }
  }

  it should "throw an exception if the topics are missing" in {
    a[NoSuchElementException] shouldBe thrownBy {
      Simple.movePerson(Morning, Acting, Grinding, Arthur)
    }
    a[NoSuchElementException] shouldBe thrownBy {
      Simple.movePerson(Morning, Grinding, Cooking, Arthur)
    }
  }

  // behavior of movePerson if the source topics are wrong is undetermined

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

    // Corwin at 1 on schedule 1 (its base score) and at 6 on schedule 2, everyone else stays at 0. Having 8 more persons less happy after him, his score is divided by 2^8
    val factor = 1.0 / 256
    schedule.score should be(Score(1.0) * factor)
    schedule2.score should be((strongPreference + weakPreference) * factor)
    schedule.deltaScoreIfSwapPerson(Morning, (Acting, Corwin), (Dancing, Garion)) should be(Some(schedule2.score - schedule.score))
  }

  "Empty schedule" should "have a zero score on empty problem" in {
    Schedule.empty(0)(using Problem.Empty, Context.Default).score should be(Score.Zero)
  }

  it should "have a score on current problem that only takes base-scores" in {
    val corwinScore = weakPreference * (1.0 / math.pow(2, 8)) // his base-score makes him the best-scored person
    Schedule.empty(0).score should be(corwinScore)
  }

  "startingUnassignedOrForced" should "return a schedule with all unassigned topics" in {
    given Random = new Random(0)
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
