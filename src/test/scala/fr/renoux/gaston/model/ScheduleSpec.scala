package fr.renoux.gaston.model

import fr.renoux.gaston.engine.Context
import org.scalatest.{FlatSpec, Matchers}

class ScheduleSpec extends FlatSpec with Matchers {

  import fr.renoux.gaston.SimpleTestModel.Persons._
  import fr.renoux.gaston.SimpleTestModel.Problems._
  import fr.renoux.gaston.SimpleTestModel.Slots._
  import fr.renoux.gaston.SimpleTestModel.Solutions._
  import fr.renoux.gaston.SimpleTestModel.Topics._

  private implicit val problem: Problem = Complete
  private implicit val context: Context = Context.Default

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

  "add" should "add a new record even if one already exists for that slot and topic" in {
    Simple.add(Record(Morning, Acting, Set(Eric, Fiona))) should be(Schedule(
      Morning(
        Acting(Arthur, Bianca),
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
    Simple.add(Record(Morning, Eating, Set(Eric, Fiona))) should be(Schedule(
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
    Simple.add(Record(Evening, Eating, Set(Eric, Fiona))) should be(Schedule(
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
      AfterNoon(
        Bathing(Arthur, Bianca),
        Dancing(Corwin, Daniela)
      )
    ))
  }

  it should "not do anything with a non-existing topic" in {
    Simple.addPersonToExistingTopic(Grinding, Eric) should be(Simple)
  }

  "swapPersons" should "swap two persons on the same slot" in {
    Simple.swapPersons(Morning, Acting -> Arthur, Cooking -> Daniela) should be(Schedule(
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
    Simple.movePerson(Morning, Acting, Cooking, Arthur) should be(Schedule(
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

  it should "reject a schedule with the same topic several times" in {
    Schedule(
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

}
