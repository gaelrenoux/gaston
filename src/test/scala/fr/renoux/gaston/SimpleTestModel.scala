package fr.renoux.gaston

import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.input.{InputLoader, InputSettings}
import fr.renoux.gaston.model.*
import fr.renoux.gaston.model.Counts.given
import fr.renoux.gaston.model.constraints.*
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference}
import fr.renoux.gaston.util.{ArrayMap, Context}

import java.util.concurrent.atomic.AtomicInteger


/** 9 persons, 9 topics, 3 slots */
class SimpleTestModel(using settings: InputSettings) {

  val strongPreference: Score = Score(5)
  val weakPreference: Score = Score(1)

  object Persons {
    private val index = new AtomicInteger(0)
    val Arthur: Person = Person(index.getAndIncrement(), "Arthur")
    val Bianca: Person = Person(index.getAndIncrement(), "Bianca")
    val Corwin: Person = Person(index.getAndIncrement(), "Corwin")
    val Daniela: Person = Person(index.getAndIncrement(), "Daniela")
    val Eric: Person = Person(index.getAndIncrement(), "Eric")
    val Fiona: Person = Person(index.getAndIncrement(), "Fiona")
    val Garion: Person = Person(index.getAndIncrement(), "Garion")
    val Hercule: Person = Person(index.getAndIncrement(), "Hercule")
    val Iago: Person = Person(index.getAndIncrement(), "Iago")
    val All: Set[Person] = Set(Arthur, Bianca, Corwin, Daniela, Eric, Fiona, Garion, Hercule, Iago)
  }

  object Slots {
    private val index = new AtomicInteger(0)
    lazy val Morning: Slot = Slot(index.getAndIncrement(), "morning", Persons.All - Persons.Eric, Some(AfterNoon))
    lazy val AfterNoon: Slot = Slot(index.getAndIncrement(), "afternoon", Persons.All - Persons.Iago, Some(Evening))
    lazy val Evening: Slot = Slot(index.getAndIncrement(), "evening", Persons.All - Persons.Arthur, None)

    val All: Seq[Seq[Slot]] = Seq(Seq(Morning, AfterNoon, Evening))
    val Count = All.flatten.size
  }

  object Topics {

    import Persons.*

    private val index = new AtomicInteger(0)

    val UnassignedMorning: Topic = Topic.unassigned(index.getAndIncrement(), SimpleTestModel.Slots.Morning)
    val UnassignedAfternoon: Topic = Topic.unassigned(index.getAndIncrement(), SimpleTestModel.Slots.AfterNoon)
    val UnassignedEvening: Topic = Topic.unassigned(index.getAndIncrement(), Slots.Evening)

    val Acting: Topic = Topic(index.getAndIncrement(), "Acting", mandatory = Set(Arthur), forbidden = Set(Bianca), min = 2, max = 5)
    val Bathing: Topic = Topic(index.getAndIncrement(), "Bathing", mandatory = Set(Bianca), forbidden = Set(Corwin), min = 2, max = 5)
    val Cooking: Topic = Topic(index.getAndIncrement(), "Cooking", mandatory = Set(Corwin), forbidden = Set(Daniela), min = 2, max = 5)
    val Dancing: Topic = Topic(index.getAndIncrement(), "Dancing", mandatory = Set(Daniela), forbidden = Set(Eric), min = 2, max = 5)
    val Eating: Topic = Topic(index.getAndIncrement(), "Eating", mandatory = Set(Eric), forbidden = Set(Fiona), min = 2, max = 5)
    val Fighting: Topic = Topic(index.getAndIncrement(), "Fighting", mandatory = Set(Fiona), forbidden = Set(Garion), min = 2, max = 5)
    val Grinding: Topic = Topic(index.getAndIncrement(), "Grinding", mandatory = Set(Garion), forbidden = Set(Hercule), min = 2, max = 5)
    val Helping: Topic = Topic(index.getAndIncrement(), "Helping", mandatory = Set(Hercule), forbidden = Set(Iago), min = 2, max = 5)
    val Inking: Topic = Topic(index.getAndIncrement(), "Inking", mandatory = Set(Iago), forbidden = Set(Arthur), min = 2, max = 5)

    val Unassigned: Map[Slot, Topic] = Map(
      SimpleTestModel.Slots.Morning -> UnassignedMorning,
      SimpleTestModel.Slots.AfterNoon -> UnassignedAfternoon,
      SimpleTestModel.Slots.Evening -> UnassignedEvening
    )
    val Concrete: Set[Topic] = Set(Acting, Bathing, Cooking, Dancing, Eating, Fighting, Grinding, Helping, Inking)
    val All: Set[Topic] = Concrete ++ Unassigned.values
  }

  object ProblemCounts {
    implicit val CompleteCounts: Counts = Counts(slots = Slots.Count, topics = Topics.All.size, persons = Persons.All.size)
  }

  object Constraints {

    import Topics.*

    val BathingAndEatingAreSimultaneous: TopicsSimultaneous = TopicsSimultaneous(Set(Bathing, Eating))

    val All: Set[Constraint] = Set(
      BathingAndEatingAreSimultaneous
    )
  }

  object Preferences {

    import Persons.*
    import ProblemCounts.CompleteCounts
    import Topics.*

    val AB: PersonTopicPreference = PersonTopicPreference(Arthur, Bathing, strongPreference)
    val BC: PersonTopicPreference = PersonTopicPreference(Bianca, Cooking, strongPreference)
    val CD: PersonTopicPreference = PersonTopicPreference(Corwin, Dancing, strongPreference)
    val DE: PersonTopicPreference = PersonTopicPreference(Daniela, Eating, strongPreference)
    val EF: PersonTopicPreference = PersonTopicPreference(Eric, Fighting, strongPreference)
    val FG: PersonTopicPreference = PersonTopicPreference(Fiona, Grinding, strongPreference)
    val GH: PersonTopicPreference = PersonTopicPreference(Garion, Helping, strongPreference)
    val HI: PersonTopicPreference = PersonTopicPreference(Hercule, Inking, strongPreference)
    val IA: PersonTopicPreference = PersonTopicPreference(Iago, Acting, strongPreference)

    val AC: PersonTopicPreference = PersonTopicPreference(Arthur, Cooking, weakPreference)
    val BD: PersonTopicPreference = PersonTopicPreference(Bianca, Dancing, weakPreference)
    val CE: PersonTopicPreference = PersonTopicPreference(Corwin, Eating, weakPreference)
    val DF: PersonTopicPreference = PersonTopicPreference(Daniela, Fighting, weakPreference)
    val EG: PersonTopicPreference = PersonTopicPreference(Eric, Grinding, weakPreference)
    val FH: PersonTopicPreference = PersonTopicPreference(Fiona, Helping, weakPreference)
    val GI: PersonTopicPreference = PersonTopicPreference(Garion, Inking, weakPreference)
    val HA: PersonTopicPreference = PersonTopicPreference(Hercule, Acting, weakPreference)
    val IB: PersonTopicPreference = PersonTopicPreference(Iago, Bathing, weakPreference)

    val ArthurHatesFionaAndDaniela: PersonGroupAntiPreference = PersonGroupAntiPreference(Arthur, Set(Fiona, Daniela).toArraySet, settings.incompatibilityAntiPreference)

    val All: Set[Preference] = Set(
      AB, BC, CD, DE, EF, FG, GH, HI, IA,
      AC, BD, CE, DF, EG, FH, GI, HA, IB,
      ArthurHatesFionaAndDaniela
    )
  }

  object Problems {

    import ProblemCounts.CompleteCounts

    val WithUnassignedTopics = new Problem(Slots.All, Topics.All, Topics.Unassigned.toArrayMap, Persons.All, Constraints.All, Preferences.All)
    val NoUnassignedTopics = new Problem(Slots.All, Topics.Concrete, ArrayMap.empty[Slot, Topic], Persons.All, Constraints.All, Preferences.All)
  }

  object Solutions {

    import Persons.*
    import Slots.*
    import Topics.*

    private implicit val context: Context = Context.Default

    val BestWithUnassignedTopics: Schedule = {
      implicit val problem: Problem = Problems.WithUnassignedTopics
      Schedule.from(
        Morning(
          Acting(Arthur, Iago, Hercule),
          Dancing(Daniela, Corwin, Bianca),
          Grinding(Garion, Fiona),
          UnassignedMorning()
        ),
        AfterNoon(
          Bathing(Bianca, Arthur),
          Eating(Eric, Daniela, Corwin),
          Helping(Hercule, Garion, Fiona),
          UnassignedAfternoon()
        ),
        Evening(
          Cooking(Corwin, Bianca),
          Fighting(Fiona, Eric, Daniela),
          Inking(Iago, Hercule, Garion),
          UnassignedEvening()
        )
      )
    }

    val BestNoUnassignedTopics: Schedule = {
      implicit val problem: Problem = Problems.NoUnassignedTopics
      Schedule.from(
        Morning(
          Acting(Arthur, Iago, Hercule),
          Dancing(Daniela, Corwin, Bianca),
          Grinding(Garion, Fiona)
        ),
        AfterNoon(
          Bathing(Bianca, Arthur),
          Eating(Eric, Daniela, Corwin),
          Helping(Hercule, Garion, Fiona)
        ),
        Evening(
          Cooking(Corwin, Bianca),
          Fighting(Fiona, Eric, Daniela),
          Inking(Iago, Hercule, Garion)
        )
      )
    }

  }

}


object SimpleTestModel extends SimpleTestModel(using InputLoader.fromDefault.force.settings)
