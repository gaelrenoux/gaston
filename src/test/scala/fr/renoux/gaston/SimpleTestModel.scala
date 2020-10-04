package fr.renoux.gaston

import java.util.concurrent.atomic.AtomicInteger

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.input.{InputLoader, InputSettings, InputTranscription}
import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.impl.ProblemImpl
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference}
import fr.renoux.gaston.util.Context

// scalastyle:off magic.number
/** 9 persons, 9 topics, 3 slots */
class SimpleTestModel(implicit settings: InputSettings) {

  private val strongPreference = Score(5)
  private val weakPreference = Score(1)

  object Persons {
    private val index = new AtomicInteger(0)
    val Arthur = Person(index.getAndIncrement(), "Arthur")
    val Bianca = Person(index.getAndIncrement(), "Bianca")
    val Corwin = Person(index.getAndIncrement(), "Corwin")
    val Daniela = Person(index.getAndIncrement(), "Daniela")
    val Eric = Person(index.getAndIncrement(), "Eric")
    val Fiona = Person(index.getAndIncrement(), "Fiona")
    val Garion = Person(index.getAndIncrement(), "Garion")
    val Hercule = Person(index.getAndIncrement(), "Hercule")
    val Iago = Person(index.getAndIncrement(), "Iago")
    val All: Set[Person] = Set(Arthur, Bianca, Corwin, Daniela, Eric, Fiona, Garion, Hercule, Iago)
  }

  object Slots {
    private val index = new AtomicInteger(0)
    val Morning = Slot(index.getAndIncrement(), "morning", Persons.All - Persons.Eric)
    val AfterNoon = Slot(index.getAndIncrement(), "afternoon", Persons.All - Persons.Iago)
    val Evening = Slot(index.getAndIncrement(), "evening", Persons.All - Persons.Arthur)
    val All: Seq[Seq[Slot]] = Seq(Seq(Morning, AfterNoon, Evening))
    val Count = All.flatten.size
  }

  object Topics {

    import Persons._

    private val index = new AtomicInteger(0)

    val UnassignedMorning: Topic = InputTranscription.unassignedTopic(index.getAndIncrement(), MinimalTestModel.Slots.Morning)
    val UnassignedAfternoon: Topic = InputTranscription.unassignedTopic(index.getAndIncrement(), MinimalTestModel.Slots.Afternoon)
    val UnassignedEvening: Topic = InputTranscription.unassignedTopic(index.getAndIncrement(), Slots.Evening)

    val Acting = Topic(index.getAndIncrement(), "Acting", mandatory = Set(Arthur), forbidden = Set(Bianca), min = 2, max = 5)
    val Bathing = Topic(index.getAndIncrement(), "Bathing", mandatory = Set(Bianca), forbidden = Set(Corwin), min = 2, max = 5)
    val Cooking = Topic(index.getAndIncrement(), "Cooking", mandatory = Set(Corwin), forbidden = Set(Daniela), min = 2, max = 5)
    val Dancing = Topic(index.getAndIncrement(), "Dancing", mandatory = Set(Daniela), forbidden = Set(Eric), min = 2, max = 5)
    val Eating = Topic(index.getAndIncrement(), "Eating", mandatory = Set(Eric), forbidden = Set(Fiona), min = 2, max = 5)
    val Fighting = Topic(index.getAndIncrement(), "Fighting", mandatory = Set(Fiona), forbidden = Set(Garion), min = 2, max = 5)
    val Grinding = Topic(index.getAndIncrement(), "Grinding", mandatory = Set(Garion), forbidden = Set(Hercule), min = 2, max = 5)
    val Helping = Topic(index.getAndIncrement(), "Helping", mandatory = Set(Hercule), forbidden = Set(Iago), min = 2, max = 5)
    val Inking = Topic(index.getAndIncrement(), "Inking", mandatory = Set(Iago), forbidden = Set(Arthur), min = 2, max = 5)

    val Unassigned = Map(
      MinimalTestModel.Slots.Morning -> UnassignedMorning,
      MinimalTestModel.Slots.Afternoon -> UnassignedAfternoon,
      MinimalTestModel.Slots.Evening -> UnassignedEvening
    )
    val Concrete: Set[Topic] = Set(Acting, Bathing, Cooking, Dancing, Eating, Fighting, Grinding, Helping, Inking)
    val All: Set[Topic] = Concrete ++ Unassigned.values
  }

  object ProblemCounts {
    implicit val CompleteCounts: Counts = Counts(slots = Slots.Count, topics = Topics.All.size, persons = Persons.All.size)
  }

  object Constraints {

    import Topics._

    val BathingAndEatingAreSimultaneous = TopicsSimultaneous(Set(Bathing, Eating))

    val All: Set[Constraint] = Set(
      BathingAndEatingAreSimultaneous
    )
  }

  object Preferences {

    import Persons._
    import Topics._
    import ProblemCounts.CompleteCounts

    val AB = PersonTopicPreference(Arthur, Bathing, strongPreference)
    val BC = PersonTopicPreference(Bianca, Cooking, strongPreference)
    val CD = PersonTopicPreference(Corwin, Dancing, strongPreference)
    val DE = PersonTopicPreference(Daniela, Eating, strongPreference)
    val EF = PersonTopicPreference(Eric, Fighting, strongPreference)
    val FG = PersonTopicPreference(Fiona, Grinding, strongPreference)
    val GH = PersonTopicPreference(Garion, Helping, strongPreference)
    val HI = PersonTopicPreference(Hercule, Inking, strongPreference)
    val IA = PersonTopicPreference(Iago, Acting, strongPreference)

    val AC = PersonTopicPreference(Arthur, Cooking, weakPreference)
    val BD = PersonTopicPreference(Bianca, Dancing, weakPreference)
    val CE = PersonTopicPreference(Corwin, Eating, weakPreference)
    val DF = PersonTopicPreference(Daniela, Fighting, weakPreference)
    val EG = PersonTopicPreference(Eric, Grinding, weakPreference)
    val FH = PersonTopicPreference(Fiona, Helping, weakPreference)
    val GI = PersonTopicPreference(Garion, Inking, weakPreference)
    val HA = PersonTopicPreference(Hercule, Acting, weakPreference)
    val IB = PersonTopicPreference(Iago, Bathing, weakPreference)

    val ArthurHatesFionaAndDaniela = PersonGroupAntiPreference(Arthur, Set(Fiona, Daniela).toBitSet, settings.incompatibilityAntiPreference.value)

    val All: Set[Preference] = Set(
      AB, BC, CD, DE, EF, FG, GH, HI, IA,
      AC, BD, CE, DF, EG, FH, GI, HA, IB,
      ArthurHatesFionaAndDaniela
    )
  }

  object Problems {
    import ProblemCounts.CompleteCounts
    val Complete = new ProblemImpl(Slots.All, Topics.All, Topics.Unassigned.toBitMap, Persons.All, Constraints.All, Preferences.All)
  }

  object Solutions {

    import Persons._
    import Slots._
    import Topics._

    private implicit val problem: Problem = Problems.Complete
    private implicit val context: Context = Context.Default

    val Best = Schedule.from(
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


object SimpleTestModel extends SimpleTestModel()(InputLoader.fromDefault.force.settings)

// scalastyle:on magic.number
