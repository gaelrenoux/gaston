package fr.renoux.gaston

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.engine.Context
import fr.renoux.gaston.input.{InputLoader, InputSettings}
import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference}
import fr.renoux.gaston.model.impl.ProblemImpl

// scalastyle:off magic.number
/** 9 persons, 9 topics, 3 slots */
class SimpleTestModel(implicit settings: InputSettings) {

  private val strongPreference = Score(5)
  private val weakPreference = Score(1)

  object Slots {
    val Morning = Slot("morning")
    val AfterNoon = Slot("afternoon")
    val Evening = Slot("evening")
    val All: Seq[Seq[Slot]] = Seq(Seq(Morning, AfterNoon, Evening))
  }

  object Persons {
    val Arthur = Person("Arthur")
    val Bianca = Person("Bianca")
    val Corwin = Person("Corwin")
    val Daniela = Person("Daniela")
    val Eric = Person("Eric")
    val Fiona = Person("Fiona")
    val Garion = Person("Garion")
    val Hercule = Person("Hercule")
    val Iago = Person("Iago")
    val All: Set[Person] = Set(Arthur, Bianca, Corwin, Daniela, Eric, Fiona, Garion, Hercule, Iago)
  }

  object Topics {
    import Persons._

    val BaseMorning: Topic = Topic.unassigned(Slots.Morning)
    val BaseAfternoon: Topic = Topic.unassigned(Slots.AfterNoon)
    val BaseEvening: Topic = Topic.unassigned(Slots.Evening)

    val Acting = Topic("Acting", mandatory = Set(Arthur), forbidden = Set(Bianca), min = 2, max = 5)
    val Bathing = Topic("Bathing", mandatory = Set(Bianca), forbidden = Set(Corwin), min = 2, max = 5)
    val Cooking = Topic("Cooking", mandatory = Set(Corwin), forbidden = Set(Daniela), min = 2, max = 5)
    val Dancing = Topic("Dancing", mandatory = Set(Daniela), forbidden = Set(Eric), min = 2, max = 5)
    val Eating = Topic("Eating", mandatory = Set(Eric), forbidden = Set(Fiona), min = 2, max = 5)
    val Fighting = Topic("Fighting", mandatory = Set(Fiona), forbidden = Set(Garion), min = 2, max = 5)
    val Grinding = Topic("Grinding", mandatory = Set(Garion), forbidden = Set(Hercule), min = 2, max = 5)
    val Helping = Topic("Helping", mandatory = Set(Hercule), forbidden = Set(Iago), min = 2, max = 5)
    val Inking = Topic("Inking", mandatory = Set(Iago), forbidden = Set(Arthur), min = 2, max = 5)

    val Bases = Set(BaseMorning, BaseAfternoon, BaseEvening)
    val All: Set[Topic] = Set(Acting, Bathing, Cooking, Dancing, Eating, Fighting, Grinding, Helping, Inking)
  }

  object Constraints {

    import Persons._
    import Slots._
    import Topics._

    val EricNotInTheMorning = PersonAbsence(Eric, Morning)
    val IagoNotInTheAfterNoon = PersonAbsence(Iago, AfterNoon)
    val ArthurNotInTheEvening = PersonAbsence(Arthur, Evening)
    val BathingAndEatingAreSimultaneous = TopicsSimultaneous(Set(Bathing, Eating))

    val All: Set[Constraint] = Set(
      EricNotInTheMorning,
      IagoNotInTheAfterNoon,
      ArthurNotInTheEvening,
      BathingAndEatingAreSimultaneous
    )
  }

  object Preferences {

    import Persons._
    import Topics._

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

    val ArthurHatesFionaAndDaniela = PersonGroupAntiPreference(Arthur, Set(Fiona, Daniela), settings.incompatibilityAntiPreference.value)

    val All: Set[Preference] = Set(
      AB, BC, CD, DE, EF, FG, GH, HI, IA,
      AC, BD, CE, DF, EG, FH, GI, HA, IB,
      ArthurHatesFionaAndDaniela
    )
  }

  object Problems {
    val Complete = new ProblemImpl(Slots.All, Topics.Bases ++ Topics.All, Persons.All, Constraints.All, Preferences.All)
  }

  object Solutions {

    import Persons._
    import Slots._
    import Topics._

    private implicit val problem: Problem = Problems.Complete
    private implicit val context: Context = Context.Default

    val Best = Schedule(
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
