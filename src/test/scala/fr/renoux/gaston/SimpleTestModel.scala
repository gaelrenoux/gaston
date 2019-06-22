package fr.renoux.gaston

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.engine.Context
import fr.renoux.gaston.input.{InputLoader, InputSettings}
import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference}
import fr.renoux.gaston.model.impl.ProblemImpl

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
    val BaseMorning: Topic = Topic.unassigned(Slots.Morning)
    val BaseAfternoon: Topic = Topic.unassigned(Slots.AfterNoon)
    val BaseEvening: Topic = Topic.unassigned(Slots.Evening)

    val Acting = Topic("Acting")
    val Bathing = Topic("Bathing")
    val Cooking = Topic("Cooking")
    val Dancing = Topic("Dancing")
    val Eating = Topic("Eating")
    val Fighting = Topic("Fighting")
    val Grinding = Topic("Grinding")
    val Helping = Topic("Helping")
    val Inking = Topic("Inking")
    val Bases = Set(BaseMorning, BaseAfternoon, BaseEvening)
    val All: Set[Topic] = Set(Acting, Bathing, Cooking, Dancing, Eating, Fighting, Grinding, Helping, Inking)
  }

  object Constraints {

    import Persons._
    import Slots._
    import Topics._

    val AA = PersonTopicObligation(Arthur, Acting)
    val BB = PersonTopicObligation(Bianca, Bathing)
    val CC = PersonTopicObligation(Corwin, Cooking)
    val DD = PersonTopicObligation(Daniela, Dancing)
    val EE = PersonTopicObligation(Eric, Eating)
    val FF = PersonTopicObligation(Fiona, Fighting)
    val GG = PersonTopicObligation(Garion, Grinding)
    val HH = PersonTopicObligation(Hercule, Helping)
    val II = PersonTopicObligation(Iago, Inking)

    val NotAI = PersonTopicInterdiction(Arthur, Inking)
    val NotBA = PersonTopicInterdiction(Bianca, Acting)
    val NotCB = PersonTopicInterdiction(Corwin, Bathing)
    val NotDC = PersonTopicInterdiction(Daniela, Cooking)
    val NotED = PersonTopicInterdiction(Eric, Dancing)
    val NotFE = PersonTopicInterdiction(Fiona, Eating)
    val NotGF = PersonTopicInterdiction(Garion, Fighting)
    val NotHG = PersonTopicInterdiction(Hercule, Grinding)
    val NotIH = PersonTopicInterdiction(Iago, Helping)

    val EricNotInTheMorning = PersonAbsence(Eric, Morning)
    val IagoNotInTheAfterNoon = PersonAbsence(Iago, AfterNoon)
    val ArthurNotInTheEvening = PersonAbsence(Arthur, Evening)

    val DefaultMinMaxes: Set[TopicNeedsNumberOfPersons] =
      Topics.Bases.map(TopicNeedsNumberOfPersons(_, min = 0, max = 5000)) ++
        Topics.All.map(TopicNeedsNumberOfPersons(_, min = 2, max = 5))

    val BathingAndEatingAreSimultaneous = TopicsSimultaneous(Set(Bathing, Eating))

    val All: Set[Constraint] = Set(
      AA,
      BB,
      CC,
      DD,
      EE,
      FF,
      GG,
      HH,
      II,

      NotAI,
      NotBA,
      NotCB,
      NotDC,
      NotED,
      NotFE,
      NotGF,
      NotHG,
      NotIH,

      EricNotInTheMorning,
      IagoNotInTheAfterNoon,
      ArthurNotInTheEvening,

      BathingAndEatingAreSimultaneous
    ) ++ DefaultMinMaxes
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

    val ArthurHatesFionaAndDaniela = PersonGroupAntiPreference(Arthur, Set(Fiona, Daniela), settings.incompatibilityAntiPreference)

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
