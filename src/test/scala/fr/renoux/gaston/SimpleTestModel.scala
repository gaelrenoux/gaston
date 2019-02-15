package fr.renoux.gaston

import fr.renoux.gaston.input.InputSettings
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference, Preference}
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model._

/** 9 persons, 9 topics, 3 slots */
class SimpleTestModel(implicit settings: InputSettings) {

  private val strongPreference = Score(5)
  private val weakPreference = Score(1)

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
    val Acting = Topic("Acting")
    val Bathing = Topic("Bathing")
    val Cooking = Topic("Cooking")
    val Dancing = Topic("Dancing")
    val Eating = Topic("Eating")
    val Fighting = Topic("Fighting")
    val Grinding = Topic("Grinding")
    val Helping = Topic("Helping")
    val Inking = Topic("Inking")
    val All: Set[Topic] = Set(Acting, Bathing, Cooking, Dancing, Eating, Fighting, Grinding, Helping, Inking)
  }

  object Slots {
    val Morning = Slot("morning")
    val AfterNoon = Slot("afternoon")
    val Evening = Slot("evening")
    val All: Set[Slot] = Set(Morning, AfterNoon, Evening)
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

    val DefaultMinMaxes: Set[TopicNeedsNumberOfPersons] = Topics.All
      .map(TopicNeedsNumberOfPersons(_, min = 2, max = 5))

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
    val Complete = Problem(3, Slots.All, Topics.All, Persons.All, Constraints.All, Preferences.All)
  }

  object Solutions {

    import Persons._
    import Slots._
    import Topics._

    val Best = Schedule(Map(
      Morning -> Map(
        Acting -> Set(Arthur, Iago, Hercule),
        Dancing -> Set(Daniela, Corwin, Bianca),
        Grinding -> Set(Garion, Fiona)
      ),
      AfterNoon -> Map(
        Bathing -> Set(Bianca, Arthur),
        Eating -> Set(Eric, Daniela, Corwin),
        Helping -> Set(Hercule, Garion, Fiona)
      ),
      Evening -> Map(
        Cooking -> Set(Corwin, Bianca),
        Fighting -> Set(Fiona, Eric, Daniela),
        Inking -> Set(Iago, Hercule, Garion)
      )
    ))

  }

}


object SimpleTestModel {
  def apply(implicit settings: InputSettings): SimpleTestModel = new SimpleTestModel
}
