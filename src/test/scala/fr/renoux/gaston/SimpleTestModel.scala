package fr.renoux.gaston

import fr.renoux.gaston.io.InputSettings
import fr.renoux.gaston.model.Schedule.Record
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference, Preference}
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Schedule, Slot, Topic}

/**
  * Created by gael on 07/05/17.
  */
class SimpleTestModel(implicit settings: InputSettings) {

  object Persons {
    val Leonardo = Person("Leonardo")
    val Raphael = Person("Raphael")
    val Donatello = Person("Donatello")
    val Michelangelo = Person("Michelangelo")
    val Bebop = Person("Bebop")
    val Rocksteady = Person("Rocksteady")
    val AllTurtles: Set[Person] = Set(Leonardo, Raphael, Donatello, Michelangelo)
    val AllEnemies: Set[Person] = Set(Bebop, Rocksteady)
  }

  object Topics {
    val Leading = Topic("leading")
    val Fighting = Topic("fighting")
    val Machines = Topic("machines")
    val Party = Topic("party")
    val Cooking = Topic("cooking")
    val All: Set[Topic] = Set(Leading, Fighting, Machines, Party, Cooking)
  }

  object Slots {
    val Morning = Slot("morning")
    val AfterNoon = Slot("afternoon")
    val Evening = Slot("evening")
    val Night = Slot("night")
    val Noonish = Slot("noonish")
    val All: Set[Slot] = Set(Morning, AfterNoon, Evening, Night, Noonish)
  }

  object Constraints {

    import Persons._
    import Slots._
    import Topics._

    val LeonardoLeads = PersonTopicObligation(Leonardo, Leading)
    val RaphaelFights = PersonTopicObligation(Raphael, Fighting)
    val DonatelloDoesMachines = PersonTopicObligation(Donatello, Machines)
    val MichelangeloParties = PersonTopicObligation(Michelangelo, Party)

    val LeonardoDoesNotParty = PersonTopicInterdiction(Leonardo, Party)
    val RaphealDoesNotDoMachines = PersonTopicInterdiction(Raphael, Machines)
    val DonatelloDoesNotFight = PersonTopicInterdiction(Donatello, Fighting)
    val MichelangeloDoesNotLead = PersonTopicInterdiction(Michelangelo, Leading)

    val LeonardoNotInTheNight = PersonAbsence(Leonardo, Night)
    val RaphaelNotInTheEvening = PersonAbsence(Raphael, Evening)
    val DonatelloInTheAfterNoon = PersonAbsence(Donatello, AfterNoon)
    val MichelangeloNotInTheMorning = PersonAbsence(Michelangelo, Morning)

    val FightingNeedsTwoToFourPersons = TopicNeedsNumberOfPersons(Fighting, min = 2, max = 4)

    val All: Set[Constraint] = Set(
      LeonardoLeads,
      RaphaelFights,
      DonatelloDoesMachines,
      MichelangeloParties,

      LeonardoDoesNotParty,
      RaphealDoesNotDoMachines,
      DonatelloDoesNotFight,
      MichelangeloDoesNotLead,

      LeonardoNotInTheNight,
      RaphaelNotInTheEvening,
      DonatelloInTheAfterNoon,
      MichelangeloNotInTheMorning,

      FightingNeedsTwoToFourPersons
    )
  }

  object Preferences {

    import Persons._
    import Topics._

    val LeonardoLovesFighting = PersonTopicPreference(Leonardo, Fighting, settings.strongPreference)
    val RaphaelLovesPartying = PersonTopicPreference(Raphael, Party, settings.strongPreference)
    val DonatelloLovesLeading = PersonTopicPreference(Donatello, Leading, settings.strongPreference)
    val MichelangeloLovesMachines = PersonTopicPreference(Michelangelo, Machines, settings.strongPreference)

    val LeonardoLikesMachines = PersonTopicPreference(Leonardo, Machines, settings.weakPreference)
    val RaphaelLikesLeading = PersonTopicPreference(Raphael, Leading, settings.weakPreference)
    val DonatelloLikesPartying = PersonTopicPreference(Donatello, Party, settings.weakPreference)
    val MichelangeloLikesFighting = PersonTopicPreference(Michelangelo, Fighting, settings.weakPreference)

    val LeonardoHatesEnemies = PersonGroupAntiPreference(Leonardo, AllEnemies, settings.strongPreference.negative)
    val RaphaelHatesEnemies = PersonGroupAntiPreference(Raphael, AllEnemies, settings.strongPreference.negative)
    val DonatelloHatesEnemies = PersonGroupAntiPreference(Donatello, AllEnemies, settings.strongPreference.negative)
    val MichelangeloHatesEnemies = PersonGroupAntiPreference(Michelangelo, AllEnemies, settings.strongPreference.negative)

    val All: Set[Preference] = Set(
      LeonardoLovesFighting,
      RaphaelLovesPartying,
      DonatelloLovesLeading,
      MichelangeloLovesMachines,

      LeonardoLikesMachines,
      RaphaelLikesLeading,
      DonatelloLikesPartying,
      MichelangeloLikesFighting
    )
  }

  object Problems {
    val Complete = Problem(Slots.All, Topics.All, Persons.AllTurtles, Constraints.All, Preferences.All)
  }

  object Solutions {

    import Persons._
    import Slots._
    import Topics._

    val Terrible = Schedule(
      Record(Morning, Leading, Set(Michelangelo, Rocksteady)),
      Record(AfterNoon, Fighting, Set(Donatello)),
      Record(Evening, Machines, Set(Raphael, Bebop)),
      Record(Night, Party, Set(Leonardo, Bebop, Rocksteady))
    )

    val Perfect = Schedule(
      Record(Morning, Leading, Set(Leonardo, Donatello, Raphael)),
      Record(AfterNoon, Fighting, Set(Raphael, Leonardo, Michelangelo)),
      Record(Evening, Machines, Set(Donatello, Michelangelo, Leonardo)),
      Record(Night, Party, Set(Michelangelo, Raphael, Donatello)),
      Record(Noonish, Cooking, Set(Leonardo, Raphael, Donatello, Michelangelo))
    )
  }

}


object SimpleTestModel {
  def apply(implicit settings: InputSettings): SimpleTestModel = new SimpleTestModel
}
