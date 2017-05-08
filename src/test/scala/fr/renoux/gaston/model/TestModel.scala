package fr.renoux.gaston.model

import fr.renoux.gaston.model.constraints.{PersonPresence, PersonTopicInterdiction, PersonTopicObligation, TopicNeedsNumberOfPersons}
import fr.renoux.gaston.model.preferences.PersonTopicPreference

/**
  * Created by gael on 07/05/17.
  */
object TestModel {

  object Persons {
    val Leonardo = Person("Leonardo")
    val Raphael = Person("Raphael")
    val Donatello = Person("Donatello")
    val Michelangelo = Person("Michelangelo")
    val All = Set(Leonardo, Raphael, Donatello, Michelangelo)
  }

  object Topics {
    val Leading = Topic("leading")
    val Fighting = Topic("fighting")
    val Machines = Topic("machines")
    val Party = Topic("party")
    val Cooking = Topic("cooking")
    val All = Set(Leading, Fighting, Machines, Party, Cooking)
  }

  object Slots {
    val Morning = Slot("morning")
    val AfterNoon = Slot("afternoon")
    val Evening = Slot("evening")
    val Night = Slot("night")
    val Noonish = Slot("noonish")
    val All = Set(Morning, AfterNoon, Evening, Night, Noonish)
  }

  object Constraints {

    import Persons._
    import fr.renoux.gaston.model.preferences.Preference._
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

    val LeonardoLovesFighting = PersonTopicPreference(Leonardo, Fighting, Strong)
    val RaphealLovesPartying = PersonTopicPreference(Raphael, Party, Strong)
    val DonatelloLovesLeading = PersonTopicPreference(Donatello, Leading, Strong)
    val MichelangeloLovesMachines = PersonTopicPreference(Michelangelo, Machines, Strong)

    val LeonardoLikesMachines = PersonTopicPreference(Leonardo, Machines, Weak)
    val RaphealLikesLeading = PersonTopicPreference(Raphael, Leading, Weak)
    val DonatelloLikesPartying = PersonTopicPreference(Donatello, Party, Weak)
    val MichelangeloLikesFighting = PersonTopicPreference(Michelangelo, Fighting, Weak)

    val LeonardoInTheMorning = PersonPresence(Leonardo, Morning, presence = true)
    val RaphaelInTheAfternoon = PersonPresence(Raphael, AfterNoon, presence = true)
    val DonatelloInTheEvening = PersonPresence(Donatello, Evening, presence = true)
    val MichelangeloInTheNight = PersonPresence(Michelangelo, Night, presence = true)

    val LeonardoNotInTheNight = PersonPresence(Leonardo, Night, presence = false)
    val RaphaelNotInTheEvening = PersonPresence(Raphael, Evening, presence = false)
    val DonatelloInTheAfterNoon = PersonPresence(Donatello, AfterNoon, presence = false)
    val MichelangeloNotInTheMorning = PersonPresence(Michelangelo, Morning, presence = false)

    val FightingNeedsTwoToFourPersons = TopicNeedsNumberOfPersons(Fighting, min = 2, max = 4)

    val All = Set(
      LeonardoLeads,
      RaphaelFights,
      DonatelloDoesMachines,
      MichelangeloParties,

      LeonardoDoesNotParty,
      RaphealDoesNotDoMachines,
      DonatelloDoesNotFight,
      MichelangeloDoesNotLead,

      LeonardoLovesFighting,
      RaphealLovesPartying,
      DonatelloLovesLeading,
      MichelangeloLovesMachines,

      LeonardoLikesMachines,
      RaphealLikesLeading,
      DonatelloLikesPartying,
      MichelangeloLikesFighting,

      LeonardoInTheMorning,
      RaphaelInTheAfternoon,
      DonatelloInTheEvening,
      MichelangeloInTheNight,

      LeonardoNotInTheNight,
      RaphaelNotInTheEvening,
      DonatelloInTheAfterNoon,
      MichelangeloNotInTheMorning,

      FightingNeedsTwoToFourPersons
    )
  }

  object Problems {
    // val Complete = Problem(Persons.All, Topics.All, Slots.All, Constraints.All)
  }

  object Solutions {

    import Persons._
    import Slots._
    import Topics._

    val Terrible = Schedule(
      (Morning, Leading, Set(Michelangelo)),
      (AfterNoon, Fighting, Set(Donatello)),
      (Evening, Machines, Set(Raphael)),
      (Night, Party, Set(Leonardo))
    )

    val Perfect = Schedule(
      (Morning, Leading, Set(Leonardo, Donatello, Raphael)),
      (AfterNoon, Fighting, Set(Raphael, Leonardo, Michelangelo)),
      (Evening, Machines, Set(Donatello, Michelangelo, Leonardo)),
      (Night, Party, Set(Michelangelo, Raphael, Donatello)),
      (Noonish, Cooking, Set(Leonardo, Raphael, Donatello, Michelangelo))
    )
  }

}
