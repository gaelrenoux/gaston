package fr.renoux.gaston

import fr.renoux.gaston.model.impl.ProblemImpl
import fr.renoux.gaston.model.{Person, Slot, Topic}


object MinimalTestModel {

  object Persons {
    val Leonardo = Person("Leonardo")
    val Raphael = Person("Raphael")
    val Donatello = Person("Donatello")
    val Michelangelo = Person("Michelangelo")
    val Bebop = Person("Bebop")
    val Rocksteady = Person("Rocksteady")

    val AllTurtles: Set[Person] = Set(Leonardo, Raphael, Donatello, Michelangelo)
    val AllEnemies: Set[Person] = Set(Bebop, Rocksteady)
    val All: Set[Person] = AllTurtles ++ AllEnemies
  }

  object Topics {
    val Leading = Topic("leading")
    val Fighting = Topic("fighting")
    val Machines = Topic("machines")
    val Party = Topic("party")

    val All: Set[Topic] = Set(Leading, Fighting, Machines, Party)
  }

  object Slots {
    val Morning = Slot("morning", Persons.All)
    val AfterNoon = Slot("afternoon", Persons.All)
    val Evening = Slot("evening", Persons.All)
    val Night = Slot("night", Persons.All)

    val All: Seq[Seq[Slot]] = Seq(Seq(Morning, AfterNoon, Evening, Night))
  }

  object Problems {
    val Minimal = new ProblemImpl(Slots.All, Topics.All, Persons.All, Set.empty, Set.empty)
  }

}