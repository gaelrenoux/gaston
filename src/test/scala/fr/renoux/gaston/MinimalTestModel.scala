package fr.renoux.gaston

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
  }

  object Topics {
    val Leading = Topic("leading")
    val Fighting = Topic("fighting")
    val Machines = Topic("machines")
    val Party = Topic("party")

    val All: Set[Topic] = Set(Leading, Fighting, Machines, Party)
  }

  object Slots {
    val Morning = Slot("morning")
    val AfterNoon = Slot("afternoon")
    val Evening = Slot("evening")
    val Night = Slot("night")

    val All: Set[Slot] = Set(Morning, AfterNoon, Evening, Night)
  }

}