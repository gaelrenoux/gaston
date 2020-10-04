package fr.renoux.gaston

import java.util.concurrent.atomic.AtomicInteger

import fr.renoux.gaston.input.InputTranscription
import fr.renoux.gaston.model.impl.ProblemImpl
import fr.renoux.gaston.model.{Counts, Person, Slot, Topic}


object MinimalTestModel {

  object Persons {
    private val index = new AtomicInteger(0)
    val Leonardo = Person(index.getAndIncrement(), "Leonardo")
    val Raphael = Person(index.getAndIncrement(), "Raphael")
    val Donatello = Person(index.getAndIncrement(), "Donatello")
    val Michelangelo = Person(index.getAndIncrement(), "Michelangelo")
    val Bebop = Person(index.getAndIncrement(), "Bebop")
    val Rocksteady = Person(index.getAndIncrement(), "Rocksteady")

    val AllTurtles: Set[Person] = Set(Leonardo, Raphael, Donatello, Michelangelo)
    val AllEnemies: Set[Person] = Set(Bebop, Rocksteady)
    val All: Set[Person] = AllTurtles ++ AllEnemies
  }

  object Topics {
    private val index = new AtomicInteger(0)
    val Leading = Topic(index.getAndIncrement(), "leading")
    val Fighting = Topic(index.getAndIncrement(), "fighting")
    val Machines = Topic(index.getAndIncrement(), "machines")
    val Party = Topic(index.getAndIncrement(), "party")

    val Unassigned: Map[Slot, Topic] = Slots.All.flatten.map(s => s -> InputTranscription.unassignedTopic(index.getAndIncrement(), s)).toMap
    val Concrete: Set[Topic] = Set(Leading, Fighting, Machines, Party)
    val All: Set[Topic] = Concrete ++ Unassigned.values
  }

  object Slots {
    private val index = new AtomicInteger(0)
    val Morning = Slot(index.getAndIncrement(), "morning", Persons.All)
    val Afternoon = Slot(index.getAndIncrement(), "afternoon", Persons.All)
    val Evening = Slot(index.getAndIncrement(), "evening", Persons.All)
    val Night = Slot(index.getAndIncrement(), "night", Persons.All)

    val All: Seq[Seq[Slot]] = Seq(Seq(Morning, Afternoon, Evening, Night))
    val Count = All.flatten.size
  }

  object Problems {
    implicit val MinimalCounts = Counts(slots = Slots.Count, topics = Topics.All.size, persons = Persons.All.size)
    val Minimal = new ProblemImpl(Slots.All, Topics.All, Topics.Unassigned, Persons.All, Set.empty, Set.empty)
  }

}
