package fr.renoux.gaston

import fr.renoux.gaston.input.InputTranscription
import fr.renoux.gaston.model.impl.ProblemImpl
import fr.renoux.gaston.model.{Counts, Person, Slot, Topic}
import fr.renoux.gaston.util.BitMap.syntax._

import java.util.concurrent.atomic.AtomicInteger


object MinimalTestModel {

  object Persons {
    private val index = new AtomicInteger(0)
    val Leonardo = Person(index.getAndIncrement(), "Leonardo")
    val Raphael = Person(index.getAndIncrement(), "Raphael")
    val Donatello = Person(index.getAndIncrement(), "Donatello")
    val Michelangelo = Person(index.getAndIncrement(), "Michelangelo")
    val Bebop = Person(index.getAndIncrement(), "Bebop")
    val Rocksteady = Person(index.getAndIncrement(), "Rocksteady")

    val AllTurtles: Array[Person] = Array(Leonardo, Raphael, Donatello, Michelangelo)
    val AllEnemies: Array[Person] = Array(Bebop, Rocksteady)
    val All: Array[Person] = AllTurtles ++ AllEnemies
  }

  object Topics {
    private val index = new AtomicInteger(0)
    val Leading = Topic(index.getAndIncrement(), "leading")
    val Fighting = Topic(index.getAndIncrement(), "fighting")
    val Machines = Topic(index.getAndIncrement(), "machines")
    val Party = Topic(index.getAndIncrement(), "party")

    val Unassigned: Map[Slot, Topic] = Slots.All.flatten.map(s => s -> InputTranscription.unassignedTopic(index.getAndIncrement(), s)).toMap
    val Concrete: Array[Topic] = Array(Leading, Fighting, Machines, Party)
    val All: Array[Topic] = Concrete ++ Unassigned.values.toArray
  }

  object Slots {
    private val index = new AtomicInteger(0)
    val Morning = Slot(index.getAndIncrement(), "morning", Persons.All.toSet)
    val Afternoon = Slot(index.getAndIncrement(), "afternoon", Persons.All.toSet)
    val Evening = Slot(index.getAndIncrement(), "evening", Persons.All.toSet)
    val Night = Slot(index.getAndIncrement(), "night", Persons.All.toSet)

    val All: Array[Array[Slot]] = Array(Array(Morning, Afternoon, Evening, Night))
    val Count = All.flatten.length
  }

  object Problems {
    val MinimalCounts: Counts = Counts(slots = Slots.Count, topics = Topics.All.length, persons = Persons.All.length)
    import MinimalCounts.implicits._
    val Minimal = new ProblemImpl(Slots.All, Topics.All, Topics.Unassigned.toBitMap(), Persons.All, Array.empty, Array.empty)
  }

}
