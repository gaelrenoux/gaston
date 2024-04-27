package fr.renoux.gaston

import fr.renoux.gaston.input.InputTranscription
import fr.renoux.gaston.model.{Counts, Person, Problem, Slot, Topic}
import fr.renoux.gaston.util.Count

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

    val AllTurtles: Set[Person] = Set(Leonardo, Raphael, Donatello, Michelangelo)
    val AllEnemies: Set[Person] = Set(Bebop, Rocksteady)
    val All: Set[Person] = AllTurtles ++ AllEnemies
    implicit val PersonCount: Count[Person] = util.Count[Person](All.size)
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
    implicit val TopicCount: Count[Topic] = util.Count[Topic](All.size)
  }

  object Slots {
    private val index = new AtomicInteger(0)
    lazy val Morning = Slot(index.getAndIncrement(), "morning", Persons.All, Some(Afternoon))
    lazy val Afternoon = Slot(index.getAndIncrement(), "afternoon", Persons.All, Some(Evening))
    lazy val Evening = Slot(index.getAndIncrement(), "evening", Persons.All, Some(Night))
    lazy val Night = Slot(index.getAndIncrement(), "night", Persons.All, None)

    val All: Seq[Seq[Slot]] = Seq(Seq(Morning, Afternoon, Evening, Night))
    implicit val SlotCount: Count[Slot] = util.Count[Slot](All.flatten.size)
  }

  object Problems {
    implicit val MinimalCounts: Counts = Counts.fromCounts(Slots.SlotCount, Topics.TopicCount, Persons.PersonCount)
    val Minimal = new Problem(Slots.All, Topics.All, Topics.Unassigned.toBitMap, Persons.All, Set.empty, Set.empty)
  }

}
