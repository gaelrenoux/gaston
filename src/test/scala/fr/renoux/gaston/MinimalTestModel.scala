package fr.renoux.gaston

import fr.renoux.gaston.model.Counts.given
import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.Count

import java.util.concurrent.atomic.AtomicInteger


/** This test model problem is trivial to solve. It is used for simple tests on constraints, preferences, etc. */
object MinimalTestModel {

  object Persons {
    private val index = new AtomicInteger(0)
    val Leonardo: Person = Person(index.getAndIncrement(), "Leonardo")
    val Raphael: Person = Person(index.getAndIncrement(), "Raphael")
    val Donatello: Person = Person(index.getAndIncrement(), "Donatello")
    val Michelangelo: Person = Person(index.getAndIncrement(), "Michelangelo")
    val Bebop: Person = Person(index.getAndIncrement(), "Bebop")
    val Rocksteady: Person = Person(index.getAndIncrement(), "Rocksteady")

    val AllTurtles: Set[Person] = Set(Leonardo, Raphael, Donatello, Michelangelo)
    val AllEnemies: Set[Person] = Set(Bebop, Rocksteady)
    val All: Set[Person] = AllTurtles ++ AllEnemies
    implicit val PersonCount: Count[Person] = util.Count[Person](All.size)
  }

  object Topics {
    private val index = new AtomicInteger(0)
    val Leading: Topic = Topic(index.getAndIncrement(), "leading")
    val Fighting: Topic = Topic(index.getAndIncrement(), "fighting")
    val Machines: Topic = Topic(index.getAndIncrement(), "machines")
    val Party: Topic = Topic(index.getAndIncrement(), "party")

    val Unassigned: Map[Slot, Topic] = Slots.All.flatten.map(s => s -> Topic.unassigned(index.getAndIncrement(), s)).toMap
    val Concrete: Set[Topic] = Set(Leading, Fighting, Machines, Party)
    val All: Set[Topic] = Concrete ++ Unassigned.values
    implicit val TopicCount: Count[Topic] = util.Count[Topic](All.size)
  }

  object Slots {
    private val index = new AtomicInteger(0)
    lazy val Morning: Slot = Slot(index.getAndIncrement(), "morning", Persons.All, Some(Afternoon))
    lazy val Afternoon: Slot = Slot(index.getAndIncrement(), "afternoon", Persons.All, Some(Evening))
    lazy val Evening: Slot = Slot(index.getAndIncrement(), "evening", Persons.All, Some(Night))
    lazy val Night: Slot = Slot(index.getAndIncrement(), "night", Persons.All, None)

    val All: Seq[Seq[Slot]] = Seq(Seq(Morning, Afternoon, Evening, Night))
    implicit val SlotCount: Count[Slot] = util.Count[Slot](All.flatten.size)
  }

  object Problems {
    implicit val MinimalCounts: Counts = Counts.fromCounts(using Slots.SlotCount, Topics.TopicCount, Persons.PersonCount)
    val Minimal = new Problem(Slots.All, Topics.All, Topics.Unassigned.toArrayMap, Persons.All, Set.empty, Set.empty)
  }

}
