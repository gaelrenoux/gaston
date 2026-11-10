package fr.renoux.gaston.engine.assignment

import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.{ArrayMap, Count}
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

/** Compares SlotMatcher with a brute-force search on many small random slots. */
class SlotMatcherSpec extends AnyFlatSpec with Matchers {

  private val InstancesCount = 3000

  /** A random slot: some topics with random bounds and forbidden persons, some persons already placed, and some new
   * persons to place. */
  private final case class Instance(records: Array[Record], newPersons: Array[Person])(using val problem: Problem)

  private def randomInstance(using random: Random): Instance = {
    val topicsCount = 1 + random.nextInt(3)
    val personsCount = random.nextInt(9)
    val persons = (0 until personsCount).map { i => Person(i, s"p$i") }
    val slot = Slot(0, "slot", persons.toSet, None)
    val topics = (0 until topicsCount).map { i =>
      val min = random.nextInt(4)
      val max = min + random.nextInt(4)
      val forbidden = persons.filter { _ => random.nextInt(4) == 0 }.toSet
      Topic(i, s"t$i", forbidden = forbidden, min = min, max = max)
    }

    given Counts = Counts.fromCounts(using Count[Slot](1), Count[Topic](topicsCount), Count[Person](personsCount))

    given problem: Problem = new Problem(Seq(Seq(slot)), topics.toSet, ArrayMap.empty, persons.toSet, Set.empty, Set.empty)

    val (alreadyPlaced, newPersons) = random.shuffle(persons).splitAt(random.nextInt(personsCount + 1))
    val placedByTopic = alreadyPlaced.flatMap { p =>
      val allowed = topics.filterNot(_.forbidden.contains(p))
      if (allowed.isEmpty) None else Some(allowed(random.nextInt(allowed.size)) -> p)
    }.groupMap(_._1)(_._2)
    val records = topics.map { t => Record(slot, t, placedByTopic.getOrElse(t, Nil).toSet) }
    Instance(records.toArray, newPersons.toArray)
  }

  /** True if some valid assignment exists, trying all possible ones. */
  private def hasBruteForceSolution(instance: Instance): Boolean = {
    val records = instance.records
    val persons = instance.newPersons

    def isValid(recordOfPerson: Array[Int]): Boolean = {
      val isAllowed = persons.indices.forall { p => !records(recordOfPerson(p)).topic.forbidden.contains(persons(p)) }
      isAllowed && records.indices.forall { r =>
        val count = records(r).countPersons + recordOfPerson.count(_ == r)
        count >= records(r).topic.min && count <= records(r).topic.max
      }
    }

    def search(p: Int, recordOfPerson: Array[Int]): Boolean =
      if (p == persons.length) isValid(recordOfPerson)
      else records.indices.exists { r =>
        recordOfPerson(p) = r
        search(p + 1, recordOfPerson)
      }

    search(0, new Array[Int](persons.length))
  }

  /** Checks that the matcher's result places each new person exactly once, on an allowed topic, and that all records
   * end up within their topic's bounds. */
  private def checkResult(instance: Instance, result: Set[Record]): Assertion = {
    val records = instance.records
    val resultByTopic = result.map { r => r.topic -> r }.toMap
    val finalRecords = records.map { r => resultByTopic.getOrElse(r.topic, r) }

    finalRecords.foreach { r =>
      val original = records.find(_.topic == r.topic).get
      original.persons.subsetOf(r.persons) should be(true)
      (r.persons -- original.persons).exists(r.topic.forbidden.contains) should be(false)
      r.countPersons should be >= r.topic.min
      r.countPersons should be <= r.topic.max
    }

    val added = finalRecords.toSeq.flatMap { r => (r.persons -- records.find(_.topic == r.topic).get.persons).toSeq }
    added.sortBy(_.id) should be(instance.newPersons.toSeq.sortBy(_.id))
  }

  "SlotMatcher" should "find an assignment exactly when one exists" in {
    given random: Random = new Random(0)

    var feasibleCount = 0
    (0 until InstancesCount).foreach { _ =>
      val instance = randomInstance
      given Problem = instance.problem

      val result = SlotMatcher.assign(instance.records, instance.newPersons)
      val isFeasible = hasBruteForceSolution(instance)
      withClue(instance.records.map(_.topic.toLongString).mkString("", "\n", "\n") + instance.records.mkString("\n")) {
        result.isDefined should be(isFeasible)
      }
      result.foreach { r =>
        feasibleCount += 1
        checkResult(instance, r)
      }
    }

    // Makes sure the random instances cover both cases significantly
    feasibleCount should be > InstancesCount / 5
    feasibleCount should be < InstancesCount * 4 / 5
  }

}
