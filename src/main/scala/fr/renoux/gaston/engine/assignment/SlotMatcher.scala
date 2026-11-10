package fr.renoux.gaston.engine.assignment

import fr.renoux.gaston.model.{Person, Problem, Record}

import java.util
import scala.util.Random

/** Assigns persons to the records of a single slot. Each person must end up on exactly one record, cannot be put on a
 * topic where they are forbidden, and each record must end up between its topic's min and max. Persons already on a
 * record stay there.
 *
 * This is a bipartite b-matching, solved with augmenting paths (Kuhn's algorithm, with capacities on records), in two
 * phases:
 *   - Phase 1 caps each record to the number of persons it still needs to reach its topic's min.
 *   - Phase 2 raises each cap to the number of persons it can still take before reaching its topic's max.
 *
 * An augmenting path only shifts persons between records, and ends on a record with spare capacity: the load of a
 * record never decreases. The minimums reached in phase 1 therefore still hold at the end of phase 2. Both phases find
 * a maximum matching, so the result is None only if no valid assignment exists.
 *
 * With P persons and R records, the complexity is O(P² × R).
 *
 * This class is mutable for performance. An instance only lives during a single call to `SlotMatcher.assign`.
 */
final class SlotMatcher private(records: Array[Record], persons: Array[Person])(using problem: Problem, random: Random) {

  private val recordsCount = records.length
  private val personsCount = persons.length

  /** For each person (by index), the indices of the records they can be put on, in random order. */
  private val allowedRecords: Array[Array[Int]] = persons.map { person =>
    random.shuffle((0 until recordsCount).filterNot { r => records(r).topic.forbidden.contains(person) }).toArray
  }

  /** For each record, how many persons it still needs to reach its topic's min. */
  private val minNeeded: Array[Int] = records.map { record => math.max(0, record.topic.min - record.countPersons) }

  /** For each record, how many persons it can still take before reaching its topic's max. */
  private val maxOpen: Array[Int] = records.map { record => record.topic.max - record.countPersons }

  /** For each record, the current maximum number of new persons. */
  private val capacity: Array[Int] = new Array[Int](recordsCount)

  /** For each record, the number of new persons currently on it. */
  private val load: Array[Int] = new Array[Int](recordsCount)

  /** For each person, the index of their record, or -1 if they are not assigned yet. */
  private val recordOfPerson: Array[Int] = Array.fill(personsCount)(-1)

  /** Records already explored by the current augmenting-path search. */
  private val isVisited: Array[Boolean] = new Array[Boolean](recordsCount)

  private def run(): Option[Set[Record]] = {
    val minNeededTotal = minNeeded.sum
    val maxOpenTotal = maxOpen.foldLeft(0L)(_ + _) // Long because the unassigned topic's max is very high
    val isObviouslyImpossible = maxOpen.exists(_ < 0) || minNeededTotal > personsCount || maxOpenTotal < personsCount

    if (isObviouslyImpossible) None
    else {
      Array.copy(minNeeded, 0, capacity, 0, recordsCount)
      if (augmentAll() < minNeededTotal) None
      else {
        Array.copy(maxOpen, 0, capacity, 0, recordsCount)
        if (augmentAll() < personsCount) None
        else Some(filledRecords)
      }
    }
  }

  /** Tries to assign every person not assigned yet. Returns the total count of assigned persons. */
  private def augmentAll(): Int = {
    var assignedCount = 0
    var p = 0
    while (p < personsCount) {
      if (recordOfPerson(p) >= 0) {
        assignedCount += 1
      } else {
        util.Arrays.fill(isVisited, false)
        if (tryAssign(p)) {
          assignedCount += 1
        }
      }
      p += 1
    }
    assignedCount
  }

  /** Looks for an augmenting path starting from that person: a record with spare capacity, or a full record where one
   * of the persons can itself be moved somewhere else. Returns true if that person was assigned. */
  private def tryAssign(p: Int): Boolean = {
    val candidates = allowedRecords(p)
    var isAssigned = false
    var i = 0
    while (!isAssigned && i < candidates.length) {
      val r = candidates(i)
      if (!isVisited(r)) {
        isVisited(r) = true
        if (load(r) < capacity(r) || relocateSomeoneFrom(r)) {
          moveTo(p, r)
          isAssigned = true
        }
      }
      i += 1
    }
    isAssigned
  }

  /** Tries to move one of the new persons on that record to another record. Returns true if it worked, in which case
   * that record has one free place. */
  private def relocateSomeoneFrom(r: Int): Boolean = {
    var isRelocated = false
    var q = 0
    while (!isRelocated && q < personsCount) {
      if (recordOfPerson(q) == r) {
        isRelocated = tryAssign(q)
      }
      q += 1
    }
    isRelocated
  }

  private def moveTo(p: Int, r: Int): Unit = {
    val previous = recordOfPerson(p)
    if (previous >= 0) {
      load(previous) -= 1
    }
    recordOfPerson(p) = r
    load(r) += 1
  }

  /** Records that received new persons, with those persons added. */
  private def filledRecords: Set[Record] =
    persons.indices.groupMap(recordOfPerson(_))(persons(_)).map { case (r, newPersons) =>
      records(r).copy(persons = records(r).persons ++ newPersons)
    }.toSet

}

object SlotMatcher {

  /** Assigns all those persons to those records (see class doc). Returns the records that received new persons, or None
   * if no valid assignment exists. Randomness comes from the order of both arrays, and from the order in which each
   * person considers the records. */
  def assign(records: Array[Record], persons: Array[Person])(using problem: Problem, random: Random): Option[Set[Record]] =
    new SlotMatcher(records, persons).run()
}
