package fr.renoux.gaston.model

import fr.renoux.gaston.util.{BitSet, testOnly}

import scala.annotation.tailrec


/** A Record is a triplet of slot, topic and assigned persons */
final case class Record(slot: Slot, topic: Topic, persons: Set[Person])(implicit val problem: Problem) extends Ordered[Record] {
  import problem.counts

  lazy val personsList: List[Person] = persons.toList
  lazy val countPersons: Int = persons.size

  lazy val personsBitSet: BitSet[Person] = persons.toBitSet

  /* No need to compare persons, on a given schedule there is not two records with the same slot and topic */
  override def compare(that: Record): Int = {
    val compareSlots = slot.name.compareTo(that.slot.name)
    if (compareSlots != 0) compareSlots
    else topic.name.compareTo(that.topic.name)
  }

  lazy val optionalPersons: Set[Person] = persons -- topic.mandatory

  lazy val canRemovePersons: Boolean = countPersons > topic.min && optionalPersons.nonEmpty
  lazy val canAddPersons: Boolean = countPersons < topic.max

  /** Clear all non-mandatory persons. */
  lazy val cleared: Record = copy(persons = topic.mandatory)

  /** Adds a person to the record. */
  def addPerson(person: Person): Record = copy(persons = persons + person)

  /** Removes a person from the record. */
  def removePerson(person: Person): Record = copy(persons = persons - person)

  /** Replace a person by another on the record. */
  def replacePerson(oldP: Person, newP: Person): Record = copy(persons = persons -oldP + newP)

  /** Score for each person, regardless of its weight. */
  lazy val unweightedScoresByPerson: Map[Person, Score] =
    persons.view.map { person =>
      val prefs = problem.personalPreferencesListByPerson(person)
      val score = if (prefs.isEmpty) Score.Zero else prefs.view.map(_.scoreRecord(this)).sum
      person -> score
    }.toMap

  lazy val impersonalScore: Score = preferencesScoreRec(problem.impersonalRecordLevelPreferencesList)

  @tailrec
  private def preferencesScoreRec(prefs: List[Preference.RecordLevel], sum: Double = 0): Score = prefs match {
    case Nil => Score(sum)
    case p :: ps =>
      val s = p.scoreRecord(this)
      if (s.value == Double.NegativeInfinity) s else preferencesScoreRec(ps, sum + s.value)
  }

  /**
    * Partial Schedules are schedule where topics are matched, but not all persons are assigned yet.
    * @return true if this respects all constraints applicable to partial schedules
    */
  lazy val isPartialSolution: Boolean = {
    topic.max >= countPersons && // topic.min <= pCount &&
      !topic.forbidden.exists(persons.contains) && topic.mandatory.forall(persons.contains) &&
      topic.slots.forall(_.contains(slot)) &&
      persons.forall(slot.personsPresent.contains)
  }

  /** @return true if this respects all constraints */
  lazy val isSolution: Boolean =
    isPartialSolution && topic.min <= countPersons


  /** Merge with another slot schedule's content. Used only in tests. */
  @testOnly def ++(that: Record): Record = {
    if (slot != that.slot || topic != that.topic) throw new IllegalArgumentException(s"$this ++ $that")
    copy(persons = persons ++ that.persons)
  }

  /** Produces a clear, single-line version of this record, with no indentation. */
  lazy val toFormattedString: String =
    s"${topic.name}: ${persons.map(_.name).mkString(", ")}"

}

object Record {
  def fromTuple(tuple: (Slot, Topic, Set[Person]))(implicit problem: Problem): Record = Record(tuple._1, tuple._2, tuple._3)

  def fromTuple2(tuple: ((Slot, Topic), Set[Person]))(implicit problem: Problem): Record = Record(tuple._1._1, tuple._1._2, tuple._2)

  def apply(slot: Slot, topic: Topic, persons: Person*)(implicit problem: Problem): Record = apply(slot, topic, persons.toSet)
}
