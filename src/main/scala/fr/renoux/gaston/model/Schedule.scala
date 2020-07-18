package fr.renoux.gaston.model

import fr.renoux.gaston.util.CollectionImplicits._
import fr.renoux.gaston.util.{Context, testOnly}
import scalaz.Scalaz._

import scala.annotation.tailrec

/**
  * A schedule is an association of people, to topics, to slots.
  * What we're trying and testing and looking for a good one.
  */
case class Schedule(
    private val wrapped: Map[Slot, SlotSchedule]
)(implicit
    val problem: Problem,
    ctx: Context
) {

  import Schedule._

  @inline private def updateWrapped(w: Map[Slot, SlotSchedule]): Schedule =
    copy(wrapped = w)

  @inline private def updateSlotSchedule(slot: Slot)(f: SlotSchedule => SlotSchedule): Schedule =
    updateWrapped(wrapped.updated(slot, f(on(slot))))

  lazy val slotSchedules: Iterable[SlotSchedule] = wrapped.values
  lazy val slotSchedulesSet: Set[SlotSchedule] = slotSchedules.toSet
  lazy val slotSchedulesList: List[SlotSchedule] = slotSchedules.toList

  lazy val planning: Planning = wrapped.mapValuesStrict(_.topics)
  lazy val topicToSlot: Map[Topic, Slot] = planning.flatMap { case (s, ts) => ts.map(_ -> s) }
  lazy val scheduledTopics: Set[Topic] = slotSchedulesSet.flatMap(_.topics)
  lazy val scheduledRealTopics: Set[Topic] = scheduledTopics.filterNot(_.virtual)
  lazy val scheduledRemovableTopics: Set[Topic] = scheduledRealTopics.filterNot(_.forced)
  lazy val scheduledRemovableTopicsSeq: Seq[Topic] = scheduledRemovableTopics.toSeq
  lazy val unscheduledTopics: Set[Topic] = (problem.realTopics -- scheduledTopics)

  lazy val personGroups: Iterable[Set[Person]] = personsPerTopic.values // not a Set: we do not want to deduplicate identical groups!
  lazy val maxPersonsOnSlot: Map[Slot, Int] = planning.mapValuesStrict(_.view.map(_.max).sum)
  lazy val minPersonsOnSlot: Map[Slot, Int] = planning.mapValuesStrict(_.view.map(_.min).sum)
  lazy val personsPerTopic: Map[Topic, Set[Person]] = slotSchedules.flatMap(_.personsPerTopic).toMap

  /** Get the SlotSchedule for a specific Slot */
  def on(slot: Slot): SlotSchedule = wrapped.getOrElse(slot, SlotSchedule.empty(slot))

  def set(slotSchedule: SlotSchedule): Schedule = updateWrapped( wrapped + (slotSchedule.slot -> slotSchedule))

  lazy val score: Score = Scorer.score(this)

  /** Add a new record to this schedule. */
  def add(record: Record): Schedule = updateSlotSchedule(record.slot)(_.add(record))

  def addAll(slot: Slot, records: Set[Record]): Schedule = {
    if (records.isEmpty) this else updateSlotSchedule(slot)(_.addAll(records))
  }

  /** Clear all non-mandatory persons on the given slots. Returned schedule is partial, obviously. */
  def clearSlots(slots: Slot*): Schedule = updateWrapped {
    val updatedSlots = slots.map { s => s -> on(s).cleared }
    wrapped ++ updatedSlots
  }

  def addTopic(slot: Slot, topic: Topic): Schedule = updateSlotSchedule(slot)(_.addTopic(topic))

  def addTopics(slot: Slot, topics: Set[Topic]): Schedule = updateSlotSchedule(slot)(_.addTopics(topics))

  /** Swap two topics from two different slots. Mandatory persons are set on the new topics and no one else, so the
    * schedule is probably unsound and/or partial. */
  def swapTopic(st1: (Slot, Topic), st2: (Slot, Topic)): Schedule = updateWrapped {
    val (slot1, topic1) = st1
    val (slot2, topic2) = st2
    val modified1 = slot1 -> on(slot1).replaceTopic(topic1, topic2)
    val modified2 = slot2 -> on(slot2).replaceTopic(topic2, topic1)
    wrapped + modified1 + modified2
  }

  /** Swap two groups of topics from two different slots. Mandatory persons are set on the new topics and no one else, so the
    * schedule is probably unsound and/or partial. */
  def swapTopics(st1: (Slot, Set[Topic]), st2: (Slot, Set[Topic])): Schedule = updateWrapped {
    val (slot1, topics1) = st1
    val (slot2, topics2) = st2
    val modified1 = slot1 -> on(slot1).replaceTopics(topics1, topics2)
    val modified2 = slot2 -> on(slot2).replaceTopics(topics2, topics1)
    wrapped + modified1 + modified2
  }

  /** Replace an existing topic by a new one (typically unscheduled, on a slot). Mandatory persons are set on the new
    * topic and no one else, so the schedule is probably unsound and/or partial. */
  def replaceTopic(slot: Slot, oldTopic: Topic, newTopic: Topic): Schedule =
    updateSlotSchedule(slot)(_.replaceTopic(oldTopic, newTopic))

  def replaceTopics(slot: Slot, oldTopics: Set[Topic], newTopics: Set[Topic]): Schedule =
    updateSlotSchedule(slot)(_.replaceTopics(oldTopics, newTopics))

  def removeTopic(slot: Slot, topic: Topic): Schedule = updateSlotSchedule(slot)(_.removeTopic(topic))

  def removeTopics(slot: Slot, topics: Set[Topic]): Schedule = updateSlotSchedule(slot)(_.removeTopics(topics))


  /** Adds a person to some topic already on schedule. If the topic is not on schedule on that slot, returns the same schedule. */
  def addPersonToExistingTopic(slot: Slot, topic: Topic, person: Person): Schedule =
    updateSlotSchedule(slot)(_.addPersonToExistingTopic(topic, person))

  /** Swap two persons on a slot. Persons are in couple with there current topic. */
  def swapPersons(slot: Slot, tp1: (Topic, Person), tp2: (Topic, Person)): Schedule =
    updateSlotSchedule(slot)(_.swapPersons(tp1, tp2))

  /** Move a person on some slot, from some topic to another one. */
  def movePerson(slot: Slot, source: Topic, destination: Topic, person: Person): Schedule =
    updateSlotSchedule(slot)(_.movePerson(source, destination, person))

  /** The schedule makes sense. No person on multiple topics at the same time. No topic on multiple slots. */
  lazy val isSound: Boolean = {
    lazy val noUbiquity = slotSchedules.forall(_.isSound)
    lazy val noDuplicates = scheduledTopics.sizeCompare(slotSchedulesList.flatMap(_.topicsList)) == 0
    noUbiquity && noDuplicates
  }

  /** Score for each person, regardless of its weight. All personal scores are slot-level, so the whole computation is done per slot. */
  lazy val unweightedScoresByPerson: Map[Person, Score] = slotSchedulesList.map(_.unweightedScoresByPerson).suml

  /** There are some impersonal global-level preferences, so we have to calculate them in addition to the slot computation. */
  lazy val impersonalScore: Score =
    slotSchedulesList.map(_.impersonalScore).suml + preferencesScoreRec(problem.impersonalGlobalLevelPreferencesList)

  @tailrec
  private def preferencesScoreRec(prefs: List[Preference.GlobalLevel], sum: Double = 0): Score = prefs match {
    case Nil => Score(sum)
    case p :: ps =>
      val s = p.scoreSchedule(this)
      if (s.value == Double.NegativeInfinity) s else preferencesScoreRec(ps, sum + s.value)
  }

  /**
    * Partial Schedules are schedule where slots and topics are matched, but not all persons are assigned yet.
    * @return true if this respects all constraints applicable to partial schedules
    */
  lazy val isPartialSolution: Boolean = {
    lazy val allSlotsOk = slotSchedules.forall(_.isPartialSolution)
    lazy val forcedTopicsOk = problem.forcedTopics.forall(scheduledTopics.contains)
    lazy val constraintsOk = problem.globalLevelConstraints.forall { c => !c.isApplicableToPartialSchedule || c.isRespected(this) }

    allSlotsOk && forcedTopicsOk && forcedTopicsOk && constraintsOk
  }

  /** @return true if this respects all constraints */
  lazy val isSolution: Boolean =
    isPartialSolution &&
      slotSchedules.forall(_.isSolution)
  problem.globalLevelConstraints.forall { c => c.isApplicableToPartialSchedule || c.isRespected(this) }

  /** Produces a clear, multiline version of this schedule. */
  lazy val toFormattedString: String = {
    val builder = new StringBuilder("Schedule:\n")
    slotSchedules.foreach { ss => builder.append(ss.toFormattedString) }
    builder.append(unscheduledTopics.map(_.name).mkString("Unscheduled topics: ", ", ", "\n"))
    builder.toString
  }

  /** Merge with another schedule's content. Used only in tests. */
  @testOnly def ++(that: Schedule): Schedule = Schedule(
    wrapped.zipByKeys(that.wrapped).mapValuesStrict {
      case (Some(a), Some(b)) => a ++ b
      case (Some(a), None) => a
      case (None, Some(b)) => b
      case (None, None) => throw new IllegalStateException
    }
  )

}

object Schedule {

  type Planning = Map[Slot, Set[Topic]]

  implicit object ScheduleIsOrdered extends scala.math.Ordering[Schedule] {
    override def compare(x: Schedule, y: Schedule): Int = x.score.compare(y.score)
  }

  /** Empty schedule for a problem */
  def empty(implicit problem: Problem, ctx: Context): Schedule = Schedule(Map.empty)

  /** Schedule where everyone is on an "unassigned" topic */
  def everyoneUnassigned(implicit problem: Problem, ctx: Context): Schedule = Schedule(
    problem.slots.map { s => s -> SlotSchedule.everyoneUnassigned(s) }.toMap
  )

  /** Commodity method */
  @testOnly def from(entries: Seq[Record]*)(implicit problem: Problem, ctx: Context): Schedule =
    new Schedule(entries.flatten.groupBy(_.slot).map[Slot, SlotSchedule] {
      case (s, rs) => s -> SlotSchedule.from(s, rs)
    })
}
