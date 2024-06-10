package fr.renoux.gaston.model

import cats.Monoid
import cats.implicits._
import fr.renoux.gaston.util.CanGroupToMap.ops.toCoupleOps
import fr.renoux.gaston.util.CollectionImplicits._
import fr.renoux.gaston.util.RandomImplicits._
import fr.renoux.gaston.util.{BitSet, Context, testOnly}

import scala.util.Random

/**
  * A schedule is an association of people, to topics, to slots.
  * What we're trying and testing and looking for a good one.
  * @param chainSeed The seed for the chain that has been used to produce this schedule. Allows to rerun it if debugging.
  * @param isAbysmal Flag to force this schedule to report the minimum possible score.
  */
final case class Schedule(
    chainSeed: Long,
    private val wrapped: Map[Slot, SlotSchedule],
    private val isAbysmal: Boolean = false
)(implicit
    val problem: Problem,
    ctx: Context
) {

  import Schedule._
  import problem.counts

  @inline private def updateWrapped(w: Map[Slot, SlotSchedule]): Schedule =
    copy(wrapped = w)

  @inline private def updateAllSlotSchedules(f: SlotSchedule => SlotSchedule): Schedule =
    updateWrapped(wrapped.map { case (slot, slotSchedule) => (slot, f(slotSchedule)) })

  @inline def updateSlotSchedule(slot: Slot)(f: SlotSchedule => SlotSchedule): Schedule =
    updateWrapped(wrapped.updated(slot, f(on(slot))))

  val scoreCalculator: ScoreCalculator = new ScoreCalculator(this)

  lazy val slotSchedules: Iterable[SlotSchedule] = wrapped.values
  lazy val slotSchedulesSet: Set[SlotSchedule] = slotSchedules.toSet
  lazy val slotSchedulesList: List[SlotSchedule] = slotSchedules.toList

  lazy val planning: Planning = wrapped.mapValuesStrict(_.topics)
  lazy val topicToSlot: Map[Topic, Slot] = planning.flatMap { case (s, ts) => ts.map(_ -> s) }
  lazy val scheduledTopics: Set[Topic] = slotSchedulesSet.flatMap(_.topics)
  lazy val scheduledTopicsBitSet: BitSet[Topic] = scheduledTopics.toBitSet
  lazy val unscheduledTopics: Set[Topic] = (problem.topicsSet -- scheduledTopics)

  lazy val personGroups: Iterable[Set[Person]] = personsByTopic.values // not a Set: we do not want to deduplicate identical groups!
  // lazy val maxPersonsOnSlot: Map[Slot, Int] = planning.mapValuesStrict(_.view.map(_.max).sum)
  // lazy val minPersonsOnSlot: Map[Slot, Int] = planning.mapValuesStrict(_.view.map(_.min).sum)
  lazy val personsByTopic: Map[Topic, Set[Person]] = slotSchedules.flatMap(_.personsByTopic).toMap

  /** Get the SlotSchedule for a specific Slot */
  def on(slot: Slot): SlotSchedule = wrapped.getOrElse(slot, SlotSchedule.empty(slot))

  def set(slotSchedule: SlotSchedule): Schedule = updateWrapped(wrapped + (slotSchedule.slot -> slotSchedule))

  lazy val score: Score = if (isAbysmal) Score.NegativeInfinity else scoreCalculator.globalScore

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


  /** This methods helps by not calculating the whole schedule's score until we're sure it's needed. However, it doesn't
    * work if there are any global preferences that depends on person assignment, as we still need to recalculate global
    * score in that case. */
  // TODO the whole method itself is a minor (5%) hot-spot
  // TODO can be improved by handling global preferences
  def deltaScoreIfSwapPerson(slot: Slot, tp1: (Topic, Person), tp2: (Topic, Person)): Option[Score] =
    if (problem.hasGlobalPreferencesWherePersonsMatter) None
    else if (score.isNegativeInfinity) None // Delta would be positive infinity, which isn't helpful. We need to recalculate to see if we're still NegInf.
    else Some {
      val existingUnweightedScoresByPerson = scoreCalculator.unweightedScoresByPerson
      val deltaUnweightedScoresByPerson = wrapped(slot).deltaScoreIfSwapPersons(tp1, tp2)
      val newUnweightedScoresByPerson = Monoid[Map[Person, Score]].combine(existingUnweightedScoresByPerson, deltaUnweightedScoresByPerson)
      scoreCalculator.personalScoreFrom(newUnweightedScoresByPerson) - scoreCalculator.personalScore
    }

  /** Move a person on some slot, from some topic to another one. */
  def movePerson(slot: Slot, source: Topic, destination: Topic, person: Person): Schedule =
    updateSlotSchedule(slot)(_.movePerson(source, destination, person))

  /** The schedule makes sense. No person on multiple topics at the same time. No topic on multiple slots. */
  lazy val isSound: Boolean = {
    lazy val noUbiquity = slotSchedules.forall(_.isSound)
    lazy val noDuplicates = scheduledTopics.sizeCompare(slotSchedulesList.flatMap(_.topicsList)) == 0
    noUbiquity && noDuplicates
  }

  /**
    * Partial Schedules are schedule where slots and topics are matched, but not all persons are assigned yet.
    * @return true if this respects all constraints applicable to partial schedules
    */
  lazy val isPartialSolution: Boolean = {
    lazy val allSlotsOk = slotSchedules.forall(_.isPartialSolution)
    lazy val forcedTopicsOk = problem.forcedTopics.forall(scheduledTopics.contains)

    /* TODO: All constraints: they should never be an error, as the navigator should not allow for such a situation.
        Kept temporarily to check if it's happening or not. Should have an option to disable, it might make the algorithm go faster. */
    lazy val constraintsOk = problem.globalLevelConstraints.forall { c => !c.isApplicableToPartialSchedule || c.isRespected(this) }

    lazy val followupTopicsOk = problem.topicsWithFollowups.isEmpty || {
      slotSchedules.forall { ss =>
        lazy val previousSlot = problem.slotsToPreviousSlot(ss.slot)
        ss.topics.forall { topic =>
          val okIfHasFollowup =
            topic.followup match {
              case None => true
              case Some(followup) => ss.slot.next.exists(nextSlot => on(nextSlot).topicsSet.contains(followup))
            }
          // TODO this might be a bit slow? But it's needed.
          lazy val okIfIsFollowup = !topic.isFollowup || on(previousSlot).topics.exists(_.followup.contains(topic))
          okIfHasFollowup && okIfIsFollowup
        }
      }
    }
    allSlotsOk && forcedTopicsOk && constraintsOk && followupTopicsOk
  }

  /** @return true if this respects all constraints */
  lazy val isSolution: Boolean = {
    isPartialSolution &&
      slotSchedules.forall(_.isSolution) &&
      problem.globalLevelConstraints.forall { c => c.isApplicableToPartialSchedule || c.isRespected(this) }
  }

  lazy val errors: Seq[String] = if (isSolution) Nil else {
    val slotErrors = slotSchedules.flatMap(_.errors).toSeq
    val missingForcedTopics = problem.forcedTopics.diff(scheduledTopics)
    val forcedTopicsError = if (missingForcedTopics.nonEmpty) Some(s"Missing forced topics ${missingForcedTopics.map(_.toLongString)}") else None
    val constraintsError = problem.globalLevelConstraints.flatMap { c =>
      if (!c.isRespected(this)) Some(s"Constraint ${c.toLongString} not respected") else None
    }.toSeq
    val followupTopicsErrors = slotSchedules.flatMap { ss =>
      ss.topics.flatMap { topic =>
        topic.followup.flatMap { followup =>
          if (ss.slot.next.exists(nextSlot => on(nextSlot).topicsSet.contains(followup))) None
          else Some(s"Bad followup topic for topic ${topic.toLongString} (on slot ${ss.slot.toLongString})")
        }
      }
    }

    slotErrors ++ forcedTopicsError ++ constraintsError ++ followupTopicsErrors
  }

  /** Produces a clear, multiline version of this schedule. */
  lazy val toFormattedString: String = {
    val builder = new StringBuilder(s"Schedule-chain seed: $chainSeed\n")
    slotSchedules.toSeq.sortBy(_.slot.name).foreach { ss => builder.append(ss.toFormattedString) }
    builder.append(unscheduledTopics.view.map(_.name).toSeq.sorted.mkString("Unscheduled topics: ", ", ", "\n"))
    builder.toString
  }

  /** Unassign all persons, except mandatory persons. If unassigned topics do not exist, does nothing. Used only in tests. */
  @testOnly def unassignAll: Schedule =
    if (problem.unassignedTopics.isEmpty) this
    else updateAllSlotSchedules(_.unassignAll)

  /** Merge with another schedule's content. Used only in tests. */
  @testOnly def ++(that: Schedule): Schedule = Schedule(
    this.chainSeed + that.chainSeed,
    wrapped.zipByKeys(that.wrapped).mapValuesStrict {
      case (Some(a), Some(b)) => a ++ b
      case (Some(a), None) => a
      case (None, Some(b)) => b
      case (None, None) => throw new IllegalStateException
    }
  )

  /** Note that equals does NOT take into account the seed or the isAbysmal flag */
  override def equals(o: Any): Boolean = o match {
    case that: Schedule => this.wrapped == that.wrapped
    case _ => false
  }

  override lazy val hashCode: Int = wrapped.hashCode

}

object Schedule {

  type Planning = Map[Slot, Iterable[Topic]]

  implicit object ScheduleIsOrdered extends scala.math.Ordering[Schedule] {
    override def compare(x: Schedule, y: Schedule): Int = x.score.compare(y.score)
  }

  /** Empty schedule for a problem */
  def empty(implicit problem: Problem, ctx: Context): Schedule = Schedule(chainSeed = 0, Map.empty)

  /** Same as `empty`, except the schedule is marked as having the worst score. */
  def abysmal(implicit problem: Problem, ctx: Context): Schedule = Schedule(chainSeed = 0, Map.empty, isAbysmal = true)

  /** Schedule where only the forced topics are placed (randomly). Forced topics contain their mandatory persons plus
    * the minimum number of persons to make them valid. Other persons are on the "unassigned" topics. */
  def startingUnassignedOrForced(chainSeed: Long)(implicit problem: Problem, ctx: Context, rand: Random): Schedule = {
    /* Everything complicated in here is for the forced-slots */
    val forcedTopicsCountPerSlot = Array.fill(problem.slotsSet.size)(0)
    val forcedTopicsBySlot: Map[Slot, Seq[Topic]] =
      problem.forcedTopicsMostToLeastConstrained.filterNot(_.isUnassigned) // unassigned topics are handled manually
        .map { topic =>
          val possibleSlots = topic.slots.getOrElse(problem.slotsSet).filterMinBy(s => forcedTopicsCountPerSlot(s.id))
          val slot = rand.pick(possibleSlots)
          forcedTopicsCountPerSlot(slot.id) += 1
          slot -> topic
        }.groupToMap

    val slotSchedules = problem.slotsSet.map { slot =>
      /* For each slot, starts with everyone unassigned */
      val allPersons = rand.shuffle(slot.personsPresent.toList)
      val slotScheduleUnassigned = SlotSchedule.everyoneUnassigned(slot)
      val unassignedTopic = slotScheduleUnassigned.topics.head
      /* Add forced topics and assign some persons on them */
      val slotScheduleWithForcedTopics = forcedTopicsBySlot.get(slot) match {
        case None => slotScheduleUnassigned
        case Some(forcedTopics) =>
          forcedTopics.foldLeft((slotScheduleUnassigned, allPersons)) { case ((slotSchedule, personsAvailable), topic) =>
            if (topic.mandatory.exists(!personsAvailable.contains(_))) {
              throw new IllegalStateException(s"Too many forced topics on the same slot ${slot.toShortString}, mandatory persons are conflicting")
            }
            val nonMandatoryPersonsToAdd = personsAvailable.filterNot(topic.mandatory).filterNot(topic.forbidden).take(topic.min - topic.mandatory.size)
            val allPersonsToAdd = topic.mandatory ++ nonMandatoryPersonsToAdd
            val newSS = allPersonsToAdd.foldLeft(slotSchedule.addTopic(topic)) { (ss, person) =>
              ss.movePerson(unassignedTopic, topic, person)
            }
            (newSS, personsAvailable.filterNot(allPersonsToAdd.contains))
          }._1
      }
      slot -> slotScheduleWithForcedTopics
    }

    Schedule(chainSeed, slotSchedules.toMap)
  }

  /** Commodity method for tests */
  @testOnly def from(entries: Seq[Record]*)(implicit problem: Problem, ctx: Context): Schedule =
    new Schedule(0, entries.flatten.groupBy(_.slot).map[Slot, SlotSchedule] {
      case (s, rs) => s -> SlotSchedule.from(s, rs)
    })
}
