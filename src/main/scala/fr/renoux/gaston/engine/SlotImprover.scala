package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.Context.chrono
import fr.renoux.gaston.model._

import scala.annotation.tailrec
import scala.util.Random

/** Improves a whole schedule by moving slots around. */
class SlotImprover(
    stopAtScore: Double = Double.MaxValue,
    maxImprovementRounds: Int = 1000
)(implicit private val problem: Problem, ctx: Context) {

  import SlotImprover._

  private val log = Logger[SlotImprover]

  private val filler: PartialScheduleFiller = new PartialScheduleFiller
  private val personImprover: PersonPlacementImprover = new PersonPlacementImprover

  /** Improve the schedule by trying swap after swap of topics. */
  @tailrec
  final def improveToMax(schedule: Schedule, previousMove: Move = Move.Nothing, maxRound: Int = maxImprovementRounds)
    (implicit rand: Random): Schedule =
    if (maxRound == 0) {
      log.warn(s"Improver could not do its best (score is ${schedule.score})")
      schedule
    } else if (schedule.score.value >= stopAtScore) {
      log.info(s"Improver stopped because expected score has been reached (score is ${schedule.score})")
      schedule
    } else improveOnce(schedule, previousMove) match {
      case None =>
        log.debug(s"[$maxRound] Best schedule I can get (score is ${schedule.score})")
        schedule // can't make it any better
      case Some((swappedSchedule, move)) =>
        log.debug(s"[$maxRound] Move: $move (new score is ${swappedSchedule.score}\n${swappedSchedule.toFormattedString}")
        improveToMax(swappedSchedule, move, maxRound - 1)
    }


  /** Take an already improved schedule, and return the first better schedule it can find by swapping topics. */
  // scalastyle:off cyclomatic.complexity method.length
  def improveOnce(schedule: Schedule, previousMove: Move)
    (implicit rand: Random): Option[(Schedule, Move)] = chrono("SlotImprover > improveOnce") {


    lazy val allAdds = chrono("SlotImprover > improveOnce > allAdds") {
      /* Add an unscheduled topic */
      for {
        slot <- shuffled(problem.slotsList).view

        /* Filter out impossible adds because of incompatibility */
        slotSchedule = schedule.on(slot)
        topic <- shuffled(schedule.unscheduledTopics -- slotSchedule.incompatibleTopics).view
        topicsToAdd = linkedTopics(topic)
        if slotSchedule.maxTopicsLeft >= topicsToAdd.size

        move = Move.Add(slot, topicsToAdd)
        if !move.reverts(previousMove)

        personsMandatoryOnTopic = topicsToAdd.flatMap(_.mandatory)

        /* Filter out impossible adds because mandatory persons are already taken */
        personsAlreadyMandatoryOnSlot = slotSchedule.mandatory
        if !personsMandatoryOnTopic.exists(personsAlreadyMandatoryOnSlot)

        /* Filter out impossible adds because mandatory persons are missing */
        if personsMandatoryOnTopic.forall(slot.personsPresent)

        /* Generate the swap */
        records = topicsToAdd.map { t => Record(slot, t, t.mandatory) }
        partial = schedule.clearSlots(slot).add(records)
        if partial.isPartialSolution

        /* Filter out impossible adds because of unreachable minimum */
        if partial.minPersonsOnSlot(slot) <= slot.personsPresentCount
      } yield (partial, move)
    }


    lazy val allSwaps = chrono("SlotImprover > improveOnce > allSwaps") {
      /* Swap topics between two scheduled topics */
      for {
        (s1, s2) <- shuffled(problem.slotCouplesSeq).view
        slotSchedule1 = schedule.on(s1)
        slotSchedule2 = schedule.on(s2)

        t1 <- shuffled(slotSchedule1.topics -- slotSchedule2.permanentlyIncompatibleTopics).view
        t2 <- shuffled(slotSchedule2.topics -- slotSchedule1.permanentlyIncompatibleTopics).view
        topics1 = linkedTopics(t1)
        topics2 = linkedTopics(t2)
        if slotSchedule1.maxTopicsLeft >= topics2.size - topics1.size
        if slotSchedule2.maxTopicsLeft >= topics1.size - topics2.size

        move = Move.Swap(topics1, topics2)
        if !move.reverts(previousMove)

        personsMandatoryOnT1 = topics1.flatMap(_.mandatory)
        personsMandatoryOnT2 = topics2.flatMap(_.mandatory)

        /* Filter out impossible adds because mandatory persons are already taken */
        personsAlreadyMandatoryOnS1 = slotSchedule1.mandatory -- personsMandatoryOnT1 // taking this topic off, they're not mandatory any more on the slot
        personsAlreadyMandatoryOnS2 = slotSchedule2.mandatory -- personsMandatoryOnT2 // taking this topic off, they're not mandatory any more on the slot
        if !personsMandatoryOnT1.exists(personsAlreadyMandatoryOnS2) // check mandatories of T1 are not already blocked on S2
        if !personsMandatoryOnT2.exists(personsAlreadyMandatoryOnS1) // check mandatories of T2 are not already blocked on

        /* Filter out impossible adds because mandatory persons are missing */
        if personsMandatoryOnT1.forall(s2.personsPresent)
        if personsMandatoryOnT2.forall(s1.personsPresent)

        /* Generate the swap */
        partial = schedule.clearSlots(s1, s2).swapTopics(s1 -> topics1, s2 -> topics2)
        if partial.isPartialSolution // TODO this rechecks everything, we shouldn't check what has been checked individually before
      } yield (partial, move)
    }


    lazy val allExternalSwaps = chrono("SlotImprover > improveOnce > allExternalSwaps") {
      /* Swap topics between unscheduled and scheduled */
      for {
        oldT <- shuffled(schedule.scheduledTopicsSeq).view
        slot = schedule.topicToSlot(oldT)
        slotSchedule = schedule.on(slot)
        newT <- shuffled(schedule.unscheduledTopics -- slotSchedule.permanentlyIncompatibleTopics).view
        oldTs = linkedTopics(oldT)
        newTs = linkedTopics(newT)
        if slotSchedule.maxTopicsLeft >= newTs.size - oldTs.size

        move = Move.Swap(oldTs, newTs)
        if !move.reverts(previousMove)

        personsMandatoryOnOldTs = oldTs.flatMap(_.mandatory)
        personsMandatoryOnNewTs = newTs.flatMap(_.mandatory)

        /* Filter out impossible swaps because of mandatory persons */
        personsAlreadyMandatoryOnSlot = slotSchedule.mandatory -- personsMandatoryOnOldTs // taking this topic off, they're not mandatory any more on the slot
        if !personsMandatoryOnNewTs.exists(personsAlreadyMandatoryOnSlot)

        /* Filter out impossible adds because mandatory persons are missing */
        if personsMandatoryOnNewTs.forall(slot.personsPresent)

        /* Generate the swap */
        partial = schedule.clearSlots(slot).replaceTopics(slot, oldTs, newTs)
        if partial.isPartialSolution
      } yield (partial, move)
    }


    lazy val allRemovals = chrono("SlotImprover > improveOnce > allRemovals") {
      /* Remove a scheduled topic */
      for {
        slot <- shuffled(problem.slotsList).view
        slotSchedule = schedule.on(slot)
        topic <- shuffled(slotSchedule.topicsList).view
        topicsToRemove = linkedTopics(topic)

        move = Move.Remove(slot, topicsToRemove)
        if !move.reverts(previousMove)

        /* Generate the swap */
        partial = schedule.removeTopics(topicsToRemove)
        if partial.isPartialSolution

        /* Filter out impossible adds because of maximum too low */
        if partial.maxPersonsOnSlot.getOrElse(slot, 0) >= slot.personsPresentCount
      } yield (partial, move)
    }

    val improvedSchedules = chrono("SlotImprover > improveOnce > improvedSchedules") {
      for {
        (partial, move) <- allAdds ++ allSwaps ++ allExternalSwaps ++ allRemovals
        _ = log.debug(s"Trying that move: $move")
        unimproved <- filler.fill(partial)(rand)
        improved = improve(unimproved)
        if improved.score > schedule.score
      } yield (improved, move)
    }

    chrono("SlotImprover > improveOnce > computation") {
      improvedSchedules.headOption
    }
  }

  // scalastyle:on cyclomatic.complexity method.length

  /** Improve the current schedule moving persons only. */
  private def improve(scoredSchedule: Schedule)(implicit rand: Random): Schedule = personImprover.improve(scoredSchedule)

  private def shuffled[A](set: Set[A]): Seq[A] = shuffled(set.toSeq)

  private def shuffled[A](seq: Seq[A]): Seq[A] = Random.shuffle(seq)

  private def linkedTopics(topic: Topic): Set[Topic] = problem.simultaneousTopicPerTopic(topic) + topic

}

object SlotImprover {


  /** Moves on the Schedule */
  trait Move {
    def reverts(m: Move): Boolean
  }

  object Move {

    case object Nothing extends Move {
      override def reverts(m: Move): Boolean = false
    }

    case class Swap(a: Set[Topic], b: Set[Topic]) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Swap(a1, b1) if (a == a1 && b == b1) || (a == b1 && b == a1) => true
        case _ => false
      }
    }

    case class Add(s: Slot, t: Set[Topic]) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Remove(s1, t1) if s == s1 && t == t1 => true
        case _ => false
      }
    }

    case class Remove(s: Slot, t: Set[Topic]) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Add(s1, t1) if s == s1 && t == t1 => true
        case _ => false
      }
    }

  }

}
