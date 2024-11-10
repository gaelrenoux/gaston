package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.PlanningSpaceNavigator.Move
import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.immutable

import scala.collection.View
import scala.util.Random


/** Tools to explore the space solution for plannings (ie, not doing the assignment). Schedules returned are always
 * unfilled (they just contain the planning), and valid (all constraints are matched, and no one is mandatory on two
 * topics at the same time). */
@immutable
final class PlanningSpaceNavigator(using private val problem: Problem) {

  private val log: Logger = Logger[PlanningSpaceNavigator]

  /** Return a LazyList of neighbouring unfilled schedules to the initial one. */
  def neighbours(schedule: Schedule)(using rand: Random): LazyList[(Schedule, Move)] = {

    lazy val allAdds = possibleAdds(schedule)
    lazy val allExternalSwaps = possibleExtSwaps(schedule)
    lazy val allSwaps = possibleSwaps(schedule)
    lazy val allRemovals = possibleRemovals(schedule)

    LazyList.from(allAdds ++ allExternalSwaps ++ allSwaps ++ allRemovals)
  }

  /** Add an unscheduled topic */
  private def possibleAdds(schedule: Schedule)(using rand: Random): View[(Schedule, Move)] = for {
    slot <- shuffled(problem.slotsList).view
    slotSchedule = schedule.on(slot)
    _ = log.debug(s"Checking for possible Additions on slot ${slot.name}")

    /* Filter out impossible topics because of incompatibility */
    topic <- shuffled(schedule.unscheduledTopics -- slotSchedule.incompatibleTopics).view

    /* Filter out impossible topics because followup topic can't be added. Also, can't move followup topics directly. */
    if (topic.followup.isEmpty || slot.hasNext) && !topic.isFollowup

    /* Handle simultaneous topics */
    topicsToAdd = simultaneousTopics(topic)

    /* Check we can add all those topics */
    if isAddPossible(slotSchedule, topicsToAdd)

    /* Get followup topics on next slot and check they can be added if needed */
    followupTopicsToAdd = topicsToAdd.flatMap(_.followup)
    if followupTopicsToAdd.isEmpty || slot.hasNext

    /* Create a new unfilled schedule with the followup topics added if possible */
    unfilledNextSlotModified <-
      if (followupTopicsToAdd.isEmpty) Some(schedule)
      else swapFollowupTopics(schedule, slot.next.get, followupTopicsToAdd, Set.empty)

    /* Create a new unfilled schedule with the added topics */
    unfilled = unfilledNextSlotModified.clearSlots(slot).addTopics(slot, topicsToAdd)

  } yield (unfilled, Move.Add(slot, topicsToAdd))

  private def isAddPossible(slotSchedule: SlotSchedule, topicsToAdd: Set[Topic]): Boolean = {
    lazy val slotIsNotFull = slotSchedule.maxTopicsLeft >= topicsToAdd.size
    lazy val mandatories = topicsToAdd.flatMap(_.mandatory)
    lazy val mandatoriesAreNotTaken = !mandatories.exists(slotSchedule.mandatory)
    lazy val mandatoriesArePresent = mandatories.forall(slotSchedule.slot.personsPresent)
    lazy val minPersonsRequired = slotSchedule.topics.foldLeft(0)(_ + _.min) + topicsToAdd.foldLeft(0)(_ + _.min)
    lazy val enoughPersonsArePresent = slotSchedule.slot.personsPresentCount >= minPersonsRequired
    slotIsNotFull && mandatoriesAreNotTaken && mandatoriesArePresent && enoughPersonsArePresent
  }

  /** Swap two scheduled topics */
  private def possibleSwaps(schedule: Schedule)(using rand: Random): View[(Schedule, Move)] = for {
    (slot1, slot2) <- shuffled(problem.slotCouplesList).view
    slotSchedule1 = schedule.on(slot1)
    slotSchedule2 = schedule.on(slot2)
    _ = log.debug(s"Checking for possible Swaps between slots ${slot1.name} and ${slot2.name}")

    /* Filter out impossible topics because of incompatibility */
    t1 <- shuffled(slotSchedule1.movableTopicsSet -- slotSchedule2.permanentlyIncompatibleTopics).view
    t2 <- shuffled(slotSchedule2.movableTopicsSet -- slotSchedule1.permanentlyIncompatibleTopics).view

    /* Filter out impossible topics because followup topic can't be added. Also, can't move followup topics directly. */
    if (t1.followup.isEmpty || slot2.hasNext) && (t2.followup.isEmpty || slot1.hasNext) && !t1.isFollowup && !t2.isFollowup

    topics1 = simultaneousTopics(t1)
    topics2 = simultaneousTopics(t2)

    /* Check we can indeed swap these topics */
    if isSwapPossible(slotSchedule1, slotSchedule2, topics1, topics2)

    /* Get followup topics on next slot and check they can be added if needed */
    followupTopics1 = topics1.flatMap(_.followup)
    followupTopics2 = topics2.flatMap(_.followup)
    if (followupTopics1.isEmpty || slot2.hasNext) && (followupTopics2.isEmpty || slot1.hasNext)

    /* Create a new unfilled schedule with the followup topics swapped if possible */
    unfilledNextSlotModified <-
      if (followupTopics1.isEmpty && followupTopics2.isEmpty) Some(schedule)
      else {
        swapFollowupTopics(schedule, slot2.next.get, followupTopics1, followupTopics2)
          .flatMap(swapFollowupTopics(_, slot1.next.get, followupTopics2, followupTopics1))
      }

    /* Generate the swap */
    unfilled = unfilledNextSlotModified.clearSlots(slot1, slot2).swapTopics(slot1 -> topics1, slot2 -> topics2)

  } yield (unfilled, Move.Swap(topics1, topics2, isExt = false))

  private def isSwapPossible(slotSchedule1: SlotSchedule, slotSchedule2: SlotSchedule, topics1: Set[Topic], topics2: Set[Topic]): Boolean = {
    lazy val slot1IsNotFull = slotSchedule1.maxTopicsLeft >= topics2.size - topics1.size
    lazy val slot2IsNotFull = slotSchedule2.maxTopicsLeft >= topics1.size - topics2.size
    lazy val mandatoriesT1 = topics1.flatMap(_.mandatory)
    lazy val mandatoriesT2 = topics2.flatMap(_.mandatory)
    lazy val mandatoriesT1AreNotTakenOnS2 = !mandatoriesT1.exists(slotSchedule2.mandatory -- mandatoriesT2) // T2 mandatories are not mandatory anymore
    lazy val mandatoriesT2AreNotTakenOnS1 = !mandatoriesT2.exists(slotSchedule1.mandatory -- mandatoriesT1) // T1 mandatories are not mandatory anymore
    lazy val mandatoriesT1ArePresentOnS2 = mandatoriesT1.forall(slotSchedule2.slot.personsPresent)
    lazy val mandatoriesT2ArePresentOnS1 = mandatoriesT2.forall(slotSchedule1.slot.personsPresent)
    lazy val minPersonsMoving1To2 = topics1.foldLeft(0)(_ + _.min) - topics2.foldLeft(0)(_ + _.min)
    lazy val minPersonsRequiredS1 = slotSchedule1.topics.foldLeft(0)(_ + _.min) - minPersonsMoving1To2
    lazy val minPersonsRequiredS2 = slotSchedule2.topics.foldLeft(0)(_ + _.min) + minPersonsMoving1To2
    lazy val enoughPersonsArePresentOnS1 = slotSchedule1.slot.personsPresentCount >= minPersonsRequiredS1
    lazy val enoughPersonsArePresentOnS2 = slotSchedule2.slot.personsPresentCount >= minPersonsRequiredS2
    lazy val maxPersonsMoving1To2 = topics1.foldLeft(0)(_ + _.max) - topics2.foldLeft(0)(_ + _.max)
    lazy val maxPersonsPossibleS1 = slotSchedule1.topics.foldLeft(0)(_ + _.max) - maxPersonsMoving1To2
    lazy val maxPersonsPossibleS2 = slotSchedule2.topics.foldLeft(0)(_ + _.max) + maxPersonsMoving1To2
    lazy val notTooManyPersonsAreOnS1 = slotSchedule1.slot.personsPresentCount <= maxPersonsPossibleS1
    lazy val notTooManyPersonsAreOnS2 = slotSchedule2.slot.personsPresentCount <= maxPersonsPossibleS2

    slot1IsNotFull && mandatoriesT2AreNotTakenOnS1 && mandatoriesT2ArePresentOnS1 && enoughPersonsArePresentOnS1 && notTooManyPersonsAreOnS1 &&
      slot2IsNotFull && mandatoriesT1AreNotTakenOnS2 && mandatoriesT1ArePresentOnS2 && enoughPersonsArePresentOnS2 && notTooManyPersonsAreOnS2
  }

  /** Swap topics between unscheduled and scheduled */
  private def possibleExtSwaps(schedule: Schedule)(using rand: Random): View[(Schedule, Move)] = for {
    slot <- shuffled(problem.slotsList).view
    slotSchedule = schedule.on(slot)
    _ = log.debug(s"Checking for possible Ext Swaps on slot ${slot.name}")
    oldTopic <- shuffled(slotSchedule.removableTopics).view

    /* Filter out impossible topics because of incompatibility */
    newTopic <- shuffled(schedule.unscheduledTopics -- slotSchedule.permanentlyIncompatibleTopics).view

    /* Filter out impossible topics because followup topic can't be added. Also, can't move followup topics directly. */
    if (newTopic.followup.isEmpty || slot.hasNext) && !newTopic.isFollowup && !oldTopic.isFollowup

    /* Handle simultaneous topics */
    oldTopics = simultaneousTopics(oldTopic)
    newTopics = simultaneousTopics(newTopic)

    /* Check we can add all those topics */
    if isExtSwapPossible(slotSchedule, oldTopics, newTopics)

    /* Get followup topics on next slot and check they can be added if needed */
    followupTopicsToRemove = oldTopics.flatMap(_.followup)
    followupTopicsToAdd = newTopics.flatMap(_.followup)
    if followupTopicsToAdd.isEmpty || slot.hasNext

    /* Create a new unfilled schedule with the followup topics added/removed if possible */
    unfilledNextSlotModified <-
      if (followupTopicsToRemove.isEmpty && followupTopicsToAdd.isEmpty) Some(schedule)
      else swapFollowupTopics(schedule, slot.next.get, followupTopicsToAdd, followupTopicsToRemove)

    /* Generate the swap */
    unfilled = unfilledNextSlotModified.clearSlots(slot).replaceTopics(slot, oldTopics, newTopics)

  } yield (unfilled, Move.Swap(oldTopics, newTopics, isExt = true))

  private def isExtSwapPossible(slotSchedule: SlotSchedule, topicsToRemove: Set[Topic], topicsToAdd: Set[Topic]): Boolean = {
    lazy val slotIsNotFull = slotSchedule.maxTopicsLeft >= topicsToAdd.size - topicsToRemove.size
    lazy val mandatoriesOld = topicsToRemove.flatMap(_.mandatory)
    lazy val mandatoriesNew = topicsToAdd.flatMap(_.mandatory)
    lazy val mandatoriesAreNotTaken = !mandatoriesNew.exists(slotSchedule.mandatory -- mandatoriesOld)
    lazy val mandatoriesArePresent = mandatoriesNew.forall(slotSchedule.slot.personsPresent)

    lazy val minPersonsRequired = slotSchedule.topics.foldLeft(0)(_ + _.min) - topicsToRemove.foldLeft(0)(_ + _.min) + topicsToAdd.foldLeft(0)(_ + _.min)
    lazy val enoughPersonsArePresent = slotSchedule.slot.personsPresentCount >= minPersonsRequired
    lazy val maxPersonsPossible = slotSchedule.topics.foldLeft(0)(_ + _.max) - topicsToRemove.foldLeft(0)(_ + _.max) + topicsToAdd.foldLeft(0)(_ + _.max)
    lazy val notTooManyPersonsArePresent = slotSchedule.slot.personsPresentCount <= maxPersonsPossible

    slotIsNotFull && mandatoriesAreNotTaken && mandatoriesArePresent && enoughPersonsArePresent && notTooManyPersonsArePresent
  }

  /** Remove a scheduled topic */
  private def possibleRemovals(schedule: Schedule)(using rand: Random): View[(Schedule, Move)] = for {
    slot <- shuffled(problem.slotsList).view
    slotSchedule = schedule.on(slot)
    _ = log.debug(s"Checking for possible Removals on slot ${slot.name}")
    topic <- shuffled(slotSchedule.removableTopics).view

    topicsToRemove = simultaneousTopics(topic)

    /* Check we can remove all those topics */
    if isRemovalPossible(slotSchedule, topicsToRemove)

    /* Get followup topics on next slot */
    followupTopicsToRemove = topicsToRemove.flatMap(_.followup)

    /* Create a new unfilled schedule with the followup topics removed if possible */
    unfilledNextSlotModified <-
      if (followupTopicsToRemove.isEmpty) Some(schedule) else swapFollowupTopics(schedule, slot.next.get, Set.empty, followupTopicsToRemove)

    /* Generate the swap */
    unfilled = unfilledNextSlotModified.removeTopics(slot, topicsToRemove)

  } yield (unfilled, Move.Remove(slot, topicsToRemove))

  private def isRemovalPossible(slotSchedule: SlotSchedule, topicsToRemove: Set[Topic]): Boolean = {
    lazy val maxPersonsPossible = slotSchedule.topics.foldLeft(0)(_ + _.max) - topicsToRemove.foldLeft(0)(_ + _.max)
    lazy val notTooManyPersonsArePresent = slotSchedule.slot.personsPresentCount <= maxPersonsPossible

    notTooManyPersonsArePresent
  }

  private def swapFollowupTopics(schedule: Schedule, nextSlot: Slot, topicsToAdd: Set[Topic], topicsToRemove: Set[Topic]): Option[Schedule] = {
    val nextSlotSchedule = schedule.on(nextSlot)
    if (isExtSwapPossible(nextSlotSchedule, topicsToRemove, topicsToAdd)) {
      Some(schedule.clearSlots(nextSlot).removeTopics(nextSlot, topicsToRemove).addTopics(nextSlot, topicsToAdd))
    } else {
      val followupMandatories: Set[Person] = topicsToAdd.flatMap(_.mandatory)
      // First check cases where it's definitely not possible: the mandatory persons are missing
      if (followupMandatories.exists(!nextSlot.personsPresent.contains(_))) None
      else {
        // Typically, we can't add because mandatory persons are already taken. We'll check that first.
        val competingTopics = nextSlotSchedule.topics.filter(t => t.mandatory.exists(followupMandatories)).toSet
        lazy val allTopicsToRemove = topicsToRemove ++ competingTopics
        if (competingTopics.nonEmpty && !competingTopics.exists(_.forced) && isExtSwapPossible(nextSlotSchedule, allTopicsToRemove, topicsToAdd)) {
          Some(schedule.clearSlots(nextSlot).updateSlotSchedule(nextSlot)(_.removeTopics(allTopicsToRemove).addTopics(topicsToAdd)))
        } else {
          // No competing topics, or removing them is not enough. We could try dropping more topics, but let's stop there for now. TODO
          None
        }
      }
    }
  }

  private def shuffled[A](set: Set[A])(using rand: Random): Seq[A] = shuffled(set.toSeq)

  private def shuffled[A](seq: Seq[A])(using rand: Random): Seq[A] = rand.shuffle(seq)

  private def shuffled[A](it: Iterable[A])(using rand: Random): Iterable[A] = rand.shuffle(it)

  private def simultaneousTopics(topic: Topic): Set[Topic] = problem.simultaneousTopicByTopic(topic) + topic

}

object PlanningSpaceNavigator {

  /** Moves on the Schedule */
  trait Move {
    def reverts(m: Move): Boolean
  }

  object Move {

    case object Nothing extends Move {
      override def reverts(m: Move): Boolean = false
    }

    /**
     * @param isExt Swap is between scheduled and unscheduled topics. If `true`, `right` are the topics being scheduled in.
     */
    final case class Swap(left: Set[Topic], right: Set[Topic], isExt: Boolean) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Swap(left2, right2, isExt2) if isExt == isExt2 && ((left == left2 && right == right2) || (left == right2 && right == left2)) => true
        case _ => false
      }

      override def toString: String = s"Swap${if (isExt) "Ext" else ""}(${left.map(_.name).mkString(",")} <-> ${right.map(_.name).mkString(",")})"
    }

    final case class Add(slot: Slot, topics: Set[Topic]) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Remove(s1, t1) if slot == s1 && topics == t1 => true
        case _ => false
      }

      override def toString: String = s"Add(${slot.name}: ${topics.map(_.name).mkString(",")})"
    }

    final case class Remove(slot: Slot, topics: Set[Topic]) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Add(s1, t1) if slot == s1 && topics == t1 => true
        case _ => false
      }

      override def toString: String = s"Remove(${slot.name}: ${topics.map(_.name).mkString(",")})"
    }

  }

}
