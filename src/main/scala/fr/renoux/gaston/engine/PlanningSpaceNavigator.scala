package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.PlanningSpaceNavigator.Move
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.immutable

import scala.collection.View
import scala.util.Random


/** Tools to explore the space solution for plannings (ie, not doing the assignment). Schedules returned are always
  * partial (they just contain the planning), and valid (all constraints are matched, and no one is mandatory on two
  * topics at the same time). */
@immutable
final class PlanningSpaceNavigator(implicit private val problem: Problem) {

  private val log: Logger = Logger[PlanningSpaceNavigator]

  /** Return a LazyList of neighbouring partial schedules to the initial one. */
  def neighbours(schedule: Schedule)(implicit rand: Random): LazyList[(Schedule, Move)] = {

    lazy val allAdds = possibleAdds(schedule)
    lazy val allSwaps = possibleSwaps(schedule)
    lazy val allExternalSwaps = possibleExtSwaps(schedule)
    lazy val allRemovals = possibleRemovals(schedule)

    LazyList.from(allAdds ++ allSwaps ++ allExternalSwaps ++ allRemovals)
  }

  /** Add an unscheduled topic */
  private def possibleAdds(schedule: Schedule)(implicit rand: Random): View[(Schedule, Move)] = for {
    slot <- shuffled(problem.slotsList).view
    slotSchedule = schedule.on(slot)
    _ = log.debug(s"Checking for possible Additions on slot ${slot.name}")

    /* Filter out impossible topics because of incompatibility */
    topic <- shuffled(schedule.unscheduledTopics -- slotSchedule.incompatibleTopics).view

    /* Handle simultaneous topics */
    topicsToAdd = simultaneousTopics(topic)

    /* Check we can add all those topics */
    if isAddPossible(slotSchedule, topicsToAdd)

    /* Generate the add */
    partial = schedule.clearSlots(slot).addTopics(slot, topicsToAdd)

    /* Filter out impossible adds because of unreachable minimum */
    if partial.on(slot).minPersons.forall(_ <= slot.personsPresentCount)

  } yield (partial, Move.Add(slot, topicsToAdd))

  private def isAddPossible(slotSchedule: SlotSchedule, topicsToAdd: Set[Topic]): Boolean = {
    lazy val slotIsNotFull = slotSchedule.maxTopicsLeft >= topicsToAdd.size
    lazy val mandatories = topicsToAdd.flatMap(_.mandatory)
    lazy val mandatoriesAreNotTaken = !mandatories.exists(slotSchedule.mandatory)
    lazy val mandatoriesArePresent = mandatories.forall(slotSchedule.slot.personsPresent)
    slotIsNotFull && mandatoriesAreNotTaken && mandatoriesArePresent
  }


  /** Swap two scheduled topics */
  private def possibleSwaps(schedule: Schedule)(implicit rand: Random): View[(Schedule, Move)] = for {
    (slot1, slot2) <- shuffled(problem.slotCouplesSeq).view
    slotSchedule1 = schedule.on(slot1)
    slotSchedule2 = schedule.on(slot2)
    _ = log.debug(s"Checking for possible Swaps between slots ${slot1.name} and ${slot2.name}")

    /* Filter out impossible topics because of incompatibility */
    t1 <- shuffled(slotSchedule1.realTopicsSet -- slotSchedule2.permanentlyIncompatibleTopics).view
    t2 <- shuffled(slotSchedule2.realTopicsSet -- slotSchedule1.permanentlyIncompatibleTopics).view

    topics1 = simultaneousTopics(t1)
    topics2 = simultaneousTopics(t2)

    /* Check we can indeed swap these topics */
    if isSwapPossible(slotSchedule1, slotSchedule2, topics1, topics2)

    /* Generate the swap */
    partial = schedule.clearSlots(slot1, slot2).swapTopics(slot1 -> topics1, slot2 -> topics2)

    /* Filter out impossible swaps because of unreachable minimum */
    if partial.on(slot1).minPersons.forall(_ <= slot1.personsPresentCount)
    if partial.on(slot2).minPersons.forall(_ <= slot2.personsPresentCount)

    /* Filter out impossible swaps because of maximum too low */
    if partial.on(slot1).maxPersons.forall(_ >= slot1.personsPresentCount)
    if partial.on(slot2).maxPersons.forall(_ >= slot2.personsPresentCount)

  } yield (partial, Move.Swap(topics1, topics2, isExt = false))

  private def isSwapPossible(slotSchedule1: SlotSchedule, slotSchedule2: SlotSchedule, topics1: Set[Topic], topics2: Set[Topic]): Boolean = {
    lazy val slot1IsNotFull = slotSchedule1.maxTopicsLeft >= topics2.size - topics1.size
    lazy val slot2IsNotFull = slotSchedule2.maxTopicsLeft >= topics1.size - topics2.size
    lazy val mandatoriesT1 = topics1.flatMap(_.mandatory)
    lazy val mandatoriesT2 = topics2.flatMap(_.mandatory)
    lazy val mandatoriesT1AreNotTakenOnS2 = !mandatoriesT1.exists(slotSchedule2.mandatory -- mandatoriesT2) // T2 mandatories are not mandatory anymore
    lazy val mandatoriesT2AreNotTakenOnS1 = !mandatoriesT2.exists(slotSchedule1.mandatory -- mandatoriesT1) // T1 mandatories are not mandatory anymore
    lazy val mandatoriesT1ArePresentOnS2 = mandatoriesT1.forall(slotSchedule2.slot.personsPresent)
    lazy val mandatoriesT2ArePresentOnS1 = mandatoriesT2.forall(slotSchedule1.slot.personsPresent)
    slot1IsNotFull && mandatoriesT2AreNotTakenOnS1 && mandatoriesT2ArePresentOnS1 &&
      slot2IsNotFull && mandatoriesT1AreNotTakenOnS2 && mandatoriesT1ArePresentOnS2
  }


  /** Swap topics between unscheduled and scheduled */
  private def possibleExtSwaps(schedule: Schedule)(implicit rand: Random): View[(Schedule, Move)] = for {
    slot <- shuffled(problem.slotsList).view
    slotSchedule = schedule.on(slot)
    _ = log.debug(s"Checking for possible Ext Swaps on slot ${slot.name}")
    oldTopic <- shuffled(slotSchedule.removableTopics).view

    /* Filter out impossible topics because of incompatibility */
    newTopic <- shuffled(schedule.unscheduledTopics -- slotSchedule.permanentlyIncompatibleTopics).view

    oldTopics = simultaneousTopics(oldTopic)
    newTopics = simultaneousTopics(newTopic)

    /* Check we can add all those topics */
    if isExtSwapPossible(slotSchedule, oldTopics, newTopics)

    /* Generate the swap */
    partial = schedule.clearSlots(slot).replaceTopics(slot, oldTopics, newTopics)

    /* Filter out impossible swaps because of unreachable minimum */
    if partial.on(slot).minPersons.forall(_ <= slot.personsPresentCount)

    /* Filter out impossible swaps because of maximum too low */
    if partial.on(slot).maxPersons.forall(_ >= slot.personsPresentCount)

  } yield (partial, Move.Swap(oldTopics, newTopics, isExt = true))

  private def isExtSwapPossible(slotSchedule: SlotSchedule, oldTopics: Set[Topic], newTopics: Set[Topic]): Boolean = {
    lazy val slotIsNotFull = slotSchedule.maxTopicsLeft >= newTopics.size - oldTopics.size
    lazy val mandatoriesOld = oldTopics.flatMap(_.mandatory)
    lazy val mandatoriesNew = newTopics.flatMap(_.mandatory)
    lazy val mandatoriesAreNotTaken = !mandatoriesNew.exists(slotSchedule.mandatory -- mandatoriesOld)
    lazy val mandatoriesArePresent = mandatoriesNew.forall(slotSchedule.slot.personsPresent)
    slotIsNotFull && mandatoriesAreNotTaken && mandatoriesArePresent
  }


  /** Remove a scheduled topic */
  private def possibleRemovals(schedule: Schedule)(implicit rand: Random): View[(Schedule, Move)] = for {
    slot <- shuffled(problem.slotsList).view
    slotSchedule = schedule.on(slot)
    _ = log.debug(s"Checking for possible Removals on slot ${slot.name}")
    topic <- shuffled(slotSchedule.removableTopics).view
    topicsToRemove = simultaneousTopics(topic)

    /* Generate the swap */
    partial = schedule.removeTopics(slot, topicsToRemove)

    /* Filter out impossible removals because of maximum too low */
    if partial.on(slot).maxPersons.forall(_ >= slot.personsPresentCount)

  } yield (partial, Move.Remove(slot, topicsToRemove))


  private def shuffled[A](set: Set[A])(implicit rand: Random): Seq[A] = shuffled(set.toSeq)

  private def shuffled[A](seq: Seq[A])(implicit rand: Random): Seq[A] = rand.shuffle(seq)

  private def shuffled[A](it: Iterable[A])(implicit rand: Random): Iterable[A] = rand.shuffle(it)

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
    case class Swap(left: Set[Topic], right: Set[Topic], isExt: Boolean) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Swap(left2, right2, isExt2) if isExt == isExt2 && ((left == left2 && right == right2) || (left == right2 && right == left2)) => true
        case _ => false
      }

      override def toString: String = s"Swap${if (isExt) "Ext" else ""}(${left.map(_.name).mkString(",")} <-> ${right.map(_.name).mkString(",")})"
    }

    case class Add(slot: Slot, topics: Set[Topic]) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Remove(s1, t1) if slot == s1 && topics == t1 => true
        case _ => false
      }

      override def toString: String = s"Add(${slot.name}: ${topics.map(_.name).mkString(",")})"
    }

    case class Remove(slot: Slot, topics: Set[Topic]) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Add(s1, t1) if slot == s1 && topics == t1 => true
        case _ => false
      }

      override def toString: String = s"Remove(${slot.name}: ${topics.map(_.name).mkString(",")})"
    }

  }

}
