package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.PlanningSpaceNavigator.Move
import fr.renoux.gaston.model._

import scala.collection.View
import scala.util.Random


/** Tools to explore the space solution for plannings (ie, not doing the assignment). Schedules returned are always
  * partial. */
final class PlanningSpaceNavigator(implicit private val problem: Problem) {

  val log: Logger = Logger[PlanningSpaceNavigator]

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

    topicsToAdd = linkedTopics(topic)

    /* Filter out topics coming in excessive number */
    if slotSchedule.maxTopicsLeft >= topicsToAdd.size

    personsMandatoryOnTopic = topicsToAdd.flatMap(_.mandatory)

    /* Filter out impossible adds because mandatory persons are already taken */
    if !personsMandatoryOnTopic.exists(slotSchedule.mandatory)

    /* Filter out impossible adds because mandatory persons are missing */
    if personsMandatoryOnTopic.forall(slot.personsPresent)

    /* Generate the swap */
    partial = schedule.clearSlots(slot).addTopics(slot, topicsToAdd)

    /* Filter out impossible adds because of unreachable minimum */
    if partial.on(slot).minPersons.forall(_ <= slot.personsPresentCount)

  } yield (partial, Move.Add(slot, topicsToAdd))


  /** Swap two scheduled topics */
  private def possibleSwaps(schedule: Schedule)(implicit rand: Random): View[(Schedule, Move)] = for {
    (slot1, slot2) <- shuffled(problem.slotCouplesSeq).view
    slotSchedule1 = schedule.on(slot1)
    slotSchedule2 = schedule.on(slot2)
    _ = log.debug(s"Checking for possible Swaps between slots ${slot1.name} and ${slot2.name}")

    /* Filter out impossible topics because of incompatibility */
    t1 <- shuffled(slotSchedule1.realTopicsSet -- slotSchedule2.permanentlyIncompatibleTopics).view
    t2 <- shuffled(slotSchedule2.realTopicsSet -- slotSchedule1.permanentlyIncompatibleTopics).view

    topics1 = linkedTopics(t1)
    topics2 = linkedTopics(t2)

    /* Filter out topics coming in excessive number */
    if slotSchedule1.maxTopicsLeft >= topics2.size - topics1.size
    if slotSchedule2.maxTopicsLeft >= topics1.size - topics2.size

    personsMandatoryOnT1 = topics1.flatMap(_.mandatory)
    personsMandatoryOnT2 = topics2.flatMap(_.mandatory)

    /* Filter out impossible adds because mandatory persons are already taken */
    personsAlreadyMandatoryOnS1 = slotSchedule1.mandatory -- personsMandatoryOnT1 // taking this topic off, they're not mandatory any more on the slot
    personsAlreadyMandatoryOnS2 = slotSchedule2.mandatory -- personsMandatoryOnT2 // taking this topic off, they're not mandatory any more on the slot
    if !personsMandatoryOnT1.exists(personsAlreadyMandatoryOnS2) // check mandatories of T1 are not already blocked on S2
    if !personsMandatoryOnT2.exists(personsAlreadyMandatoryOnS1) // check mandatories of T2 are not already blocked on

    /* Filter out impossible adds because mandatory persons are missing */
    if personsMandatoryOnT1.forall(slot2.personsPresent)
    if personsMandatoryOnT2.forall(slot1.personsPresent)

    /* Generate the swap */
    partial = schedule.clearSlots(slot1, slot2).swapTopics(slot1 -> topics1, slot2 -> topics2)

    /* Filter out impossible swaps because of unreachable minimum */
    if partial.on(slot1).minPersons.forall(_ <= slot1.personsPresentCount)
    if partial.on(slot2).minPersons.forall(_ <= slot2.personsPresentCount)

    /* Filter out impossible swaps because of maximum too low */
    if partial.on(slot1).maxPersons.forall(_ >= slot1.personsPresentCount)
    if partial.on(slot2).maxPersons.forall(_ >= slot2.personsPresentCount)

  } yield (partial, Move.Swap(topics1, topics2, isExt = false))


  /** Swap topics between unscheduled and scheduled */
  private def possibleExtSwaps(schedule: Schedule)(implicit rand: Random): View[(Schedule, Move)] = for {
    slot <- shuffled(problem.slotsList).view
    slotSchedule = schedule.on(slot)
    _ = log.debug(s"Checking for possible Ext Swaps on slot ${slot.name}")
    oldTopic <- shuffled(slotSchedule.removableTopics).view

    /* Filter out impossible topics because of incompatibility */
    newTopic <- shuffled(schedule.unscheduledTopics -- slotSchedule.permanentlyIncompatibleTopics).view

    oldTopics = linkedTopics(oldTopic)
    newTopics = linkedTopics(newTopic)

    /* Filter out topics coming in excessive number */
    if slotSchedule.maxTopicsLeft >= newTopics.size - oldTopics.size

    personsMandatoryOnOldTs = oldTopics.flatMap(_.mandatory)
    personsMandatoryOnNewTs = newTopics.flatMap(_.mandatory)

    /* Filter out impossible swaps because of mandatory persons */
    personsAlreadyMandatoryOnSlot = slotSchedule.mandatory -- personsMandatoryOnOldTs // taking this topic off, they're not mandatory any more on the slot
    if !personsMandatoryOnNewTs.exists(personsAlreadyMandatoryOnSlot)

    /* Filter out impossible adds because mandatory persons are missing */
    if personsMandatoryOnNewTs.forall(slot.personsPresent)

    /* Generate the swap */
    partial = schedule.clearSlots(slot).replaceTopics(slot, oldTopics, newTopics)

    /* Filter out impossible swaps because of unreachable minimum */
    if partial.on(slot).minPersons.forall(_ <= slot.personsPresentCount)

    /* Filter out impossible swaps because of maximum too low */
    if partial.on(slot).maxPersons.forall(_ >= slot.personsPresentCount)

  } yield (partial, Move.Swap(oldTopics, newTopics, isExt = true))


  /** Remove a scheduled topic */
  private def possibleRemovals(schedule: Schedule)(implicit rand: Random): View[(Schedule, Move)] = for {
    slot <- shuffled(problem.slotsList).view
    slotSchedule = schedule.on(slot)
    _ = log.debug(s"Checking for possible Removals on slot ${slot.name}")
    topic <- shuffled(slotSchedule.removableTopics).view
    topicsToRemove = linkedTopics(topic)

    /* Generate the swap */
    partial = schedule.removeTopics(slot, topicsToRemove)

    /* Filter out impossible removals because of maximum too low */
    if partial.on(slot).maxPersons.forall(_ >= slot.personsPresentCount)

  } yield (partial, Move.Remove(slot, topicsToRemove))


  private def shuffled[A](set: Set[A])(implicit rand: Random): Seq[A] = shuffled(set.toSeq)

  private def shuffled[A](seq: Seq[A])(implicit rand: Random): Seq[A] = rand.shuffle(seq)

  private def shuffled[A](it: Iterable[A])(implicit rand: Random): Iterable[A] = rand.shuffle(it)

  private def linkedTopics(topic: Topic): Set[Topic] = problem.simultaneousTopicByTopic(topic) + topic

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
    }

    case class Add(slot: Slot, topics: Set[Topic]) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Remove(s1, t1) if slot == s1 && topics == t1 => true
        case _ => false
      }
    }

    case class Remove(slot: Slot, topics: Set[Topic]) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Add(s1, t1) if slot == s1 && topics == t1 => true
        case _ => false
      }
    }

  }

}
