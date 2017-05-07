package fr.renoux.gaston.engine

import fr.renoux.gaston.model.{Person, Problem, Solution, Topic}

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
object Engine {

  def getSolution(problem: Problem)(implicit random: Random) = {


  }

  def generateRandomSolution(problem: Problem)(implicit random: Random) = {
    val slotsCount = problem.slots.size
    val topicsCount = problem.topics.size
    val personsCount = problem.persons.size

    /* dispatch topics on slots */
    val slotTopics = {
      val randomizedTopics = random.shuffle(problem.topics.toSeq)
      val groupedTopics = Seq.fill(slotsCount)(ListBuffer[Topic]())
      for (i <- 0 until topicsCount) groupedTopics(i % slotsCount) += randomizedTopics(i)
      val randomizedSlots = random.shuffle(problem.slots.toSeq)
      randomizedSlots zip (groupedTopics map (_.toSeq))
    }

    /* for each slot, dispatch persons on topics */
    val schedule = slotTopics map { case (slot, topics) =>
      val randomizedPersons = random.shuffle(problem.persons.toSeq)
      val currentTopicsCount = topics.size
      val groupedPersons = Seq.fill(currentTopicsCount)(ListBuffer[Person]())
      for (i <- 0 until personsCount) groupedPersons(i % currentTopicsCount) += randomizedPersons(i)
      slot -> (topics zip (groupedPersons map (_.toSet)))
    }

    val flatSchedule = schedule flatMap { case (slot, couples) =>
      couples map { c => (slot, c._1, c._2) }
    } toSet

    Solution(flatSchedule)
  }

  def getBetterSolution(problem: Problem, solution: Solution)(implicit random: Random) =
    if (solution.areMandatoryConstraintsSatisified(problem.constraints)) getBetterSolutionWithSmallChanges(problem, solution)
    else getBetterSolutionWithBigChanges(problem, solution)

  private def getBetterSolutionWithBigChanges(problem: Problem, solution: Solution, triesLeft: Int = 1000)(implicit random: Random) = if (triesLeft == 0) None else {
    /* switch topics between slots, then people between slots */
  }

  private def getBetterSolutionWithSmallChanges(problem: Problem, solution: Solution, triesLeft: Int = 1000)(implicit random: Random) = if (triesLeft == 0) None else {

  }


}
