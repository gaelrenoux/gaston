package fr.renoux.gaston.model.problem

import fr.renoux.gaston.model.Problem
import fr.renoux.gaston.model.constraints.TopicNeedsNumberOfPersons
import scalaz.Scalaz._

import scala.annotation.tailrec

class Metrics(problem: Problem) {

  /** Upper limit of the number of possible schedules */
  def scheduleCountUpperLimit: BigInt = {
    val maxPersonsPerTopic = problem.constraints.collect {
      case TopicNeedsNumberOfPersons(_, _, max) => max
    }.max
    val minTopicsPerSlot = (problem.personsCount.toDouble / maxPersonsPerTopic).ceil.toInt
    val maxTopicsPerSlot = problem.maxTopicCountPerSlot.nonEmpty.option(problem.maxTopicCountPerSlot.values.max).getOrElse((problem.topics.size.toDouble / problem.slots.size).floor.toInt)

    Metrics.scheduleCountUpperLimit(
      slots = problem.slots.size,
      topics = problem.topics.size,
      minTopicsPerSlot = minTopicsPerSlot,
      maxTopicsPerSlot = maxTopicsPerSlot
    )
  }
}

object Metrics {

  def scheduleCountUpperLimit(slots: BigInt, topics: BigInt, minTopicsPerSlot: BigInt, maxTopicsPerSlot: BigInt): BigInt = {

    @tailrec
    def fact(n: BigInt, acc: BigInt = 1): BigInt = if (n <= 1) acc else fact(n - 1, acc * n)

    def comb(n: BigInt, k: BigInt): BigInt = fact(n) / fact(k) / fact(n - k)

    def recCount(slots: BigInt, topics: BigInt): BigInt = {
      if (slots * minTopicsPerSlot > topics) {
        0

      } else if (slots == 1) {
        val max = maxTopicsPerSlot.min(topics)
        val r = (minTopicsPerSlot to max).map(comb(topics, _)).sum
        r

      } else {
        val r = (minTopicsPerSlot to maxTopicsPerSlot).map(n => comb(topics, n) * recCount(slots - 1, topics - n)).sum
        r
      }
    }

    recCount(slots, topics)
  }

}