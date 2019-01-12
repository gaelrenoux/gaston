package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Schedule, Score}

import scala.annotation.tailrec
import scala.util.Random

/**
  * Improves an existing Schedule by satisfying preferences.
  */
class HeuristicScheduleImprover(val problem: Problem, val scorer: Scorer) extends ScheduleImprover {

  private val log = Logger[HeuristicScheduleImprover]

  override def improve(schedule: Schedule, initialScore: Score, rounds: Int = 10000)(implicit rand: Random): Schedule = {
    val scoreByPerson = scorer.weightedScoresByPerson(schedule)
    heuristicAmelioration(schedule, scoreByPerson, rounds)
  }

  @tailrec
  final def heuristicAmelioration(schedule: Schedule, scoreByPerson: Map[Person, Score], maxRounds: Int = 1000): Schedule =
    if (maxRounds == 0) {
      log.debug("Stopping heuristic amelioration because max number of rounds was reached")
      schedule
    } else {
      val (personToImprove, score) = scoreByPerson.minBy(_._2)

      bestMoveForPerson(schedule, personToImprove)

      val preferencesToImprove = problem.preferencesPerPerson(personToImprove).filter(_.score(schedule).value <= 0)
      val sortedPreferencesToImprove = preferencesToImprove.toSeq.sortBy(p => math.abs(p.reward.value)).reverse

      val (candidate, candidateScore) = bestMoveForPerson(schedule, personToImprove).getOrElse(schedule, score)
      if (candidateScore.value > score.value)
        heuristicAmelioration(candidate, scorer.weightedScoresByPerson(candidate), maxRounds - 1)
      else
        heuristicAmelioration(schedule, scoreByPerson, maxRounds - 1)
    }

  /** Returns the best possible swap on a specific slot */
  private def bestMoveForPerson(schedule: Schedule, person: Person): Option[(Schedule, Score)] = {
    ???

    /*
    val topics = schedule.topicsPerSlot(slot)
    val records = schedule.records.filter(_.slot == slot)

    val movableFromTopic = topics map { t => t -> (schedule.personsPerTopic(t) -- problem.mandatoryPersonsPerTopic(t)) } toMap

    val swappedSchedules = for {
      r1 <- records
      r2 <- records - r1
      p1 <- movableFromTopic(r1.topic) -- problem.forbiddenPersonsPerTopic(r2.topic)
      p2 <- movableFromTopic(r2.topic) -- problem.forbiddenPersonsPerTopic(r1.topic)
    } yield {
      val newR1 = r1.copy(persons = r1.persons - p1 + p2)
      val newR2 = r2.copy(persons = r2.persons - p2 + p1)
      schedule.copy(records = schedule.records - r1 - r2 + newR1 + newR2)
      /* if (p1 == Person("fatima") && p2 == Person("adam")) Some(schedule.copy(schedule.records - r1 - r2 + newR1 + newR2))
      else None */
    }

    if (swappedSchedules.nonEmpty) Some(swappedSchedules map { s => (s, score(s)) } maxBy (_._2))
    else None
    */
  }


}
