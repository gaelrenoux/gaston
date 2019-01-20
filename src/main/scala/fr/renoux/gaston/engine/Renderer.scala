package fr.renoux.gaston.engine

import java.text.DecimalFormat

import fr.renoux.gaston.input.InputSettings
import fr.renoux.gaston.model.preferences.PersonTopicPreference
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Schedule, Score}

class Renderer(
    val settings: InputSettings,
    val problem: Problem
) {

  private val scorer = Scorer.of(problem)

  private val ScoreDecimalFormat = new DecimalFormat("000.00")

  private val ShortScoreDecimalFormat = new DecimalFormat("000")

  /** For each person, preferences */
  val preferencesByPerson: Map[Person, Set[PersonTopicPreference]] = problem.preferences.collect {
    case p: PersonTopicPreference => p
  }.groupBy(_.person)

  /** Formats the schedule and analysis to a pretty String */
  def all(schedule: Schedule, score: Score): String =
    s"\n${schedule.toFormattedString}\nSchedule score is $score\n\n${personsSatisfaction(schedule)}\n"


  /** Display the persons' scores for that schedule */
  def personsSatisfaction(schedule: Schedule): String = {
    val weightedScoresByPerson: Map[Person, Score] = scorer.weightedScoresByPerson(schedule)

    /* For each name, weighted score and descending list of satisfied rewards */
    val summaryByPerson: Seq[(String, Double, Seq[Double])] = preferencesByPerson.map {
      case (person, preferences) =>
        val satisfied = preferences.filter(_.score(schedule) > Score.Zero).toSeq.map(_.reward.value).sorted.reverse
        (person.name, weightedScoresByPerson(person).value, satisfied)
    }.toSeq

    val summariesFromBestToWorse = summaryByPerson.sortBy(_._2).reverse

    val summaryTextBody = summariesFromBestToWorse.map { case (name, score, satisfied) =>
      val nameTxt = name.padTo(8, ' ').take(8)
      val scoreTxt = ScoreDecimalFormat.format(score)
      val satisfiedTxt = satisfied.map(ShortScoreDecimalFormat.format).mkString(" ")
      s"$nameTxt    $scoreTxt    ($satisfiedTxt)"
    }.mkString("\n")

    /* Adds a title line */
    s"Person      Score     (Detail)\n$summaryTextBody"
  }

}
