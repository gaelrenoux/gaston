package fr.renoux.gaston.command

import java.text.DecimalFormat

import fr.renoux.gaston.model._
import fr.renoux.gaston.model.preferences.PersonTopicPreference

/** A tool to render a solution as a pretty String. */
class Renderer(
    val problem: Problem
) {

  import Renderer._

  import Ordering.Double.IeeeOrdering

  /** For each person, preferences */
  private val preferencesByPerson: Map[Person, Set[PersonTopicPreference]] = problem.preferences.collect {
    case p: PersonTopicPreference => p
  }.groupBy(_.person)

  /** Formats the schedule and analysis to a pretty String. Empty lines at the beginning and the end. */
  def apply(schedule: Schedule): String = {
    val weightedScoresByPerson: Map[Person, Score] = Scorer.weightedScoresByPerson(schedule)

    /* For each name, weighted score, descending list of satisfied rewards, number of mandatory topics */
    val summaryByPerson: Seq[(String, Double, Seq[Double], Int)] = preferencesByPerson.toSeq.map {
      case (person, preferences) =>
        val satisfied = preferences.filter(_.score(schedule) > Score.Zero).toSeq.map(_.reward.value).sorted.reverse
        val mandatoryCount = problem.mandatoryTopicsPerPerson(person).size
        (person.name, weightedScoresByPerson(person).value, satisfied, mandatoryCount)
    }

    val summariesFromBestToWorse = summaryByPerson.sortBy(_._2).reverse

    val summaryTextBody = summariesFromBestToWorse.map { case (name, score, satisfied, mandatoryCount) =>
      val nameTxt = name.padTo(8, ' ').take(8) // scalastyle:ignore magic.number
      val scoreTxt = ScoreDecimalFormat.format(score)
      val satisfiedTxt = satisfied.map(ShortScoreDecimalFormat.format).mkString(" ")
      val mandatoryTxt = " MND" * mandatoryCount
      s"$nameTxt    $scoreTxt    ($satisfiedTxt$mandatoryTxt)"
    }.mkString("\n")

    s"""
       |${schedule.toFormattedString}
       |Schedule score is ${schedule.score}
       |
       |Person      Score     (Detail)
       |$summaryTextBody
       |""".stripMargin
  }

}

object Renderer {

  private val ScoreDecimalFormat = new DecimalFormat("000.00")

  private val ShortScoreDecimalFormat = new DecimalFormat("000")

}
