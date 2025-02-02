package fr.renoux.gaston.command

import fr.renoux.gaston.model.*
import fr.renoux.gaston.model.preferences.PersonTopicPreference

import java.text.DecimalFormat

/** A tool to render a solution as a pretty String. */
final class Renderer(
    val problem: Problem
) {

  import Renderer.*

  import Ordering.Double.IeeeOrdering

  /** For each person, preferences */
  private val topicPreferencesByPerson: Map[Person, Set[PersonTopicPreference]] = problem.preferences.collect {
    case p: PersonTopicPreference => p
  }.groupBy(_.person)

  private val otherPreferences: Set[Preference] = problem.preferences.filterNot(_.isInstanceOf[PersonTopicPreference])

  /** Formats the schedule and analysis to a pretty String. Empty lines at the beginning and the end. */
  def apply(schedule: Schedule): String = {
    val weightedScoresByPerson: Map[Person, Score] = schedule.scoreCalculator.weightedScoresByPerson

    /* For each name, weighted score, base score, descending list of satisfied rewards, number of mandatory topics */
    val summaryByPerson: Seq[(String, Double, Double, Seq[Double], Int)] = topicPreferencesByPerson.toSeq.map {
      case (person, preferences) =>
        val score = weightedScoresByPerson(person).value
        val baseScore = problem.baseScoreByPerson.getOrElse(person, Score.Zero).value
        val satisfied = preferences.view.filter(_.score(schedule) != Score.Zero).map(_.reward.value).toSeq.sorted.reverse
        val mandatoryCount = problem.mandatoryTopicsByPerson(person).intersect(schedule.scheduledTopics).size
        (person.name, score, baseScore, satisfied, mandatoryCount)
    }

    val summariesFromBestToWorse = summaryByPerson.sortBy(_._2).reverse

    val summaryTextBody = summariesFromBestToWorse.map { case (name, score, baseScore, satisfied, mandatoryCount) =>
      val nameTxt = name.padTo(8, ' ').take(8)
      val scoreTxt = ScoreDecimalFormat.format(score)
      val baseScoreTxt = if (baseScore == 0.0) "" else ShortScoreDecimalFormat.format(baseScore) + " "
      val mandatoryTxt = if (mandatoryCount == 0) "" else Seq.fill(mandatoryCount)(Record.MandatoryMarker).mkString(" ", "  ", " ")
      val satisfiedTxt = satisfied.map(ShortScoreDecimalFormat.format).mkString(" ")
      s"$nameTxt    $scoreTxt    ($baseScoreTxt$mandatoryTxt$satisfiedTxt )"
    }.mkString("\n")

    val notableOtherPrefs = otherPreferences.filter(_.score(schedule) != Score.Zero)
    val notableOtherPrefsTxt = if (notableOtherPrefs.isEmpty) "" else {
      s"\nOther:\n${notableOtherPrefs.map(p => s"  ${p.toLongString}").mkString("\n")}"
    }

    s"""${schedule.toFormattedString}
       |Schedule score is ${schedule.score.value}
       |
       |Person       Score     ( Detail )
       |$summaryTextBody
       |$notableOtherPrefsTxt
       |""".stripMargin
  }

}

object Renderer {

  val ScoreDecimalFormat = new DecimalFormat(" 000.00;-0")

  val ShortScoreDecimalFormat = new DecimalFormat(" 000;-0")

}
