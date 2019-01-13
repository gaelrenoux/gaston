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

  /** For each person, preferences (strongs are marked with true) */
  val preferencesByPerson: Map[Person, Set[(PersonTopicPreference, Boolean)]] = problem.preferences.collect {
    case p@PersonTopicPreference(_, _, settings.strongPreference) => (p, true)
    case p@PersonTopicPreference(_, _, settings.weakPreference) => (p, false)
  }.groupBy(_._1.person)

  /** Formats the schedule and analysis to a pretty String */
  def all(schedule: Schedule, score: Score): String =
    s"\n${schedule.toFormattedString}\nSchedule score is $score\n\n${personsSatisfaction(schedule)}\n"


  /** Display the persons' scores for that schedule */
  def personsSatisfaction(schedule: Schedule): String = {
    val weightedScoresByPerson: Map[Person, Score] = scorer.weightedScoresByPerson(schedule)

    val summaryByPerson: Map[String, (Score, Int, Int)] = preferencesByPerson.map {
      case (person, preferences) =>
        val satisfied = preferences.filter(_._1.score(schedule) > Score.Zero)
        person.name -> (weightedScoresByPerson(person), satisfied.count(_._2), satisfied.count(!_._2))
    }

    val summariesFromBestToWorse = summaryByPerson.toSeq.sortBy(_._2._1).reverse

    val summaryTextBody = summariesFromBestToWorse.map { case (name, (s, strong, weak)) =>
      val nameTxt = name.padTo(8, ' ').take(8)
      val scoreTxt = ScoreDecimalFormat.format(s.value)
      val strongTxt = f"$strong%2d"
      val weakTxt = f"$weak%2d"
      s"$nameTxt    $scoreTxt    $strongTxt  $weakTxt"
    }.mkString("\n")

    /* Adds a title line */
    s"Person      Score     Str Weak\n$summaryTextBody"
  }

}
