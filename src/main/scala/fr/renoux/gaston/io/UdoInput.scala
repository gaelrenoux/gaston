package fr.renoux.gaston.io

import java.nio.file.Path

import fr.renoux.gaston.Settings
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonTopicPreference, PersonsIncompatibilityAntiPreference, Preference}
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Score, Slot, Topic}
import pureconfig.error.ConfigReaderFailures
import pureconfig.{loadConfig, loadConfigFromFiles}


object UdoInput extends Input {

  def fromClassPath: (Problem, Settings) = from(loadConfig[InputRoot])

  def from(files: Path*): (Problem, Settings) = from(loadConfigFromFiles[InputRoot](files))

  /** Load the real input from the user model */
  def from(either: Either[ConfigReaderFailures, InputRoot]): (Problem, Settings) = {
    val model: InputModel = either match {
      case Left(failures) => throw new IllegalStateException(s"Could not parse configuration:\n${failures.toList.mkString("\n")}")
      case Right(res) => res.gaston
    }

    val slotsPerName = model.slots map { s => s -> Slot(s) } toMap //TODO other attributes ?
    val topicsPerName = model.topics map { t => t.name -> Topic(t.name) } toMap //TODO other attributes ?
    val personsPerName = model.persons map { p => p -> Person(p) } toMap //TODO weight persons !

    val absenceConstraints = model.absences.getOrElse(Set()) flatMap {
      case InputAbsence(person, slots) => slots map { s => PersonAbsence(personsPerName(person), slotsPerName(s)) }
    }

    val interdictionConstraints = model.topics flatMap { umt =>
      val topic = topicsPerName(umt.name)
      umt.forbidden.getOrElse(Set()).map(personsPerName).map(PersonTopicInterdiction(_, topic))
    }

    val obligationConstraints = model.topics flatMap { umt =>
      val topic = topicsPerName(umt.name)
      umt.mandatory.getOrElse(Set()).map(personsPerName).map(PersonTopicInterdiction(_, topic))
    }

    val numberConstraints = model.topics flatMap { umt =>
      if (umt.min.isEmpty && umt.max.isEmpty) None
      else {
        val topic = topicsPerName(umt.name)
        Some(TopicNeedsNumberOfPersons(topic, umt.min, umt.max))
      }
    }

    val constraints = Set[Constraint]() ++ absenceConstraints ++ interdictionConstraints ++ obligationConstraints ++ numberConstraints

    val incompatibilityPreferences = model.incompatibilites.getOrElse(Set()) map { case InputIncompatibility(ones, others, penalty) =>
      val personOnes = ones.map(personsPerName)
      val personOthers = others.map(personsPerName)
      PersonsIncompatibilityAntiPreference(personOnes, personOthers, Score(-penalty))
    }

    val personPreferences = model.preferences.getOrElse(Set()) flatMap { case InputPreference(p, strongs, weaks) =>
      val person = personsPerName(p)
      val all = strongs.getOrElse(Set()).map((_, model.settings.strongPreference)) ++ weaks.getOrElse(Set()).map((_, model.settings.weakPreference))
      all map { case (t, reward) =>
        PersonTopicPreference(person, topicsPerName(t), reward)
      }
    }
    val preferences = Set[Preference]() ++ incompatibilityPreferences ++ personPreferences

    val problem = Problem(
      slotsPerName.values.toSet,
      topicsPerName.values.toSet,
      personsPerName.values.toSet,
      constraints,
      preferences
    )

    (problem, model.settings)
  }

  private case class InputRoot(
                                gaston: InputModel
                              )

  private case class InputModel(
                                 slots: Set[String],
                                 topics: Set[InputTopic],
                                 persons: Set[String],
                                 absences: Option[Set[InputAbsence]],
                                 incompatibilites: Option[Set[InputIncompatibility]],
                                 preferences: Option[Set[InputPreference]],
                                 settings: Settings
                               )

  private case class InputAbsence(
                                   person: String,
                                   slots: Set[String]
                                 )

  private case class InputTopic(
                                 name: String,
                                 mandatory: Option[Set[String]],
                                 forbidden: Option[Set[String]],
                                 min: Option[Int],
                                 max: Option[Int]
                               )

  private case class InputIncompatibility(
                                           ones: Set[String],
                                           others: Set[String],
                                           penalty: Double
                                         )

  private case class InputPreference(
                                      person: String,
                                      strong: Option[Set[String]],
                                      weak: Option[Set[String]]
                                    )

}