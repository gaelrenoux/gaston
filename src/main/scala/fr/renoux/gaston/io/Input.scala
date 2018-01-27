package fr.renoux.gaston.io

import java.io.{File, PrintWriter}
import java.nio.file.Path

import fr.renoux.gaston.Settings
import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference, Preference}
import fr.renoux.gaston.model.problem.Problem
import pureconfig.error.ConfigReaderFailures
import pureconfig.{ConfigReader, ConfigWriter, loadConfig, loadConfigFromFiles}

import scala.io.Source


object Input {

  def fromClassPath: (Problem, Settings) = from(loadConfig[InputRoot])

  def fromClassPath(path: String): (Problem, Settings) = {
    val correctedPath = if (path.headOption.contains('/')) path else s"/$path"
    val absoluteFilePath = getClass.getResource(correctedPath).getPath
    val absoluteFile = new File(absoluteFilePath)
    fromPath(absoluteFile.toPath)
  }

  def fromString(config: String): (Problem, Settings) = {
    val file = File.createTempFile("gaston-input-", null)
    new PrintWriter(file) { write(config); close() }
    fromPath(file.toPath)
  }

  def fromPath(files: Path*): (Problem, Settings) = from(loadConfigFromFiles[InputRoot](files))

  /** Render a configuration into a String. */
  def render(input: InputRoot): String = ConfigWriter[InputRoot].to(input).render().split("\n") flatMap { line =>
    if (line.trim.startsWith("#")) None else Some(line)
  } mkString "\n"

  /** Load the real input from the user model */
  private def from(either: Either[ConfigReaderFailures, InputRoot]): (Problem, Settings) = {
    val model: InputModel = either match {
      case Left(failures) => throw new IllegalStateException(s"Could not parse configuration:\n${failures.toList.mkString("\n")}")
      case Right(res) => res.gaston
    }

    val slotsPerName = model.slots map { s => s -> Slot(s) } toMap //TODO other attributes ?
    val topicsPerName = model.topics map { t => t.name -> Topic(t.name) } toMap //TODO other attributes ?

    val personsPerName = model.persons map { p => p.name -> Person(p.name, p.weight.map(Weight(_)).getOrElse(Weight.Default)) } toMap

    val absenceConstraints = model.persons.collect {
      case ip if ip.absences.isDefined =>
        ip.absences.get map { s => PersonAbsence(personsPerName(ip.name), slotsPerName(s)) }
    } flatten

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

    val incompatibilityPreferences = model.persons.collect {
      case ip if ip.incompatible.isDefined =>
        val person = personsPerName(ip.name)
        val group = ip.incompatible.get.map(personsPerName)
        PersonGroupAntiPreference(person, group, model.settings.incompatibilityPreference)
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

  /* TODO try without the Options, using default values */

  case class InputRoot(
                        gaston: InputModel
                      )

  case class InputModel(
                         settings: Settings,
                         slots: Set[String],
                         persons: Set[InputPerson],
                         topics: Set[InputTopic],
                         preferences: Option[Set[InputPreference]]
                       )

  case class InputPreference(
                              person: String,
                              strong: Option[Set[String]],
                              weak: Option[Set[String]]
                            )

  case class InputPerson(
                          name: String,
                          weight: Option[Double],
                          incompatible: Option[Set[String]],
                          absences: Option[Set[String]]
                        )

  case class InputTopic(
                         name: String,
                         mandatory: Option[Set[String]],
                         forbidden: Option[Set[String]],
                         min: Option[Int],
                         max: Option[Int]
                       )

}