package fr.renoux.gaston.io

import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference, Preference}
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Slot, Topic, Weight}

import scalaz.ValidationNel
import scalaz.syntax.ToValidationOps

object PureConfigTranscriber extends ToValidationOps {

    /** Load the real input from the user model */
  def transcribe(inputRoot: InputRoot): ValidationNel[String, Problem] = {
    val input: InputModel = inputRoot.gaston

    val slotsPerName = input.slots map { s => s -> Slot(s) } toMap
    val topicsPerName = input.topics map { t => t.name -> Topic(t.name) } toMap
    val personsPerName = input.persons map { p => p.name -> Person(p.name, p.weight.map(Weight(_)).getOrElse(Weight.Default)) } toMap
    val maps = Maps(slotsPerName, topicsPerName, personsPerName)

    val absenceConstraints = getAbsenceConstraints(input, maps)

    val interdictionConstraints = getInterdictionConstraints(input, maps)

    val obligationConstraints = getObligationConstraints(input, maps)

    val numberConstraints = getNumberConstraints(input, maps)

    val constraints = Set[Constraint]() ++ absenceConstraints ++ interdictionConstraints ++ obligationConstraints ++ numberConstraints

    val incompatibilityPreferences = getIncompatibilityPreferences(input, maps)

    val personPreferences = getPersonPreferences(input, maps)

    val preferences = Set[Preference]() ++ incompatibilityPreferences ++ personPreferences

    Problem(
      slotsPerName.values.toSet,
      topicsPerName.values.toSet,
      personsPerName.values.toSet,
      constraints,
      preferences
    ).success
  }

  private def getAbsenceConstraints(input: InputModel, maps: Maps) = {
    input.persons.collect {
      case ip if ip.absences.isDefined =>
        ip.absences.get map { s => PersonAbsence(maps.personsPerName(ip.name), maps.slotsPerName(s)) }
    } flatten
  }

  private def getInterdictionConstraints(input: InputModel, maps: Maps) = {
    input.topics flatMap { umt =>
      val topic = maps.topicsPerName(umt.name)
      umt.forbidden.getOrElse(Set()).map(maps.personsPerName).map(PersonTopicInterdiction(_, topic))
    }
  }

  private def getObligationConstraints(input: InputModel, maps: Maps) = {
    input.topics flatMap { umt =>
      val topic = maps.topicsPerName(umt.name)
      umt.mandatory.getOrElse(Set()).map(maps.personsPerName).map(PersonTopicObligation(_, topic))
    }
  }

  private def getNumberConstraints(input: InputModel, maps: Maps) = {
    input.topics flatMap { umt =>
      if (umt.min.isEmpty && umt.max.isEmpty) None
      else {
        val topic = maps.topicsPerName(umt.name)
        Some(TopicNeedsNumberOfPersons(topic, umt.min, umt.max))
      }
    }
  }

  private def getIncompatibilityPreferences(input: InputModel, maps: Maps) = {
    input.persons.collect {
      case ip if ip.incompatible.isDefined =>
        val person = maps.personsPerName(ip.name)
        val group = ip.incompatible.get.map(maps.personsPerName)
        PersonGroupAntiPreference(person, group, input.settings.incompatibilityAntiPreference)
    }
  }

  private def getPersonPreferences(input: InputModel, maps: Maps) = {
    input.preferences.getOrElse(Set()) flatMap { case InputPreference(p, strongs, weaks) =>
      val person = maps.personsPerName(p)
      val all = strongs.getOrElse(Set()).map((_, input.settings.strongPreference)) ++ weaks.getOrElse(Set()).map((_, input.settings.weakPreference))
      all map { case (t, reward) =>
        PersonTopicPreference(person, maps.topicsPerName(t), reward)
      }
    }
  }

  private case class Maps(
                           slotsPerName: Map[String, Slot],
                           topicsPerName: Map[String, Topic],
                           personsPerName: Map[String, Person]
                         )

}
