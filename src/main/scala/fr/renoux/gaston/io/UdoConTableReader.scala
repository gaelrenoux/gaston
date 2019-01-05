package fr.renoux.gaston.io

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.CollectionImplicits._
import fr.renoux.gaston.util.StringImplicits._

/**
  * Some stuff initialized from the normal JSON input, the rest is read from the table.
  */
class UdoConTableReader(udoSettings: InputUdoSettings, settings: InputSettings) {

  private val log = Logger[UdoConTableReader]

  def read(table: String): InputRoot = {
    val cells: Seq[Seq[String]] = table.split("\n", -1).filterNot(_.isEmpty) map (_.split("\t", -1).map(_.trim).toSeq) toSeq

    log.debug(s"Cells:\n${cells.mkString("\n")}")
    val cellsFirstLine = cells.head
    val cellsWithoutFirstLine = cells.tail

    val slots = Set("D1-afternoon", "D1-evening", "D2-afternoon", "D2-evening", "D3-afternoon")

    val personsWithoutWeight: Seq[InputPerson] = cellsFirstLine drop udoSettings.personsStartingIndex map { n =>
      InputPerson(n, Some(Weight.Default.value), incompatible = Some(Set()), absences = Some(Set()))
    }

    val topicsWithoutPersons = cellsWithoutFirstLine map { line =>
      (
        line(udoSettings.topicsIndex),
        line(udoSettings.maxPlayersIndex),
        udoSettings.minPlayersIndex.map(line).getOrElse("")
      )
    } map { case (topicName, max, min) =>
      InputTopic(
        name = topicName,
        mandatory = Some(Set()),
        forbidden = Some(Set()),
        linked = None, //TODO should be some Set
        min = min.toIntOption.map(_ + 1), //add the GM
        max = max.toIntOption.map(_ + 1), //add the GM
        forcedSlot = None
      )
    }

    val choices = personsWithoutWeight.zipWithIndex flatMap { case (person, ix) =>
      log.debug(s"Choices for $person.name at index $ix")
      val personColumnIndex = ix + udoSettings.personsStartingIndex
      val personColumn = cellsWithoutFirstLine map { line =>
        if (line.lengthCompare(personColumnIndex) <= 0) "" else line(personColumnIndex).trim
      }
      topicsWithoutPersons zip personColumn flatMap {
        case (topic, "MJ") => Some(('gamemaster, person.name, topic.name))
        case (topic, "0") => Some(('forbidden, person.name, topic.name))
        case (topic, "1") => Some(('weak, person.name, topic.name))
        case (topic, "2") => Some(('strong, person.name, topic.name))
        case (_, "") => None
        case (topic, x) => log.warn(s"Unknown entry $x for person ${person.name} and topic ${topic.name}"); None
      }
    }

    val persons = choices.foldLeft(personsWithoutWeight) {
      case (currentPersons, ('gamemaster, personName, _)) => currentPersons replace {
        case person if person.name == personName => person.copy(weight = Some(udoSettings.gamemasterWeight.value))
      }
      case (currentPersons, _) => currentPersons
    } toSet

    log.debug(s"Persons: $persons")

    val topics = choices.foldLeft(topicsWithoutPersons) {
      case (currentTopics, ('gamemaster, personName, topicName)) => currentTopics replace {
        case topic if topic.name == topicName => topic.copy(mandatory = Some(topic.mandatory.getOrElse(Set()) + personName))
      }
      case (currentTopics, ('forbidden, personName, topicName)) => currentTopics replace {
        case topic if topic.name == topicName => topic.copy(forbidden = Some(topic.forbidden.getOrElse(Set()) + personName))
      }
      case (currentTopics, _) => currentTopics
    } toSet

    log.debug(s"Topics: $topics")

    val emptyPreferences = personsWithoutWeight map { p => InputPreference(p.name, Some(Set()), Some(Set())) }

    val preferences = choices.foldLeft(emptyPreferences) {
      case (currentPreferences, ('weak, personName, topicName)) => currentPreferences replace {
        case pref if pref.person == personName => pref.copy(weak = Some(pref.weak.getOrElse(Set()) + topicName))
      }
      case (currentPreferences, ('strong, personName, topicName)) => currentPreferences replace {
        case pref if pref.person == personName => pref.copy(strong = Some(pref.strong.getOrElse(Set()) + topicName))
      }
      case (currentPreferences, _) => currentPreferences
    } toSet

    log.debug(s"Preferences: $preferences")

    InputRoot(
      InputModel(
        settings = settings,
        udoSettings = Some(udoSettings),
        slots = slots,
        persons = persons,
        topics = topics,
        preferences = Some(preferences)
      )
    )
  }

}