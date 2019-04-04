package fr.renoux.gaston.input

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.Weight
import fr.renoux.gaston.util.CollectionImplicits._
import fr.renoux.gaston.util.StringImplicits._
import fr.renoux.gaston.util.CanGroupToMap._

/**
  * Some stuff initialized from the normal JSON input, the rest is read from the table.
  */
class UdoConTableReader(udoSettings: InputUdoSettings, settings: InputSettings) {

  import UdoConTableReader._

  private val log = Logger[UdoConTableReader]

  def read(table: String): InputRoot = {
    val cells: Seq[Seq[String]] = table.split("\n", -1).filterNot(_.isEmpty).map(_.split("\t", -1).map(_.trim).toSeq).toSeq

    log.debug(s"Cells:\n${cells.mkString("\n")}")
    val cellsFirstRow = cells.head
    val cellsWithoutFirstRow = cells.tail
    //TODO better parameterize the line for starting topics

    val slots: Set[InputSlot] = Set[InputSlot](InputSlot("Day1", maxTopics = Some(4)), InputSlot("Day2")) //sample Slots

    /* Keep the order to zip with the choices later */
    val topicsSeq: Seq[InputTopic] =
      cellsWithoutFirstRow.map { row =>
        val topicName: String = row(udoSettings.topicsIndex)
        val max: Option[Int] = row(udoSettings.maxPlayersIndex).toIntOption.map(_ + 1) //add the GM
      val min: Option[Int] = udoSettings.minPlayersIndex.map(row).flatMap(_.toIntOption).map(_ + 1) //add the GM

        InputTopic(
          name = topicName,
          min = min.filterNot(_ == settings.defaultMinPersonsPerTopic),
          max = max.filterNot(_ == settings.defaultMaxPersonsPerTopic)
        )
      }

    log.debug(s"Topics: $topicsSeq")

    val namedPersons: Seq[InputPerson] = cellsFirstRow.drop(udoSettings.personsStartingIndex).map(InputPerson(_))

    val personChoices: Map[InputPerson, Map[TopicStatus, Set[InputTopic]]] =
      namedPersons.zipWithIndex.map { case (person, ix) =>
        val personColumnIndex = ix + udoSettings.personsStartingIndex
        log.debug(s"Choices for $person.name at index $personColumnIndex")

        val personColumn = cellsWithoutFirstRow.map { row =>
          if (row.length <= personColumnIndex) "" else row(personColumnIndex).trim
        }

        val statusTopics: Seq[(TopicStatus, InputTopic)] = topicsSeq.zip(personColumn).flatMap {
          case (topic, "MJ") => Some((Mandatory, topic))
          case (topic, "0") => Some((Forbidden, topic))
          case (topic, "1") => Some((Weak, topic))
          case (topic, "2") => Some((Strong, topic))
          case (_, "") => None
          case (topic, x) => log.warn(s"Unknown entry $x for person ${person.name} and topic ${topic.name}"); None
        }

        person -> statusTopics.groupToMap.mapValuesStrict(_.toSet).withDefaultValue(Set())
      }.toMap

    val persons = personChoices.map { case (person, choices) =>
      val wishes = choices.collect {
        case (Strong, ts) => InputPersonWishes(udoSettings.strongWishValue, ts.map(_.name))
        case (Weak, ts) => InputPersonWishes(udoSettings.weakWishValue, ts.map(_.name))
      }.toSet

      person.copy(
        weight = if (choices.contains(Mandatory)) udoSettings.gamemasterWeight else Weight.Default,
        mandatory = choices(Mandatory).map(_.name),
        forbidden = choices(Forbidden).map(_.name),
        wishes = wishes
      )
    }

    log.debug(s"Persons: $persons")

    InputRoot(
      InputModel(
        settings = settings,
        udoSettings = Some(udoSettings),
        slots = slots,
        persons = persons.toSet,
        topics = topicsSeq.toSet
      )
    )
  }

}

object UdoConTableReader {

  sealed trait TopicStatus

  case object Mandatory extends TopicStatus

  case object Strong extends TopicStatus

  case object Weak extends TopicStatus

  case object Forbidden extends TopicStatus

}