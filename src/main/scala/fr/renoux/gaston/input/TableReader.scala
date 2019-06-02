package fr.renoux.gaston.input

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.{Score, Weight}
import fr.renoux.gaston.util.CanGroupToMap._
import fr.renoux.gaston.util.StringImplicits._
import fr.renoux.gaston.util.CollectionImplicits._

import scala.collection.immutable.SortedSet
import scala.util.Try

/**
  * Reads a table of preferences to complement the input settings.
  */
class TableReader(input: InputModel) {

  val tableSettings: InputTableSettings = input.tableSettings
  val settings: InputSettings = input.settings

  private val log = Logger[TableReader]

  private implicit object InputTopicOrdering extends Ordering[InputTopic] {
    override def compare(x: InputTopic, y: InputTopic): Int = x.name.compareTo(y.name)
  }

  private implicit object InputPersonOrdering extends Ordering[InputPerson] {
    override def compare(x: InputPerson, y: InputPerson): Int = x.name.compareTo(y.name)
  }

  /** Read the table as a CSV text */
  def read(table: String): InputRoot = {
    val lines: Seq[String] = table.split("\n", -1).filter(_.nonEmpty).toSeq
    val cells: Seq[Seq[String]] = lines.map(_.split(tableSettings.separator, -1).map(_.trim).toSeq)

    log.debug(s"Cells:\n${cells.mkString("\n")}")
    val cellsPersonsRow = cells(tableSettings.personsRow)
    val cellsWithContent = cells.drop(tableSettings.wishesStartRow)

    /* Topics. Keep the order to zip with the choices later */
    val topicsSeq: Seq[InputTopic] =
      cellsWithContent.map { row =>
        val topicName: String = row(tableSettings.topicCol)
        val max: Option[Int] = row(tableSettings.maxPersonsCol).toIntOption.map(_ + tableSettings.personsCountAdd)
        val min: Option[Int] = tableSettings.minPersonsCol.map(row).flatMap(_.toIntOption).map(_ + tableSettings.personsCountAdd)
        val occurrences: Option[Int] = tableSettings.topicOccurrencesCol.map(row).flatMap(_.toIntOption)

        InputTopic(
          name = topicName,
          min = min.filterNot(_ == settings.defaultMinPersonsPerTopic),
          max = max.filterNot(_ == settings.defaultMaxPersonsPerTopic),
          occurrences = occurrences
        )
      }

    log.debug(s"Topics: $topicsSeq")
    val topicNames = topicsSeq.map(_.name)

    /* For each person's names, a list of mandatory topic's names */
    val mandatoryPersonsToTopics = cells.map { row =>
      val topicName: String = row(tableSettings.topicCol)
      val mandatoryName: String = row(tableSettings.mandatoryPersonCol)
      mandatoryName -> topicName
    }.groupToMap.mapValuesStrict(_.toSet)

    /* The persons, */
    val indexedPersonNames = cellsPersonsRow.zipWithIndex.drop(tableSettings.personsStartCol)
    val persons = indexedPersonNames.map { case (person, personColumnIndex) =>

      val personColumn = cellsWithContent.map { row =>
        if (row.length <= personColumnIndex) "" else row(personColumnIndex).trim
      }

      val mandatoryTopics = mandatoryPersonsToTopics.getOrElse(person, Set.empty)

      val forbiddenTopics = tableSettings.forbiddenPersonMarker.map { marker =>
        topicNames.zip(personColumn).filter(_._2 == marker).map(_._1).toSet
      }.getOrElse(Set.empty)

      val scoresByTopic = topicsSeq.zip(personColumn).flatMap {
        case (topic, value) => wishValueToScoreOption(value).map(topic.name -> _)
      }.toMap

      InputPerson(
        name = person,
        weight = if (mandatoryTopics.nonEmpty) tableSettings.mandatoryPersonWeight else Weight.Default,
        mandatory = mandatoryTopics,
        forbidden = forbiddenTopics,
        wishes = scoresByTopic
      )
    }

    log.debug(s"Persons: $persons")

    InputRoot(
      InputModel(
        settings = settings,
        tableSettings = tableSettings,
        slots = input.slots,
        persons = SortedSet(persons: _*),
        topics = SortedSet(topicsSeq: _*),
        constraints = InputGlobalConstraints()
      )
    )
  }

  def wishValueToScoreOption(value: String): Option[Score] =
    if (value.isEmpty) None
    else if (tableSettings.forbiddenPersonMarker.contains(value)) None
    else tableSettings.preferencesScoreMapping match {
      case None => Try(Score(value.toDouble)).toOption
      case Some(mapping) => mapping.get(value)
    }

}
