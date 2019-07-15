package fr.renoux.gaston.input

import com.typesafe.scalalogging.Logger
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import fr.renoux.gaston.model.{Score, Weight}
import fr.renoux.gaston.util.CanGroupToMap._
import fr.renoux.gaston.util.CollectionImplicits._
import fr.renoux.gaston.util.StringImplicits._

import scala.util.Try

/**
  * Reads a table of preferences to complement the input settings.
  */
class TableReader(input: InputModel) {

  val tableSettings: InputTableSettings = input.tableSettings
  val settings: InputSettings = input.settings

  private val log = Logger[TableReader]

  private implicit object InputTopicOrdering extends Ordering[InputTopic] {
    override def compare(x: InputTopic, y: InputTopic): Int = x.name.value.compareTo(y.name.value)
  }

  private implicit object InputPersonOrdering extends Ordering[InputPerson] {
    override def compare(x: InputPerson, y: InputPerson): Int = x.name.value.compareTo(y.name.value)
  }

  /** Read the table as a CSV text */
  def read(table: String): InputModel = {
    //TODO replace calls to unsafeFrom with proper validation
    val lines: Seq[String] = table.split("\n", -1).filter(_.nonEmpty).toSeq
    val cells: Seq[Seq[String]] = lines.map(_.split(tableSettings.separator, -1).map(_.trim).toSeq)

    log.debug(s"Cells:\n${cells.mkString("\n")}")
    val cellsPersonsRow = cells(tableSettings.personsRow)
    val cellsWithContent = cells.drop(tableSettings.wishesStartRow)

    /* Topics. Keep the order to zip with the choices later */
    val topicsSeq: Seq[InputTopic] =
      cellsWithContent.map { row =>
        val topicName: String = row(tableSettings.topicCol)
        val max: Option[PosInt] = row(tableSettings.maxPersonsCol).toIntOption.map(_ + tableSettings.personsCountAdd).map(PosInt.unsafeFrom)
        val min: Option[PosInt] = tableSettings.minPersonsCol.map(_.value).map(row).flatMap(_.toIntOption).map(_ + tableSettings.personsCountAdd).map(PosInt.unsafeFrom)
        val occurrences: Option[PosInt] = tableSettings.topicOccurrencesCol.map(_.value).map(row).flatMap(_.toIntOption).map(PosInt.unsafeFrom)

        InputTopic(
          name = NonEmptyString.unsafeFrom(topicName),
          min = min.filterNot(_ == settings.defaultMinPersonsPerTopic),
          max = max.filterNot(_ == settings.defaultMaxPersonsPerTopic),
          occurrences = occurrences
        )
      }

    log.debug(s"Topics: $topicsSeq")
    val topicNames = topicsSeq.map(_.name)

    /* For each person's names, a list of mandatory topic's names */
    val mandatoryPersonsToTopics: Map[NonEmptyString, Set[NonEmptyString]] = cells.map { row =>
      val topicName = NonEmptyString.unsafeFrom(row(tableSettings.topicCol))
      val mandatoryName = NonEmptyString.unsafeFrom(row(tableSettings.mandatoryPersonCol))
      mandatoryName -> topicName
    }.groupToMap.mapValuesStrict(_.toSet)

    /* The persons, */
    val indexedPersonNames = cellsPersonsRow.zipWithIndex.drop(tableSettings.personsStartCol)
    val persons = indexedPersonNames.map { case (maybePerson, personColumnIndex) =>
      val person = NonEmptyString.unsafeFrom(maybePerson)

      val personColumn = cellsWithContent.map { row =>
        if (row.length <= personColumnIndex) "" else row(personColumnIndex).trim
      }

      val mandatoryTopics = mandatoryPersonsToTopics.getOrElse(person, Set.empty)

      val forbiddenTopics = tableSettings.forbiddenPersonMarker.map { marker =>
        topicNames.zip(personColumn).filter(_._2 == marker).map(_._1).toSet
      }.getOrElse(Set.empty)

      val scoresByTopic = topicsSeq.zip(personColumn).flatMap {
        case (topic, value) => wishValueToScoreOption(value).map(topic.name.value -> _)
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

    InputModel(
      settings = settings,
      tableSettings = tableSettings,
      slots = input.slots,
      persons = persons.toList.sorted,
      topics = topicsSeq.toList.sorted,
      constraints = InputGlobalConstraints()
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
