package fr.renoux.gaston.input

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.{Score, Weight}
import fr.renoux.gaston.util.*
import fr.renoux.gaston.util.CanGroupToMap.given
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

import scala.util.Try

/**
 * Reads a table of preferences to complement the input settings.
 */
final class TableReader(input: InputModel) {

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
  def read(table: String): InputModel = {
    // TODO replace calls to unsafeFrom with proper validation
    val lines: IndexedSeq[String] = table.split("\n", -1).filter(_.trim.nonEmpty).toIndexedSeq
    val cells: IndexedSeq[IndexedSeq[String]] = lines.map(_.split(tableSettings.separator, -1).map(_.trim).toIndexedSeq)

    log.debug(s"Cells:\n${cells.mkString("\n")}")
    val cellsPersonsRow = cells(tableSettings.personsRow)
    val cellsWithContent = cells.drop(tableSettings.wishesStartRow)

    /* Topics. Keep the order to zip with the choices later */
    val topicsSeq: Seq[InputTopic] =
      cellsWithContent.map { row =>
        val topicName: String = row(tableSettings.topicCol)
        val max: Option[PosInt] =
          row(tableSettings.maxPersonsCol).toIntOption.map(_ + tableSettings.personsCountAdd).map(_.refineUnsafe[Positive]: PosInt)
        // TODO Submit ticket to Iron for this? It's annoying having to declare the type. It might be solved by something like _.refineUnsafeTo[PosInt] instead.
        val min: Option[PosInt] =
          tableSettings.minPersonsCol.map(row).flatMap(_.toIntOption).map(_ + tableSettings.personsCountAdd).map(_.refineUnsafe[Positive]: PosInt)
        val occurrences: Option[PosInt] =
          tableSettings.topicOccurrencesCol.map(row).flatMap(_.toIntOption).map(_.refineUnsafe[Positive]: PosInt)

        InputTopic(
          name = topicName.refineUnsafe[Not[Empty]],
          min = min.filterNot(_ == settings.defaultMinPersonsPerTopic),
          max = max.filterNot(_ == settings.defaultMaxPersonsPerTopic),
          occurrences = occurrences
        )
      }

    log.debug(s"Topics: $topicsSeq")
    val topicNames = topicsSeq.map(_.name)

    /* For each person's names, a list of mandatory topic's names */
    val mandatoryPersonsToTopics: Map[NonEmptyString, Set[NonEmptyString]] = cellsWithContent.map { row =>
      val topicName = row(tableSettings.topicCol).refineOption[Not[Empty]]
        .getOrElse(throw new IllegalArgumentException("Topic column is empty"))
      val mandatoryName = row(tableSettings.mandatoryPersonCol).refineOption[Not[Empty]]
        .getOrElse(throw new IllegalArgumentException("Mandatory person column is empty"))
      mandatoryName -> topicName
    }.groupToMap.mapValuesStrict(_.toSet)

    /* The persons, */
    val indexedPersonNames = cellsPersonsRow.zipWithIndex.drop(tableSettings.personsStartCol)
    val persons = indexedPersonNames.map { case (maybePerson, personColumnIndex) =>
      val person = maybePerson.refineUnsafe[Not[Empty]]

      val personColumn = cellsWithContent.map { row =>
        if (row.length <= personColumnIndex) "" else row(personColumnIndex).trim
      }

      val mandatoryTopics = mandatoryPersonsToTopics.getOrElse(person, Set.empty)

      val forbiddenTopics = tableSettings.forbiddenPersonMarker.map { marker =>
        topicNames.zip(personColumn).filter(_._2 == marker).map(_._1).toSet
      }.getOrElse(Set.empty)

      val scoresByTopic: Map[String, Score] = topicsSeq.zip(personColumn).flatMap {
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
