package fr.renoux.gaston.model

import cats.implicits._
import fr.renoux.gaston.util.Context

/** Parse a schedule from various formats */
final class ScheduleParser(implicit problem: Problem, context: Context) {

  type ErrorOr[A] = Either[String, A]

  private val firstLineRegex = """Schedule seed: (\d+)""".r

  /** Parses a schedule from the result of [[Schedule.toFormattedString]]. */
  def parseFormattedString(str: String): Either[String, Schedule] = {
    val lines = str.linesIterator.toList
    for {
      headerResult <- readHeader(lines)
      (seed, linesLeft) = headerResult
      result <- readAllSlotSchedules(0, linesLeft)
      (slotSchedules, _) = result
    } yield Schedule(seed, slotSchedules.map(ss => ss.slot -> ss).toMap)
  }

  private def readHeader(lines: List[String]): Either[String, (Long, List[String])] = lines match {
    case firstLineRegex(seed) :: lines =>
      try Right((seed.toLong, lines))
      catch {
        case _: NumberFormatException => Left(s"Seed in first line was not a number: $seed")
      }
    case h :: _ => Left(s"Unrecognized first line: $h")
    case Nil => Left(s"Empty schedule")
  }

  private[model] def readAllSlotSchedules(
      returnIndentation: Int,
      lines: List[String],
      acc: List[SlotSchedule] = Nil
  ): ErrorOr[(Set[SlotSchedule], List[String])] =
    readSlotSchedule(returnIndentation, lines).flatMap {
      case (None, lines) => Right((acc.toSet, lines))
      case (Some(slotSchedule), linesLeft) => readAllSlotSchedules(returnIndentation, linesLeft, slotSchedule :: acc)
    }

  private[model] def readSlotSchedule(returnIndentation: Int, lines: List[String]): ErrorOr[(Option[SlotSchedule], List[String])] = lines match {
    case Nil => Right((None, Nil))
    case slotLine :: otherLines =>
      val indentation = countIndentation(slotLine)
      if (indentation <= returnIndentation) Right((None, lines))
      else {
        val slotName = slotLine.trim
        for {
          slot <- problem.slotsList.find(_.name == slotName).toRight(s"Unknown slot: $slotName")
          readResult <- readAllRecords(slot, indentation, otherLines)
          (records, linesLeft) = readResult
          slotSchedule = SlotSchedule(slot, records.map(r => r.topic -> r).toMap)
        } yield (Some(slotSchedule), linesLeft)
      }
  }

  private[model] def readAllRecords(slot: Slot, returnIndentation: Int, lines: List[String], acc: List[Record] = Nil): ErrorOr[(Set[Record], List[String])] =
    lines match {
      case Nil => Right((acc.toSet, Nil))
      case _ if countIndentation(lines.head) <= returnIndentation => Right((acc.toSet, lines))
      case recordLine :: linesLeft =>
        parseRecord(slot, recordLine).flatMap { record =>
          readAllRecords(slot, returnIndentation, linesLeft, record :: acc)
        }
    }

  private[model] def parseRecord(slot: Slot, line: String): ErrorOr[Record] = line.trim.split(Record.FormattedTopicPersonsSeparator) match {
    case Array(topicString, personsString) =>
      val topicName = topicString.trim
      val personNames = personsString.split(Record.FormattedPersonsSeparator).map(_.trim)
      val topic = problem.topicsList.find(_.name == topicName).toRight(s"Unknown topic: $topicName")
      lazy val persons = personNames.toList.traverse { name => problem.personsList.find(_.name == name).toRight(s"Unknown person: $name") }
      for {t <- topic; ps <- persons} yield Record(slot, t, ps.toSet)
    case _ => Left(s"Not a valid topic line: $line")
  }

  private def countIndentation(line: String): Int = line.takeWhile(_.isWhitespace).length

}
