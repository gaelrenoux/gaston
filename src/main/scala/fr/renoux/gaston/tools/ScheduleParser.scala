package fr.renoux.gaston.tools

import cats.implicits.*
import fr.renoux.gaston.model.{Person, Problem, Record, Schedule, Slot, SlotSchedule}
import fr.renoux.gaston.util.Context

/** Parse a schedule from various formats */
final class ScheduleParser(using problem: Problem, context: Context) {

  type ErrorOr[A] = Either[String, A]

  private val seedLineRegex = """Schedule-chain seed: (\d+)""".r

  /** Parses a schedule from the result of [[Schedule.toFormattedString]]. */
  def parseFormattedString(str: String): Either[String, Schedule] = {
    val lines = str.trim.linesIterator.toList
    for {
      afterHeader <- readUselessLine(lines, "Schedule:")
      scheduleResult <- readAllSlotSchedules(0, afterHeader)
      (slotSchedules, afterSchedule) = scheduleResult
      afterUnscheduled <- readUselessLine(afterSchedule, "Unscheduled topics:")
      seedResult <- readSeed(afterUnscheduled)
      (globalSeed, _) = seedResult
    } yield Schedule(globalSeed, slotSchedules.map(ss => ss.slot -> ss).toMap)
  }

  private def readUselessLine(lines: List[String], prefix: String): Either[String, List[String]] = lines match {
    case line :: lines if line.trim.startsWith(prefix) => Right(lines)
    case h :: _ => Left(s"Unrecognized line not starting with [$prefix]: $h")
    case Nil => Left("Empty schedule")
  }

  private def readSeed(lines: List[String]): Either[String, (Long, List[String])] = lines match {
    case seedLineRegex(globalSeed) :: lines =>
      try Right((globalSeed.toLong, lines))
      catch {
        case _: NumberFormatException => Left(s"Global-seed was not a number: $globalSeed")
      }
    case h :: _ => Left(s"Unrecognized seed line: $h")
    case Nil => Left("Empty schedule")
  }

  private def readAllSlotSchedules(
      returnIndentation: Int,
      lines: List[String],
      acc: List[SlotSchedule] = Nil
  ): ErrorOr[(Set[SlotSchedule], List[String])] =
    readSlotSchedule(returnIndentation, lines).flatMap {
      case (None, lines) => Right((acc.toSet, lines))
      case (Some(slotSchedule), linesLeft) => readAllSlotSchedules(returnIndentation, linesLeft, slotSchedule :: acc)
    }

  private[tools] def readSlotSchedule(returnIndentation: Int, lines: List[String]): ErrorOr[(Option[SlotSchedule], List[String])] = lines match {
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

  private[tools] def readAllRecords(slot: Slot, returnIndentation: Int, lines: List[String], acc: List[Record] = Nil): ErrorOr[(Set[Record], List[String])] =
    lines match {
      case Nil => Right((acc.toSet, Nil))
      case _ if countIndentation(lines.head) <= returnIndentation => Right((acc.toSet, lines))
      case recordLine :: linesLeft =>
        parseRecord(slot, recordLine).flatMap { record =>
          readAllRecords(slot, returnIndentation, linesLeft, record :: acc)
        }
    }

  private[tools] def parseRecord(slot: Slot, line: String): ErrorOr[Record] = line.trim.split(Record.FormattedTopicPersonsSeparator) match {
    case Array(topicString, personsString) =>
      val topicName = topicString.trim
      val personNames = personsString.split(Record.FormattedPersonsSeparator).map(_.trim.replace(s" (${Record.MandatoryMarker})", ""))
      val topic = problem.topicsList.find(_.name == topicName).toRight(s"Unknown topic: $topicName")
      lazy val persons = personNames.toList.traverse { name => problem.personsList.find(_.name == name).toRight(s"Unknown person: $name") }
      for {t <- topic; ps <- persons} yield Record(slot, t, ps.toSet)
    case Array(topicString) => // no person on this topic
      val topicName = topicString.trim
      val topic = problem.topicsList.find(_.name == topicName).toRight(s"Unknown topic: $topicName")
      topic.map { t => Record(slot, t, Set.empty[Person]) }
    case _ => Left(s"Not a valid topic line: $line")
  }

  private def countIndentation(line: String): Int = line.takeWhile(_.isWhitespace).length

}
