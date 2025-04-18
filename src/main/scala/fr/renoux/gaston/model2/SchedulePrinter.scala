package fr.renoux.gaston.model2

class SchedulePrinter(problem: SmallProblem) extends Printable[Schedule] {

  import problem.given

  // TODO Missing mandatory markers, plus the mandatory person should always come first

  extension (schedule: Schedule)
    override def toPrettyString: String = {
      val sb = new StringBuilder
      problem.slotsCount.foreach { sid =>
        sb.append(problem.slotsToName(sid)).append("\n")
        val assignment = schedule.slotsToAssignment(sid)
        val topics = schedule.slotsToTopics(sid)
        topics.foreach { tid =>
          sb.append("    ").append(problem.topicsToName(tid)).append(" ==>")
          val persons = assignment.topicsToPersons(tid).toSet
          if (persons.nonEmpty) {
            val personNames = persons.map(problem.personsToName.apply).toSeq.sorted
            sb.append(" ").append(personNames.mkString(", "))
            ()
          }
          sb.append("\n")
          ()
        }
      }
      val unscheduled = problem.topicsCount.range.toSet -- schedule.topicsPresent.toSet
      sb.append("Unscheduled topics: ").append(unscheduled.map(problem.topicsToName.apply).toSeq.sorted.mkString(", "))
      sb.toString
    }
}
