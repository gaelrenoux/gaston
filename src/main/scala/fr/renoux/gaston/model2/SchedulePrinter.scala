package fr.renoux.gaston.model2

class SchedulePrinter(problem: SmallProblem) extends Printable[Schedule] {
  import problem.given

  extension (schedule: Schedule)
    override def toPrettyString: String = {
      val sb = new StringBuilder
      problem.slotsCount.foreach { sid =>
        sb.append(problem.slotsNames(sid)).append("\n")
        problem.topicsCount.foreach { tid =>
          val persons = problem.personsCount.range.filter(schedule.content(sid, tid, _))
          if (persons.nonEmpty) {
            val personNames = persons.map(problem.personsName.apply)
            sb.append("    ").append(problem.topicsName(tid)).append(" ==> ")
            sb.append(personNames.mkString(", ")).append("\n")
            ()
          }
        }
      }
      val unscheduled = problem.topicsCount.range.toSet -- schedule.topicsPresent.toSet
      sb.append("Unscheduled topics: ").append(unscheduled.map(problem.topicsName.apply).toSeq.sorted.mkString(", "))
      sb.toString
    }
}
