package fr.renoux.gaston.model.constraints

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.{Constraint, Person, Schedule, Topic}

import scala.collection.mutable

/** No person (outside of the persons explicitely exempted from this rule) can be on more than one of the topics inside that list. */
case class TopicsExclusive(topics: Set[Topic], exemptions: Set[Person] = Set()) extends Constraint {

  private val log = Logger[Constraint]

  override def isRespected(schedule: Schedule): Boolean = {
    val groups = schedule.personsPerTopic.filterKeys(topics.contains).values.view.map(_.filterNot(exemptions))
    val acc = mutable.Set[Person]()
    val bool = groups.forall { ps =>
      if (ps.exists(acc)) false
      else {
        acc ++= ps
        true
      }
    }

    if (!bool) log.debug(s"Constraint $this is broken on $schedule")
    bool
  }

}

