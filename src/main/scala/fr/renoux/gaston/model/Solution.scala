package fr.renoux.gaston.model

/**
  * Created by gael on 07/05/17.
  */
case class Solution(
                     schedule: Set[(Slot, Topic, Set[Person])]
                   ) {

  lazy val personsPerSlot: Map[Slot, Set[Person]] = schedule groupBy (_._1) mapValues { x => x flatMap (_._3) }
  lazy val personsPerTopic: Map[Topic, Set[Person]] = schedule groupBy (_._2) mapValues { x => x flatMap (_._3) }

  def score(constraints: Set[_ <: Constraint]): Double =
  /* toSeq is needed to avoid removing duplicate scores ! */
    constraints.toSeq map (_.score(this)) sum

  def areMandatoryConstraintsSatisified(constraints: Set[_ <: Constraint]): Boolean = constraints forall {
    case m: MandatoryConstraint => m.isRespected(this)
    case _ => true
  }

}

object Solution {
  def apply(schedule: (Slot, Topic, Set[Person])*): Solution = new Solution(schedule.toSet)
}
