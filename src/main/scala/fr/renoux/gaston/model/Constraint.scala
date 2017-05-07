package fr.renoux.gaston.model

/**
  * Created by gael on 07/05/17.
  */
trait Constraint {
  def isMandatory: Boolean
  def score(solution: Solution): Double
}

trait MandatoryConstraint extends Constraint {
  override val isMandatory = true
  def isRespected(solution: Solution): Boolean
}