package fr.renoux.gaston.model

/**
  * Created by gael on 07/05/17.
  */
trait Constraint {
  def isMandatory: Boolean

  def evaluate(solution: Solution): Double
}
