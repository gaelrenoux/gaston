package fr.renoux.gaston.model

/**
  * Created by gael on 07/05/17.
  */
abstract class Obligation extends Constraint {
  override val isMandatory: Boolean = true
}
