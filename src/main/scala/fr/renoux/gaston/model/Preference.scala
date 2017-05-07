package fr.renoux.gaston.model

/**
  * Created by gael on 07/05/17.
  */
abstract class Preference extends Constraint {
  override val isMandatory: Boolean = true
  def value: Preference.Strength
}

object Preference {
  sealed abstract class Strength(val score: Double)
  object Strong extends Strength(ScoringConstants.StrongPreference)
  object Weak extends Strength(ScoringConstants.WeakPreference)
}