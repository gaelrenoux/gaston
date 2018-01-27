package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.{Schedule, Score}

/**
  * Abstract class for preferences providing a basis for how to score things. Scored is an object that can be checked
  * individually, obtained by aggregating some stuff from the schedule. */
abstract class AbstractPreference[Scored] extends Preference {

  def score(schedule: Schedule): Score = elementsScored(schedule).map(score).sum

  /** On a schedule, what are the elements to look at to now if it's working. */
  protected def elementsScored(schedule: Schedule): Iterable[Scored]

  /** How to score a preference on one those elements. Does NOT apply the weight ! */
  protected def score(checked: Scored): Score
}
