package fr.renoux.gaston.model

/**
  * Created by gael on 07/05/17.
  */
case class PersonSlotPreference(
                                 person: Person,
                                 slot: Slot,
                                 value: Preference.Strength) extends Preference {

  override def score(solution: Solution): Double = solution.personsPerSlot map {
    case (s, persons) if s == slot && persons(person) => value.score
    case _  => ScoringConstants.Zero
  } sum
}