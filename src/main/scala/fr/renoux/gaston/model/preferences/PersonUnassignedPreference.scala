package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.*
import fr.renoux.gaston.model.Person.Id

// TODO maybe rename "unassigned slot" to "hole" ? Shorter, makes the code easier to read.

/**
  * The entire preference for unassigned slots (holes) for a given person. This takes into account:
  * - that the number of required is reached
  * - rewarding the non-required holes
  *
  * Note that checking this person is not on two unassigned slots in the same row is done through the exclusive topics
  * preference, not here. TODO maybe move it here since we're counting this kind of stuff already ?
  *
  * It's not a Preference.Personal, because those are always record-level and this one is by necessity global-level.
  * @param requiredCount How many free slots has the person required. Note that there's a maximum of one free slot per cycle.
  * @param expectedCount How many unassigned slots should they expect, given their other choices and the settings.
  * @param baseReward Base score for any unrequired unassigned slot. Negative.
  * @param scalingReward Reward added to the base reward, scaled by comparing the expected count to the actual count of unassigned slots.
  */
final case class PersonUnassignedPreference(person: Person, requiredCount: Int, expectedCount: Double, baseReward: Score, scalingReward: Score)
    extends Preference.GlobalLevel {
  assert(requiredCount >= 0, s"$this should have a non-negative requiredCount")
  assert(expectedCount >= 0, s"$this should have a non-negative expectedCount")
  assert(baseReward.value <= 0, s"$this should have a non-posivite baseReward")
  assert(scalingReward.value < 0, s"$this should have a negative scalingReward")

  override def reward: Score = throw new UnsupportedOperationException // no immediately obvious reward to expose

  /** Reward for a completely unexpected hole */
  private val fullReward = baseReward + scalingReward

  /** How many hole are fully expected (their reward reduced to the base-reward) */
  private val fullyExpectedCount = math.floor(expectedCount).toInt

  /** How many holes are still fully-unexpected after removing unrequired holes */
  private val fullyExpectedUnrequiredCount = math.max(0, fullyExpectedCount - requiredCount)

  /** The reward for the partially-expected hole, ie the decimal part of the expectedCount.
    *
    * Note: we always consider that there is a partially-expected hole. If expectedCount is already an integer, then
    * the "partially-expected" hole has an expectation of zero (and therefore a full reward). */
  private val rewardForUnrequiredPartiallyExpectedHole = {
    val partiallyExpectedHoleExpectation = expectedCount - fullyExpectedCount // how much the partially-expected empty-slot is expected
    baseReward + (scalingReward * (1.0 - partiallyExpectedHoleExpectation))
  }

  /** True if all expected hole are required, including the partially-expected one. */
  private val areAllExpectedHolesRequired = requiredCount >= expectedCount

  override def scoreSchedule(schedule: Schedule): Score = {
    val count = schedule.countSlotCyclesWithUnassignedByPerson(person)
    val unrequiredCount = count - requiredCount

    if (unrequiredCount < 0) Preference.NecessaryPreferenceScore * (-unrequiredCount) // if <, then necessary preference unsatisfied, and if =, then zero (perfect case)

    else if (areAllExpectedHolesRequired) { // all unrequired holes take the full reward, because they are fully unexpected
      fullReward * unrequiredCount

    } else {
      // unrequiredCount > 0
      // expectedCount > requiredCount (and therefore fullyExpectedCount >= requiredCount)

      // First, we count the unrequired, fully-expected holes
      val unrequiredFullyExpectedCount = math.min(unrequiredCount, fullyExpectedUnrequiredCount) // is >= 0
      val unrequiredFullyExpectedReward = baseReward * unrequiredFullyExpectedCount

      // Second, we count the unrequired, partially-expected hole (if it exists)
      val existsUnrequiredPartiallyExpectedHole = unrequiredCount > unrequiredFullyExpectedCount
      val unrequiredPartiallyExpectedReward = if (existsUnrequiredPartiallyExpectedHole) rewardForUnrequiredPartiallyExpectedHole else Score.Zero

      // Finally, we count the unrequired, fully-unexpected holes (if they exist)
      val unrequiredFullyUnexpectedCount = math.max(unrequiredCount - unrequiredFullyExpectedCount - 1, 0)
      val unrequiredFullyUnexpectedReward = fullReward * unrequiredFullyUnexpectedCount

      unrequiredFullyExpectedReward + unrequiredPartiallyExpectedReward + unrequiredFullyUnexpectedReward
    }
  }

  override lazy val toLongString: String = s"PersonUnassignedPreference(${person.toShortString}, $requiredCount, $expectedCount, $baseReward, $scalingReward)"

  override lazy val toAbstract: (String, Id, Int, Double, Double, Double) =
    ("PersonUnassignedPreference", person.id, requiredCount, expectedCount, baseReward.value, scalingReward.value)
}

