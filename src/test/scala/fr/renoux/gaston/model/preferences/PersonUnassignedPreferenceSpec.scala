package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PersonUnassignedPreferenceSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.MinimalTestModel.Persons.*
  import fr.renoux.gaston.MinimalTestModel.Problems.*
  import fr.renoux.gaston.MinimalTestModel.Slots.*
  import fr.renoux.gaston.MinimalTestModel.Topics.*

  /** Same as the minimal problem, but with each slot on its own slot-cycle, so that we can have up to four holes. */
  given Problem = Minimal.copy(slotSequences = Seq(Seq(Morning), Seq(Afternoon), Seq(Evening), Seq(Night)))(using Minimal.counts)

  given Context = Context.Default

  private val BaseReward = Score(-10)
  private val ScalingReward = Score(-100)
  private val MultipleSameCycleReward = Score(-1000)

  private def pref(requiredCount: Int, expectedCount: Double) =
    PersonUnassignedPreference(Michelangelo, requiredCount, expectedCount, BaseReward, ScalingReward, MultipleSameCycleReward)

  /** Schedule where Michelangelo is unassigned on that count of slots (each slot being on a different slot-cycle). */
  private def withHoles(count: Int): Schedule = {
    val slotsAndTopics = Seq(Morning -> Leading, Afternoon -> Fighting, Evening -> Machines, Night -> Party)
    slotsAndTopics.zipWithIndex.map { case ((slot, topic), index) =>
      val actualTopic = if (index < count) Unassigned(slot) else topic
      Schedule.from(slot(actualTopic(Michelangelo)))
    }.reduce(_ ++ _)
  }

  private def scoreOf(preference: Preference, holes: Int): Double = preference.score(withHoles(holes)).value

  behavior of "PersonUnassignedPreference without any expected holes"

  it should "return zero when there are no holes and none are required" in {
    scoreOf(pref(0, 0), 0) should be(0.0)
  }

  it should "return the full reward for each unrequired hole" in {
    scoreOf(pref(0, 0), 1) should be(-110.0 +- 1e-6)
    scoreOf(pref(0, 0), 3) should be(-330.0 +- 1e-6)
  }

  it should "return zero for required holes" in {
    scoreOf(pref(2, 0), 2) should be(0.0)
  }

  it should "return the necessary-preference score for each missing required hole" in {
    scoreOf(pref(1, 0), 0) should be(Preference.NecessaryPreferenceScore.value)
    scoreOf(pref(3, 0), 1) should be((Preference.NecessaryPreferenceScore * 2).value)
  }

  it should "return the full reward for holes beyond the required ones" in {
    scoreOf(pref(1, 0), 3) should be(-220.0 +- 1e-6)
  }

  behavior of "PersonUnassignedPreference with expected holes"

  it should "return the base reward for fully expected holes" in {
    scoreOf(pref(0, 2.0), 1) should be(-10.0 +- 1e-6)
    scoreOf(pref(0, 2.0), 2) should be(-20.0 +- 1e-6)
  }

  it should "return the full reward for holes beyond an integer expected count" in {
    scoreOf(pref(0, 2.0), 3) should be(-130.0 +- 1e-6)
  }

  it should "return a prorated reward for the partially expected hole" in {
    scoreOf(pref(0, 1.3), 1) should be(-10.0 +- 1e-6)
    scoreOf(pref(0, 1.3), 2) should be(-90.0 +- 1e-6) // -10, then -10 - 70
    scoreOf(pref(0, 1.3), 3) should be(-200.0 +- 1e-6) // -10, then -10 - 70, then -110
  }

  it should "prorate the first hole when the expected count is below one" in {
    scoreOf(pref(0, 0.4), 1) should be(-70.0 +- 1e-6) // -10 - 60
    scoreOf(pref(0, 0.4), 2) should be(-180.0 +- 1e-6)
  }

  it should "count required holes as part of the expected ones" in {
    scoreOf(pref(1, 1.3), 1) should be(0.0)
    scoreOf(pref(1, 1.3), 2) should be(-80.0 +- 1e-6) // -10 - 70
    scoreOf(pref(1, 1.3), 3) should be(-190.0 +- 1e-6)
    scoreOf(pref(1, 2.5), 2) should be(-10.0 +- 1e-6)
    scoreOf(pref(1, 2.5), 3) should be(-70.0 +- 1e-6) // -10, then -10 - 50
  }

  it should "return the full reward beyond the required holes when they exceed the expected count" in {
    scoreOf(pref(2, 1.5), 2) should be(0.0)
    scoreOf(pref(2, 1.5), 3) should be(-110.0 +- 1e-6)
  }

  it should "still return the necessary-preference score for missing required holes" in {
    scoreOf(pref(2, 1.3), 1) should be(Preference.NecessaryPreferenceScore.value)
  }

  behavior of "PersonUnassignedPreference on multiple slots by cycle"

  it should "count the reward for multiple holes on the same slot-cycle" in {
    given Problem = Minimal // two cycles: (Morning, Afternoon) and (Evening, Night)

    val schedule = Schedule.from(Morning(Unassigned(Morning)(Michelangelo))) ++
      Schedule.from(Afternoon(Unassigned(Afternoon)(Michelangelo))) ++
      Schedule.from(Evening(Machines(Michelangelo))) ++
      Schedule.from(Night(Party(Michelangelo)))
    pref(0, 0).score(schedule).value should be(-1110.0 +- 1e-6)
  }

}
