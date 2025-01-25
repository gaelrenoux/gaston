package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PersonFreeSlotPreferenceSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.MinimalTestModel.Persons.{*, given}
  import fr.renoux.gaston.MinimalTestModel.Problems.{*, given}
  import fr.renoux.gaston.MinimalTestModel.Slots.*
  import fr.renoux.gaston.MinimalTestModel.Topics.{*, given}

  given Problem = Minimal //.copy(slotSequences = Seq(Seq(Morning, Afternoon), Seq(Evening, Night)))

  given Context = Context.Default

  def scheduled(s: Slot, t: Topic, ps: Person*): Schedule = Schedule.from(s(t(ps *)))

  behavior of "PersonFreeSlotPreference"

  it should "return the reward times the count if the person has no free slot" in {
    val mikeWantsTimeOff: Preference = PersonFreeSlotPreference(Michelangelo, 2)
    mikeWantsTimeOff.score(
      scheduled(Morning, Leading, Leonardo, Michelangelo) ++
        scheduled(Afternoon, Fighting, Raphael, Michelangelo) ++
        scheduled(Evening, Machines, Donatello, Michelangelo) ++
        scheduled(Night, Party, Leonardo, Donatello, Michelangelo)
    ) should be(mikeWantsTimeOff.reward * 2)
  }

  it should "return the reward times the missing count if the person has missing free slots" in {
    val mikeWantsTimeOff: Preference = PersonFreeSlotPreference(Michelangelo, 2)
    mikeWantsTimeOff.score(
      scheduled(Morning, Leading, Leonardo, Michelangelo) ++
        scheduled(Afternoon, Fighting, Raphael, Michelangelo) ++
        scheduled(Evening, Unassigned(Evening), Donatello, Michelangelo) ++
        scheduled(Night, Party, Leonardo, Donatello, Michelangelo)
    ) should be(mikeWantsTimeOff.reward * 1)
  }

  it should "return zero if the person has exactly the right number of free slots" in {
    val mikeWantsTimeOff: Preference = PersonFreeSlotPreference(Michelangelo, 2)
    mikeWantsTimeOff.score(
      scheduled(Morning, Unassigned(Morning), Leonardo, Michelangelo) ++
        scheduled(Afternoon, Fighting, Raphael, Michelangelo) ++
        scheduled(Evening, Unassigned(Evening), Donatello, Michelangelo) ++
        scheduled(Night, Party, Leonardo, Donatello, Michelangelo)
    ) should be(Score.Zero)
  }

  it should "return zero if the person has more than the right number of free slots" in {
    val mikeWantsTimeOff: Preference = PersonFreeSlotPreference(Michelangelo, 1)
    mikeWantsTimeOff.score(
      scheduled(Morning, Unassigned(Morning), Leonardo, Michelangelo) ++
        scheduled(Afternoon, Fighting, Raphael, Michelangelo) ++
        scheduled(Evening, Unassigned(Evening), Donatello, Michelangelo) ++
        scheduled(Night, Party, Leonardo, Donatello, Michelangelo)
    ) should be(Score.Zero)
  }

  it should "count multiple free slots on the same sequence only once" in {
    val mikeWantsTimeOff: Preference = PersonFreeSlotPreference(Michelangelo, 2)
    mikeWantsTimeOff.score(
      scheduled(Morning, Leading, Leonardo, Michelangelo) ++
        scheduled(Afternoon, Fighting, Raphael, Michelangelo) ++
        scheduled(Evening, Unassigned(Evening), Donatello, Michelangelo) ++
        scheduled(Night, Unassigned(Night), Leonardo, Donatello, Michelangelo)
    ) should be(mikeWantsTimeOff.reward * 1)
  }

}
