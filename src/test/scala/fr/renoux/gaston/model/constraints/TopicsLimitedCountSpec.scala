package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class TopicsLimitedCountSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.MinimalTestModel.Persons.*
  import fr.renoux.gaston.MinimalTestModel.Problems.*
  import fr.renoux.gaston.MinimalTestModel.Slots.*
  import fr.renoux.gaston.MinimalTestModel.Topics.{*, given}

  given Problem = Minimal
  given Context = Context.Default

  def scheduled(s: Slot, t: Topic, ps: Person*): Schedule = Schedule.from(s(t(ps *)))

  behavior of "TopicsLimitedCount"
  val atMostTwoOfLeadingFightingMachines: Constraint = TopicsLimitedCount(Set(Leading, Fighting, Machines).toArraySet, 2)

  it should "not break if fewer topics than the count are scheduled" in {
    atMostTwoOfLeadingFightingMachines.isRespected(
      scheduled(Morning, Fighting, Raphael) ++ scheduled(Afternoon, Party, Leonardo)
    ) should be(true)
  }

  it should "not break if exactly the count of topics are scheduled" in {
    atMostTwoOfLeadingFightingMachines.isRespected(
      scheduled(Morning, Fighting, Raphael) ++ scheduled(Afternoon, Machines, Donatello)
    ) should be(true)
  }

  it should "break if more topics than the count are scheduled, even on different slots" in {
    atMostTwoOfLeadingFightingMachines.isRespected(
      scheduled(Morning, Fighting, Raphael) ++ scheduled(Afternoon, Machines, Donatello) ++ scheduled(Evening, Leading, Leonardo)
    ) should be(false)
  }

  it should "not count topics outside of the constraint" in {
    atMostTwoOfLeadingFightingMachines.isRespected(
      scheduled(Morning, Fighting, Raphael) ++ scheduled(Afternoon, Machines, Donatello) ++ scheduled(Evening, Party, Leonardo)
    ) should be(true)
  }

  it should "not be hard-coded, and be checkable on partial schedules" in {
    atMostTwoOfLeadingFightingMachines.isHardCoded should be(false)
    atMostTwoOfLeadingFightingMachines.isApplicableToPartialSchedule should be(true)
  }

  it should "refuse a count that is not lower than the number of topics" in {
    an[AssertionError] should be thrownBy TopicsLimitedCount(Set(Leading, Fighting).toArraySet, 2)
  }

}
