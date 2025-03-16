package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.{ArraySet, Context}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TopicsExclusiveForSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.MinimalTestModel.Persons.{*, given}
  import fr.renoux.gaston.MinimalTestModel.Problems.*
  import fr.renoux.gaston.MinimalTestModel.Slots.*
  import fr.renoux.gaston.MinimalTestModel.Topics.{*, given}

  given Problem = Minimal

  given Context = Context.Default

  def scheduled(s: Slot, t: Topic, ps: Person*): Schedule = Schedule.from(s(t(ps *)))

  behavior of "TopicsExclusiveFor"
  val mikeAndRaphaelCannotLeadAndParty: Preference = TopicsExclusiveFor(Set(Leading, Party).toArraySet, Set(Raphael, Michelangelo).toArraySet)

  it should "return the reward if a person does both" in {
    mikeAndRaphaelCannotLeadAndParty.score(scheduled(Morning, Leading, Donatello, Michelangelo) ++
      scheduled(Afternoon, Party, Raphael, Michelangelo)
    ) should be(mikeAndRaphaelCannotLeadAndParty.reward)
  }

  it should "return the reward if a person does both with a non-concerned person" in {
    mikeAndRaphaelCannotLeadAndParty.score(scheduled(Morning, Leading, Donatello, Michelangelo, Leonardo) ++
      scheduled(Afternoon, Party, Raphael, Michelangelo, Leonardo)
    ) should be(mikeAndRaphaelCannotLeadAndParty.reward)
  }

  it should "return zero if no one does both" in {
    mikeAndRaphaelCannotLeadAndParty.score(scheduled(Morning, Leading, Donatello, Michelangelo) ++
      scheduled(Afternoon, Party, Raphael)
    ) should be(Score.Zero)
  }

  it should "return zero if non-concerned persons do both" in {
    mikeAndRaphaelCannotLeadAndParty.score(scheduled(Morning, Leading, Donatello, Leonardo) ++
      scheduled(Afternoon, Party, Raphael, Leonardo, Michelangelo)
    ) should be(Score.Zero)
  }

  it should "return the reward if a person does two out of three" in {
    val mikeCannotLeadAndPartyAndMachines = TopicsExclusiveFor(Set(Leading, Party, Machines).toArraySet, Set(Michelangelo).toArraySet)
    mikeCannotLeadAndPartyAndMachines.score(scheduled(Morning, Leading, Leonardo, Michelangelo) ++
      scheduled(Afternoon, Party, Raphael, Michelangelo) ++ scheduled(Evening, Machines, Donatello)
    ) should be(mikeCannotLeadAndPartyAndMachines.reward)
  }

  it should "return twice the reward if a person does three out of three" in {
    val mikeCannotLeadAndPartyAndMachines = TopicsExclusiveFor(Set(Leading, Party, Machines).toArraySet, Set(Michelangelo).toArraySet)
    mikeCannotLeadAndPartyAndMachines.score(
      scheduled(Morning, Leading, Leonardo, Michelangelo) ++
        scheduled(Afternoon, Party, Raphael, Michelangelo) ++
        scheduled(Evening, Machines, Donatello, Michelangelo)
    ) should be(mikeCannotLeadAndPartyAndMachines.reward * 2)
  }

}
