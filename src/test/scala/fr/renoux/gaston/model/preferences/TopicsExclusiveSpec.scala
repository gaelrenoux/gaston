package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.{ArraySet, Context}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TopicsExclusiveSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.MinimalTestModel.Persons.{*, given}
  import fr.renoux.gaston.MinimalTestModel.Problems.*
  import fr.renoux.gaston.MinimalTestModel.Slots.*
  import fr.renoux.gaston.MinimalTestModel.Topics.{*, given}

  given Problem = Minimal
  given Context = Context.Default

  def scheduled(s: Slot, t: Topic, ps: Person*): Schedule = Schedule.from(s(t(ps *)))

  behavior of "TopicsExclusive"
  val cannotLeadAndPartyExceptLeo: Preference = TopicsExclusive(Set(Leading, Party).toArraySet, Set(Leonardo).toArraySet)

  it should "return the reward if a person does both" in {
    cannotLeadAndPartyExceptLeo.score(scheduled(Morning, Leading, Donatello, Michelangelo) ++
      scheduled(Afternoon, Party, Raphael, Michelangelo)
    ) should be(cannotLeadAndPartyExceptLeo.reward)
  }

  it should "return the reward if a person does both with another exempted person" in {
    cannotLeadAndPartyExceptLeo.score(scheduled(Morning, Leading, Donatello, Michelangelo, Leonardo) ++
      scheduled(Afternoon, Party, Raphael, Michelangelo, Leonardo)
    ) should be(cannotLeadAndPartyExceptLeo.reward)
  }

  it should "return zero if no one does both" in {
    cannotLeadAndPartyExceptLeo.score(scheduled(Morning, Leading, Donatello, Michelangelo) ++
      scheduled(Afternoon, Party, Raphael)
    ) should be(Score.Zero)
  }

  it should "return zero if an exempted person does both" in {
    cannotLeadAndPartyExceptLeo.score(scheduled(Morning, Leading, Donatello, Leonardo) ++
      scheduled(Afternoon, Party, Raphael, Leonardo, Michelangelo)
    ) should be(Score.Zero)
  }

  it should "break if a person does two out of three" in {
    TopicsExclusive(Set(Leading, Party, Machines).toArraySet, ArraySet.empty[Person]).score(scheduled(Morning, Leading, Leonardo, Michelangelo) ++
      scheduled(Afternoon, Party, Raphael, Michelangelo) ++ scheduled(Evening, Machines, Donatello)
    ) should be(cannotLeadAndPartyExceptLeo.reward)
  }

}
