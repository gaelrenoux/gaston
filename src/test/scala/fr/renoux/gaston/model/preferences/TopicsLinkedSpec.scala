package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TopicsLinkedSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.MinimalTestModel.Persons.*
  import fr.renoux.gaston.MinimalTestModel.Problems.*
  import fr.renoux.gaston.MinimalTestModel.Slots.*
  import fr.renoux.gaston.MinimalTestModel.Topics.{*, given}

  given Problem = Minimal
  given Context = Context.Default

  def scheduled(s: Slot, t: Topic, ps: Person*): Schedule = Schedule.from(s(t(ps *)))

  behavior of "TopicsLinked"
  val mustFightAndPartyAndMachines: Preference = TopicsLinked(Set(Fighting, Party, Machines).toArraySet)

  it should "return zero if a person does both" in {
    mustFightAndPartyAndMachines.score(scheduled(Morning, Fighting, Donatello, Michelangelo) ++
      scheduled(Afternoon, Party, Donatello, Michelangelo) ++ scheduled(Evening, Machines, Donatello, Michelangelo)
    ) should be(Score.Zero)
  }

  it should "return the reward if a persons misses one" in {
    mustFightAndPartyAndMachines.score(scheduled(Morning, Fighting, Donatello, Michelangelo) ++
      scheduled(Afternoon, Party, Donatello, Michelangelo) ++ scheduled(Evening, Machines, Donatello)
    ) should be(mustFightAndPartyAndMachines.reward)
  }

  it should "count two rewards if a person misses two" in {
    mustFightAndPartyAndMachines.score(scheduled(Morning, Fighting, Donatello) ++
      scheduled(Afternoon, Party, Donatello, Michelangelo) ++ scheduled(Evening, Machines, Donatello)
    ) should be(mustFightAndPartyAndMachines.reward * 2)
  }

  it should "count two rewards if two persons miss one" in {
    mustFightAndPartyAndMachines.score(scheduled(Morning, Fighting, Donatello, Michelangelo) ++
      scheduled(Afternoon, Party, Donatello, Michelangelo) ++ scheduled(Evening, Machines)
    ) should be(mustFightAndPartyAndMachines.reward * 2)
  }

  it should "count three rewards a person miss one and another misses two" in {
    mustFightAndPartyAndMachines.score(scheduled(Morning, Fighting, Donatello) ++
      scheduled(Afternoon, Party, Donatello, Michelangelo) ++ scheduled(Evening, Machines)
    ) should be(mustFightAndPartyAndMachines.reward * 3)
  }

}
