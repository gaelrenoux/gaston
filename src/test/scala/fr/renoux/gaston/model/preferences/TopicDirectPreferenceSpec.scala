package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TopicDirectPreferenceSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.MinimalTestModel.Persons.*
  import fr.renoux.gaston.MinimalTestModel.Problems.*
  import fr.renoux.gaston.MinimalTestModel.Slots.*
  import fr.renoux.gaston.MinimalTestModel.Topics.*

  given Problem = Minimal
  given Context = Context.Default

  def scheduled(s: Slot, t: Topic, ps: Person*): Schedule = Schedule.from(s(t(ps *)))


  behavior of "TopicDirectPreferenceSpec"
  val fightingIsGood: Preference = TopicDirectPreference(Fighting, Score(42))

  it should "return the score when respected" in {
    fightingIsGood.score(scheduled(Morning, Fighting, Leonardo, Raphael)
    ) should be(Score(42))
  }

  it should "return the score when respected (even with no one on it)" in {
    fightingIsGood.score(scheduled(Morning, Fighting)
    ) should be(Score(42))
  }

  it should "return zero when not respected" in {
    fightingIsGood.score(scheduled(Morning, Machines, Leonardo, Raphael)
      ++ scheduled(Morning, Leading, Donatello, Michelangelo)
    ) should be(Score(0))
  }

}
