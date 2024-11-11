package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PersonTopicPreferenceSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.MinimalTestModel.Persons.*
  import fr.renoux.gaston.MinimalTestModel.Problems.*
  import fr.renoux.gaston.MinimalTestModel.Slots.*
  import fr.renoux.gaston.MinimalTestModel.Topics.*

  given Problem = Minimal
  given Context = Context.Default

  def scheduled(s: Slot, t: Topic, ps: Person*): Schedule = Schedule.from(s(t(ps *)))


  behavior of "PersonsTopicPreference"
  val leonardoLovesFighting: Preference = PersonTopicPreference(Leonardo, Fighting, Score(42))

  it should "return the score when respected" in {
    leonardoLovesFighting.score(scheduled(Morning, Fighting, Leonardo, Raphael)
    ) should be(Score(42))
  }

  it should "return zero when not respected" in {
    leonardoLovesFighting.score(scheduled(Morning, Machines, Leonardo, Raphael)
      ++ scheduled(Morning, Fighting, Donatello, Michelangelo)
    ) should be(Score(0))
  }

}
