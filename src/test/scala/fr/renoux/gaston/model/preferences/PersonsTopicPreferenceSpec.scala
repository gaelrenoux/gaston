package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class PersonsTopicPreferenceSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.MinimalTestModel.Persons._
  import fr.renoux.gaston.MinimalTestModel.Problems._
  import fr.renoux.gaston.MinimalTestModel.Slots._
  import fr.renoux.gaston.MinimalTestModel.Topics._

  private implicit val problem: Problem = Minimal
  private implicit val context: Context = Context.Default

  def scheduled(s: Slot, t: Topic, ps: Person*): Schedule = Schedule.from(s(t(ps: _*)))


  behavior of "PersonsTopicPreference"
  val leonardoLovesFighting: Preference = PersonTopicPreference(Leonardo, Fighting, FlatScore(42))

  it should "return the score when respected" in {
    leonardoLovesFighting.score(scheduled(Morning, Fighting, Leonardo, Raphael)
    ) should be(FlatScore(42))
  }

  it should "return zero when not respected" in {
    leonardoLovesFighting.score(scheduled(Morning, Machines, Leonardo, Raphael)
      ++ scheduled(Morning, Fighting, Donatello, Michelangelo)
    ) should be(FlatScore(0))
  }

}
