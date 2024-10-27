package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PersonPersonPreferenceSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.MinimalTestModel.Persons.*
  import fr.renoux.gaston.MinimalTestModel.Problems.*
  import fr.renoux.gaston.MinimalTestModel.Slots.*
  import fr.renoux.gaston.MinimalTestModel.Topics.*

  private implicit val problem: Problem = Minimal
  private implicit val context: Context = Context.Default

  def scheduled(s: Slot, t: Topic, ps: Person*): Schedule = Schedule.from(s(t(ps *)))


  behavior of "PersonsPersonPreference"
  val leonardoLovesRaphael: Preference = PersonPersonPreference(Leonardo, Raphael, Score(42))

  it should "return the score when respected" in {
    leonardoLovesRaphael.score(scheduled(Morning, Fighting, Leonardo, Raphael)
    ) should be(Score(42))
  }

  it should "return zero when not respected" in {
    leonardoLovesRaphael.score(scheduled(Morning, Fighting, Leonardo, Donatello)
      ++ scheduled(Morning, Machines, Raphael, Michelangelo)
    ) should be(Score(0))
  }

}
