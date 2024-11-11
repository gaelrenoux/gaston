package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PersonGroupAntiPreferenceSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.MinimalTestModel.Persons.{*, given}
  import fr.renoux.gaston.MinimalTestModel.Problems.*
  import fr.renoux.gaston.MinimalTestModel.Slots.*
  import fr.renoux.gaston.MinimalTestModel.Topics.*

  given Problem = Minimal
  given Context = Context.Default

  def scheduled(s: Slot, t: Topic, ps: Person*): Schedule = Schedule.from(s(t(ps *)))

  behavior of "PersonGroupAntiPreference"
  val leonardoHatesEnemies: Preference = PersonGroupAntiPreference(Leonardo, Set(Bebop, Rocksteady).toArraySet, Score(-150))

  it should "return a negative score for just one hated person" in {
    leonardoHatesEnemies.score(scheduled(Morning, Fighting, Leonardo, Raphael, Bebop)
    ) should be(Score(-150))
  }

  it should "return a negative score multiplied by the number of hated persons in the topic" in {
    leonardoHatesEnemies.score(scheduled(Morning, Fighting, Leonardo, Raphael, Bebop, Rocksteady)
    ) should be(Score(-150 * 2))
  }

  it should "sum negative scores on all topics" in {
    leonardoHatesEnemies.score(scheduled(Morning, Fighting, Leonardo, Raphael, Bebop, Rocksteady)
      ++ scheduled(Afternoon, Machines, Leonardo, Donatello)
      ++ scheduled(Evening, Party, Leonardo, Michelangelo, Rocksteady)
    ) should be(Score(-150 * 3))
  }

  it should "return zero when not present" in {
    leonardoHatesEnemies.score(scheduled(Morning, Fighting, Leonardo, Raphael)
    ) should be(Score.Zero)
  }

}
