package fr.renoux.gaston.model

import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by gael on 07/05/17.
  */
class PreferencesSpec extends FlatSpec with Matchers {

  import fr.renoux.gaston.MinimalTestModel.Persons._
  import fr.renoux.gaston.MinimalTestModel.Slots._
  import fr.renoux.gaston.MinimalTestModel.Topics._

  def scheduled(s: Slot, t: Topic, ps: Person*) = Schedule(1, Map(s -> Map(t -> ps.toSet)))


  behavior of "PersonsTopicPreference"
  val leonardoLovesFighting = PersonTopicPreference(Leonardo, Fighting, Score(42))

  it should "return the score when respected" in {
    leonardoLovesFighting.score(scheduled(Morning, Fighting, Leonardo, Raphael)
    ) should be(Score(42))
  }

  it should "return zero when not respected" in {
    leonardoLovesFighting.score(scheduled(Morning, Machines, Leonardo, Raphael)
      ++ scheduled(Morning, Fighting, Donatello, Michelangelo)
    ) should be(Score(0))
  }


  behavior of "PersonsIncompatibilityAntiPreference"
  val LeonardoHatesEnemies = PersonGroupAntiPreference(Leonardo, Set(Bebop, Rocksteady), Score(-150))

  it should "return a negative score for just one hated person" in {
    LeonardoHatesEnemies.score(scheduled(Morning, Fighting, Leonardo, Raphael, Bebop)
    ) should be(Score(-150))
  }

  it should "return a negative score multiplied by the number of hated persons in the topic" in {
    LeonardoHatesEnemies.score(scheduled(Morning, Fighting, Leonardo, Raphael, Bebop, Rocksteady)
    ) should be(Score(-150 * 2))
  }

  it should "sum negative scores on all topics" in {
    LeonardoHatesEnemies.score(scheduled(Morning, Fighting, Leonardo, Raphael, Bebop, Rocksteady)
      ++ scheduled(AfterNoon, Machines, Leonardo, Donatello)
      ++ scheduled(Evening, Party, Leonardo, Michelangelo, Rocksteady)
    ) should be(Score(-150 * 3))
  }

  it should "return zero when not present" in {
    LeonardoHatesEnemies.score(scheduled(Morning, Fighting, Leonardo, Raphael)
    ) should be(Score.Zero)
  }
}
