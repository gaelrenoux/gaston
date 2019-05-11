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
  import fr.renoux.gaston.MinimalTestModel.Problems._

  implicit val problem: Problem = Minimal

  def scheduled(s: Slot, t: Topic, ps: Person*): Schedule = Schedule(s(t(ps: _*)))


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

  /*
  behavior of "TopicsExclusive"
  val cannotLeadAndPartyExceptLeo = TopicsExclusive(Set(Leading, Party), Set(Leonardo))

  it should "break if a person does both on different slots" in {
    cannotLeadAndPartyExceptLeo.isRespected(scheduled(Morning, Leading, Donatello, Michelangelo) ++
      scheduled(AfterNoon, Party, Raphael, Michelangelo)
    ) should be(false)
  }

  it should "break if a person does both on different slots with another exempted person" in {
    cannotLeadAndPartyExceptLeo.isRespected(scheduled(Morning, Leading, Donatello, Michelangelo, Leonardo) ++
      scheduled(AfterNoon, Party, Raphael, Michelangelo, Leonardo)
    ) should be(false)
  }

  it should "not break if no one does both" in {
    cannotLeadAndPartyExceptLeo.isRespected(scheduled(Morning, Leading, Donatello, Michelangelo) ++
      scheduled(AfterNoon, Party, Raphael)
    ) should be(true)
  }

  it should "not break if an exempted person does both" in {
    cannotLeadAndPartyExceptLeo.isRespected(scheduled(Morning, Leading, Donatello, Leonardo) ++
      scheduled(AfterNoon, Party, Raphael, Leonardo, Michelangelo)
    ) should be(true)
  }

  it should "break if a person does two out of three" in {
    TopicsExclusive(Set(Leading, Party, Machines)).isRespected(scheduled(Morning, Leading, Leonardo, Michelangelo) ++
      scheduled(AfterNoon, Party, Raphael, Michelangelo) ++ scheduled(Evening, Machines, Donatello)
    ) should be(false)
  }
   */

}
