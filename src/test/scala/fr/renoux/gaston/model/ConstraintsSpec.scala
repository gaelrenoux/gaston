package fr.renoux.gaston.model

import fr.renoux.gaston.engine.Context
import fr.renoux.gaston.model.constraints._
import org.scalatest.{FlatSpec, Matchers}

/** Unit test on constraints */
class ConstraintsSpec extends FlatSpec with Matchers {

  import fr.renoux.gaston.MinimalTestModel.Persons._
  import fr.renoux.gaston.MinimalTestModel.Problems._
  import fr.renoux.gaston.MinimalTestModel.Slots._
  import fr.renoux.gaston.MinimalTestModel.Topics._

  private implicit val problem: Problem = Minimal
  private implicit val context: Context = Context.Default

  def scheduled(s: Slot, t: Topic, ps: Person*): Schedule = Schedule(s(t(ps: _*)))

  def scheduled(s: Slot, ts: Topic*): Schedule = Schedule(s(ts.map(_.apply()): _*))

  behavior of "TopicsSimultaneous"
  val leadingFightingMachinesSimultaneous = TopicsSimultaneous(Set(Leading, Fighting, Machines))

  it should "break if the topics are on various slots" in {
    leadingFightingMachinesSimultaneous.isRespected(scheduled(Morning, Fighting, Raphael, Leonardo) ++
      scheduled(AfterNoon, Machines, Donatello) ++ scheduled(AfterNoon, Leading, Leonardo, Raphael)
    ) should be(false)
  }

  it should "break if one of the topics is missing" in {
    leadingFightingMachinesSimultaneous.isRespected(scheduled(AfterNoon, Machines, Donatello) ++
      scheduled(AfterNoon, Leading, Leonardo, Raphael)
    ) should be(false)
  }

  it should "not break if the topics are all on the same slot" in {
    leadingFightingMachinesSimultaneous.isRespected(scheduled(AfterNoon, Fighting, Raphael, Leonardo) ++
      scheduled(AfterNoon, Machines, Donatello) ++ scheduled(AfterNoon, Leading, Leonardo, Raphael)
      ++ scheduled(Evening, Party, Leonardo, Raphael, Michelangelo, Donatello)
    ) should be(true)
  }

  behavior of "TopicForcedSlot"
  val partyMustBeEveningOrNight = TopicForcedSlot(Party, Set(Evening, Night))

  it should "break if the topic is on the wrong slot" in {
    partyMustBeEveningOrNight.isRespected(scheduled(AfterNoon, Party, Michelangelo, Donatello, Raphael)
    ) should be(false)
  }

  it should "not break if the topic is on the correct slot" in {
    partyMustBeEveningOrNight.isRespected(scheduled(Evening, Party, Michelangelo, Donatello, Raphael)
    ) should be(true)
  }

  it should "not break if the topic is missing" in {
    partyMustBeEveningOrNight.isRespected(scheduled(AfterNoon, Fighting, Michelangelo, Donatello, Raphael)
    ) should be(true)
  }

}
