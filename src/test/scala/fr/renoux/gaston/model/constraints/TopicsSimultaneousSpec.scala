package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


/** Unit test on constraints */
class TopicsSimultaneousSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.MinimalTestModel.Persons.*
  import fr.renoux.gaston.MinimalTestModel.Problems.*
  import fr.renoux.gaston.MinimalTestModel.Slots.*
  import fr.renoux.gaston.MinimalTestModel.Topics.*

  given Problem = Minimal
  given Context = Context.Default

  def scheduled(s: Slot, t: Topic, ps: Person*): Schedule = Schedule.from(s(t(ps *)))

  def scheduled(s: Slot, ts: Topic*): Schedule = Schedule.from(s(ts.map(_.apply()) *))

  behavior of "TopicsSimultaneous"
  val leadingFightingMachinesSimultaneous: Constraint = TopicsSimultaneous(Set(Leading, Fighting, Machines))

  it should "break if the topics are on different slots" in {
    leadingFightingMachinesSimultaneous.isRespected(
      scheduled(Morning, Fighting, Raphael, Leonardo) ++ scheduled(Afternoon, Machines, Donatello) ++ scheduled(Afternoon, Leading, Leonardo, Raphael)
    ) should be(false)
  }

  it should "break if one of the topics is missing" in {
    leadingFightingMachinesSimultaneous.isRespected(
      scheduled(Afternoon, Machines, Donatello) ++ scheduled(Afternoon, Leading, Leonardo, Raphael)
    ) should be(false)
  }

  it should "not break if the topics are all on the same slot" in {
    leadingFightingMachinesSimultaneous.isRespected(
      scheduled(Afternoon, Fighting, Raphael, Leonardo) ++ scheduled(Afternoon, Machines, Donatello)
        ++ scheduled(Afternoon, Leading, Leonardo, Raphael) ++ scheduled(Evening, Party, Leonardo, Raphael, Michelangelo, Donatello)
    ) should be(true)
  }

  it should "not break if the topics are not scheduled" in {
    leadingFightingMachinesSimultaneous.isRespected(
      scheduled(Afternoon, Party, Raphael, Leonardo)
    ) should be(true)
  }

}
