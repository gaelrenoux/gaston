package fr.renoux.gaston.model

import fr.renoux.gaston.model.constraints._
import org.scalatest.{FlatSpec, Matchers}

/** Unit test on constraints */
class ConstraintsSpec extends FlatSpec with Matchers {

  import fr.renoux.gaston.MinimalTestModel.Persons._
  import fr.renoux.gaston.MinimalTestModel.Slots._
  import fr.renoux.gaston.MinimalTestModel.Topics._

  def scheduled(s: Slot, t: Topic, ps: Person*) = Schedule(1, Map(s -> Map(t -> ps.toSet)))


  behavior of "TopicNeedsNumberOfPersons"
  val fightingNeedsTwoToFourPersons = TopicNeedsNumberOfPersons(Fighting, min = 2, max = 4)

  it should "break on not enough persons" in {
    fightingNeedsTwoToFourPersons.isRespected(scheduled(Morning, Fighting, Leonardo)
    ) should be(false)
  }

  it should "break on too many persons" in {
    fightingNeedsTwoToFourPersons.isRespected(
      scheduled(Morning, Fighting, Leonardo, Donatello, Raphael, Michelangelo, Bebop)
    ) should be(false)
  }

  it should "not break inside the limits" in {
    fightingNeedsTwoToFourPersons.isRespected(scheduled(Morning, Fighting, Leonardo, Donatello, Raphael)
    ) should be(true)
  }

  it should "not break on the superior limit" in {
    fightingNeedsTwoToFourPersons.isRespected(scheduled(Morning, Fighting, Leonardo, Donatello, Raphael, Michelangelo)
    ) should be(true)
  }

  it should "not break on the inferior limit" in {
    fightingNeedsTwoToFourPersons.isRespected(scheduled(Morning, Fighting, Leonardo, Raphael)
    ) should be(true)
  }


  behavior of "PersonTopicObligation"
  val leonardoLeads = PersonTopicObligation(Leonardo, Leading)

  it should "break if the person is missing" in {
    leonardoLeads.isRespected(scheduled(Morning, Leading, Michelangelo, Donatello)
    ) should be(false)
  }

  it should "break if everyone is missing" in {
    leonardoLeads.isRespected(scheduled(Morning, Leading)
    ) should be(false)
  }

  it should "not break if the person is present" in {
    leonardoLeads.isRespected(scheduled(Morning, Leading, Leonardo, Donatello)
    ) should be(true)
  }

  it should "not break if the person is the only one present" in {
    leonardoLeads.isRespected(scheduled(Morning, Leading, Leonardo)
    ) should be(true)
  }


  behavior of "PersonTopicInterdiction"
  val leonardoDoesNotParty = PersonTopicInterdiction(Leonardo, Party)

  it should "break if the person is present" in {
    leonardoDoesNotParty.isRespected(scheduled(Morning, Party, Leonardo)
    ) should be(false)
  }

  it should "not break if the person is absent" in {
    leonardoDoesNotParty.isRespected(scheduled(Morning, Party, Michelangelo, Donatello)
    ) should be(true)
  }


  behavior of "PersonAbsence"
  val michelangeloNotInTheMorning = PersonAbsence(Michelangelo, Morning)

  it should "break if the person is present" in {
    michelangeloNotInTheMorning.isRespected(scheduled(Morning, Fighting, Michelangelo, Leonardo)
    ) should be(false)
  }

  it should "not break if the person is absent" in {
    michelangeloNotInTheMorning.isRespected(scheduled(Morning, Fighting, Raphael, Leonardo) ++
      scheduled(AfterNoon, Machines, Michelangelo, Donatello)
    ) should be(true)
  }


  behavior of "SimultaneousTopics"
  val leadingFightingMachinesSimultaneous = SimultaneousTopics(Set(Leading, Fighting, Machines))

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

}
