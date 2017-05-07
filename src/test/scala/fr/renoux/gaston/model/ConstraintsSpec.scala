package fr.renoux.gaston.model

import com.typesafe.scalalogging.Logger
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by gael on 07/05/17.
  */
class ConstraintsSpec extends FlatSpec with Matchers {
  val log = Logger[ConstraintsSpec]

  import TestModel._


  behavior of "Obligation"
  it should "return the broken mandatory constraint score when broken" in {
    Constraints.LeonardoLeads.evaluate(Solutions.Terrible) should be(ScoringConstants.BrokenMandatoryConstraint)
    Constraints.LeonardoInTheMorning.evaluate(Solutions.Terrible) should be(ScoringConstants.BrokenMandatoryConstraint)
  }
  it should "return zero when respected" in {
    Constraints.LeonardoLeads.evaluate(Solutions.Perfect) should be(ScoringConstants.Zero)
    Constraints.LeonardoInTheMorning.evaluate(Solutions.Perfect) should be(ScoringConstants.Zero)
  }

  behavior of "Interdiction"
  it should "return the broken mandatory constraint score when broken" in {
    Constraints.LeonardoDoesNotParty.evaluate(Solutions.Terrible) should be(ScoringConstants.BrokenMandatoryConstraint)
    Constraints.LeonardoNotInTheNight.evaluate(Solutions.Terrible) should be(ScoringConstants.BrokenMandatoryConstraint)
  }
  it should "return zero when respected" in {
    Constraints.LeonardoDoesNotParty.evaluate(Solutions.Perfect) should be(ScoringConstants.Zero)
    Constraints.LeonardoNotInTheNight.evaluate(Solutions.Perfect) should be(ScoringConstants.Zero)
  }

  behavior of "Preferences"
  it should "return the strong score when it is strong and satisfied" in {
    Constraints.LeonardoLovesFighting.evaluate(Solutions.Perfect) should be(ScoringConstants.StrongPreference)
  }
  it should "return the weak score when it is weak and satisfied" in {
    Constraints.LeonardoLikesMachines.evaluate(Solutions.Perfect) should be(ScoringConstants.WeakPreference)
  }
  it should "return zero when not respected" in {
    Constraints.LeonardoLovesFighting.evaluate(Solutions.Terrible) should be(ScoringConstants.Zero)
    Constraints.LeonardoLikesMachines.evaluate(Solutions.Terrible) should be(ScoringConstants.Zero)
  }

  behavior of "PersonHasSomethingForAllSlots"
  it should "return the broken mandatory constraint score when broken" in {
    Constraints.LeonardoHasSomethingForAllSlots.evaluate(Solutions.Terrible) should be(ScoringConstants.BrokenMandatoryConstraint)
  }
  it should "return zero when respected" in {
    log.debug("personsPerSlot " + Solutions.Perfect.personsPerSlot)
    Constraints.LeonardoHasSomethingForAllSlots.evaluate(Solutions.Perfect) should be(ScoringConstants.Zero)
  }

  behavior of "TopicNeedsNumberOfPersons"
  it should "return the broken mandatory constraint score when broken" in {
    Constraints.FightingNeedsTwoToFourPersons.evaluate(Solutions.Terrible) should be(ScoringConstants.BrokenMandatoryConstraint)
  }
  it should "return zero when respected" in {
    Constraints.FightingNeedsTwoToFourPersons.evaluate(Solutions.Perfect) should be(ScoringConstants.Zero)
  }

}
