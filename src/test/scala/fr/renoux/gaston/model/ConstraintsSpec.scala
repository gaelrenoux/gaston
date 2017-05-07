package fr.renoux.gaston.model

import com.typesafe.scalalogging.Logger
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by gael on 07/05/17.
  */
class ConstraintsSpec extends FlatSpec with Matchers {
  val log = Logger[ConstraintsSpec]

  import TestModel.Constraints._
  import TestModel.Solutions._


  behavior of "Obligation"
  it should "return the broken mandatory constraint score when broken" in {
    LeonardoLeads.score(Terrible) should be(ScoringConstants.BrokenMandatoryConstraint)
    LeonardoInTheMorning.score(Terrible) should be(ScoringConstants.BrokenMandatoryConstraint)
  }
  it should "return zero when respected" in {
    LeonardoLeads.score(Perfect) should be(ScoringConstants.Zero)
    LeonardoInTheMorning.score(Perfect) should be(ScoringConstants.Zero)
  }

  behavior of "Interdiction"
  it should "return the broken mandatory constraint score when broken" in {
    LeonardoDoesNotParty.score(Terrible) should be(ScoringConstants.BrokenMandatoryConstraint)
    LeonardoNotInTheNight.score(Terrible) should be(ScoringConstants.BrokenMandatoryConstraint)
  }
  it should "return zero when respected" in {
    LeonardoDoesNotParty.score(Perfect) should be(ScoringConstants.Zero)
    LeonardoNotInTheNight.score(Perfect) should be(ScoringConstants.Zero)
  }

  behavior of "Preferences"
  it should "return the strong score when it is strong and satisfied" in {
    LeonardoLovesFighting.score(Perfect) should be(ScoringConstants.StrongPreference)
  }
  it should "return the weak score when it is weak and satisfied" in {
    LeonardoLikesMachines.score(Perfect) should be(ScoringConstants.WeakPreference)
  }
  it should "return zero when not respected" in {
    LeonardoLovesFighting.score(Terrible) should be(ScoringConstants.Zero)
    LeonardoLikesMachines.score(Terrible) should be(ScoringConstants.Zero)
  }

  behavior of "PersonHasSomethingForAllSlots"
  it should "return the broken mandatory constraint score when broken" in {
    LeonardoHasSomethingForAllSlots.score(Terrible) should be(ScoringConstants.BrokenMandatoryConstraint)
  }
  it should "return zero when respected" in {
    log.debug("personsPerSlot " + Perfect.personsPerSlot)
    LeonardoHasSomethingForAllSlots.score(Perfect) should be(ScoringConstants.Zero)
  }

  behavior of "TopicNeedsNumberOfPersons"
  it should "return the broken mandatory constraint score when broken" in {
    FightingNeedsTwoToFourPersons.score(Terrible) should be(ScoringConstants.BrokenMandatoryConstraint)
  }
  it should "return zero when respected" in {
    FightingNeedsTwoToFourPersons.score(Perfect) should be(ScoringConstants.Zero)
  }

}
