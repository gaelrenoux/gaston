package fr.renoux.gaston.model

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.io.Definitions
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by gael on 07/05/17.
  */
class ConstraintsSpec extends FlatSpec with Matchers {
  val log = Logger[ConstraintsSpec]

  import fr.renoux.gaston.SimpleTestModel.Constraints._
  import fr.renoux.gaston.SimpleTestModel.Preferences._
  import fr.renoux.gaston.SimpleTestModel.Solutions._


  behavior of "TopicNeedsNumberOfPersons"
  it should "return 1 when broken once" in {
    FightingNeedsTwoToFourPersons.countBroken(Terrible) should be(1)
  }
  it should "return zero when respected" in {
    FightingNeedsTwoToFourPersons.countBroken(Perfect) should be(0)
  }

  behavior of "PersonTopicObligation"
  it should "return 1 when broken once" in {
    LeonardoLeads.countBroken(Terrible) should be(1)
  }
  it should "return zero when respected" in {
    LeonardoLeads.countBroken(Perfect) should be(0)
  }

  behavior of "PersonTopicInterdiction"
  it should "return 1 when broken once" in {
    LeonardoDoesNotParty.countBroken(Terrible) should be(1)
  }
  it should "return zero when respected" in {
    LeonardoDoesNotParty.countBroken(Perfect) should be(0)
  }

  behavior of "PersonAbsence"
  it should "return 1 when broken once" in {
    LeonardoNotInTheNight.countBroken(Terrible) should be(1)
  }
  it should "return zero when respected" in {
    LeonardoNotInTheNight.countBroken(Perfect) should be(0)
  }

  behavior of "Preferences"
  it should "return weight when satisfied once" in {
    LeonardoLovesFighting.score(Perfect) should be(Score(Definitions.StrongPreference.value))
  }
  it should "return zero when not satisfied" in {
    LeonardoLovesFighting.score(Terrible) should be(Score.Zero)
  }

}
