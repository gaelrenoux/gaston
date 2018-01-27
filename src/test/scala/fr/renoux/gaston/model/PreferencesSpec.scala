package fr.renoux.gaston.model

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.io.UdoInput
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by gael on 07/05/17.
  */
class PreferencesSpec extends FlatSpec with Matchers {
  val log = Logger[PreferencesSpec]
  implicit val settings = UdoInput.fromClassPath._2
  val SimpleTestModel = fr.renoux.gaston.SimpleTestModel(settings)

  import SimpleTestModel.Preferences._
  import SimpleTestModel.Solutions._


  behavior of "PersonsTopicPreference"
  it should "return the strong score when respected on a strong constraint" in {
    LeonardoLovesFighting.score(Perfect) should be(settings.strongPreference)
  }
  it should "return the weak score when respected on a weak constraint" in {
    LeonardoLikesMachines.score(Perfect) should be(settings.weakPreference)
  }
  it should "return zero when not respected" in {
    LeonardoLovesFighting.score(Terrible).value should be(0)
    LeonardoLikesMachines.score(Terrible).value should be(0)
  }


  behavior of "PersonsIncompatibilityAntiPreference"
  it should "return a negative score when present" in {
    EnemiesHate.score(Terrible).value should be <0.0
  }
  it should "return zero when not present" in {
    EnemiesHate.score(Perfect).value should be(0)
  }
}
