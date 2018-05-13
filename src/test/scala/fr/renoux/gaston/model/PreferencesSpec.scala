package fr.renoux.gaston.model

import fr.renoux.gaston.io.{InputLoader, InputSettings}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by gael on 07/05/17.
  */
class PreferencesSpec extends FlatSpec with Matchers {
  private implicit val settings: InputSettings = InputLoader.fromClassPath.forceToInput.gaston.settings
  private val SimpleTestModel = fr.renoux.gaston.SimpleTestModel(settings)

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
  it should "return a negative score multiplied by the number of hated persons" in {
    LeonardoHatesEnemies.score(Terrible).value should be(settings.strongPreference.value * (-2))
  }
  it should "return zero when not present" in {
    LeonardoHatesEnemies.score(Perfect).value should be(0)
  }
}
