package fr.renoux.gaston.input

import java.io.File

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.model._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InputLoaderSpec extends AnyFlatSpec with Matchers {

  "Loading from default" should "load all defaults values" in {
    val input = InputLoader.fromDefault.force
    input.settings.incompatibilityAntiPreference.value should be(Score(-1000))
    input.persons.size should be(0)
  }

  "Loading from the classpath" should "load from default values and the given file" in {
    val input = InputLoader.fromClassPath("named-configuration.conf").force
    input.settings.incompatibilityAntiPreference.value should be(Score(-1042))
    input.slots.flatten.size should be(0)
    input.persons.size should be(1)
  }

  "Loading from a file" should "load from default values and the given file" in {
    val stringPath = getClass.getResource("/named-configuration.conf").getPath
    val path = new File(stringPath).toPath
    val input = InputLoader.fromPath(path).force
    input.settings.incompatibilityAntiPreference.value should be(Score(-1042))
    input.slots.flatten.size should be(0)
    input.persons.size should be(1)
  }

}
