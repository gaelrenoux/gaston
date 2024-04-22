package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


// scalastyle:off magic.number
class OptionImplicitsSpec extends AnyFlatSpec with Matchers {

  import OptionImplicits._

  "optional" should "work" in {
    "Something".optional(Option.empty[String])(_ + _) should be("Something")
    "Something".optional(Some("More"))(_ + _) should be("SomethingMore")
  }

}
