package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


// scalastyle:off magic.number
class StringImplicitsSpec extends AnyFlatSpec with Matchers {

  import StringImplicits._

  "replaceRec" should "work" in {
    "Hello www www whatever www".replace("ww", "-w") shouldNot be("Hello --w --w whatever --w")
    "Hello www www whatever www".replaceRec("ww", "-w") should be("Hello --w --w whatever --w")
  }

}
