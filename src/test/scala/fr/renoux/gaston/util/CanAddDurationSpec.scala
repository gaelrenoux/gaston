package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration.DurationInt

class CanAddDurationSpec extends AnyFlatSpec with Matchers {

  import CanAddDuration.given

  behavior of "+"
  it should "add a positive duration" in {
    Instant.ofEpochMilli(42) + 1.second should be(Instant.ofEpochMilli(1042))
    Instant.ofEpochMilli(42) + 1.day should be(Instant.ofEpochMilli(42 + 24 * 3600 * 1000))
  }
  it should "add a negative duration" in {
    Instant.ofEpochMilli(42) + (-1).millisecond should be(Instant.ofEpochMilli(41))
    Instant.ofEpochMilli(100042) + (-1).day should be(Instant.ofEpochMilli(100042 - 24 * 3600 * 1000))
  }
  it should "add zero" in {
    Instant.ofEpochMilli(42) + 0.millisecond should be(Instant.ofEpochMilli(42))
    Instant.ofEpochMilli(42) + 0.day should be(Instant.ofEpochMilli(42))
  }

  behavior of "-"
  it should "return a positive duration" in {
    Instant.ofEpochMilli(42) - Instant.ofEpochMilli(39) should be(3.milliseconds)
  }
  it should "return a negative duration" in {
    Instant.ofEpochMilli(42) - Instant.ofEpochMilli(50) should be((-8).milliseconds)
  }
  it should "return a zero duration" in {
    Instant.ofEpochMilli(42) - Instant.ofEpochMilli(42) should be(0.milliseconds)
  }
}
