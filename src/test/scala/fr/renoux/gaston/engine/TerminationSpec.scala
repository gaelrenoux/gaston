package fr.renoux.gaston.engine

import fr.renoux.gaston.model.Score
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class TerminationSpec extends AnyFlatSpec with Matchers {

  behavior of "Perpetual"

  it should "never terminate" in {
    Termination.Perpetual.checkScore(Score(Double.MaxValue)) should be(false)
    Termination.Perpetual.checkCount(Long.MaxValue) should be(false)
    Termination.Perpetual.checkTimeout(Instant.now()) should be(false)
    Termination.Perpetual.checkTimeout() should be(false)
  }

  behavior of "checkScore"

  it should "terminate once the target score is reached" in {
    val termination = Termination(score = Some(Score(10)))
    termination.checkScore(Score(9.99)) should be(false)
    termination.checkScore(Score(10)) should be(true)
    termination.checkScore(Score(11)) should be(true)
  }

  behavior of "checkCount"

  it should "terminate once the count is reached" in {
    val termination = Termination(count = Some(100))
    termination.checkCount(99) should be(false)
    termination.checkCount(100) should be(true)
    termination.checkCount(101) should be(true)
  }

  behavior of "reduceCount"

  it should "reduce the count" in {
    Termination(count = Some(100)).reduceCount(30) should be(Termination(count = Some(70)))
  }

  it should "do nothing without a count" in {
    Termination(score = Some(Score(10))).reduceCount(30) should be(Termination(score = Some(Score(10))))
  }

  behavior of "intCount"

  it should "convert the count to an Int when possible" in {
    Termination(count = Some(100)).intCount should be(Some(100))
    Termination(count = Some(Int.MaxValue.toLong + 1)).intCount should be(None)
    Termination().intCount should be(None)
  }

  behavior of "checkTimeout"

  it should "terminate once the timeout is reached" in {
    val termination = Termination(timeout = Some(Instant.ofEpochMilli(1000)))
    termination.checkTimeout(Instant.ofEpochMilli(999)) should be(false)
    termination.checkTimeout(Instant.ofEpochMilli(1000)) should be(true)
    termination.checkTimeout(999L) should be(false)
    termination.checkTimeout(1000L) should be(true)
    termination.checkTimeout() should be(true)
  }

  it should "not terminate before a timeout in the future" in {
    Termination(timeout = Some(Instant.now().plusSeconds(3600))).checkTimeout() should be(false)
  }

}
