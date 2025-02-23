package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase

class CountTest extends TestBase {

  "exists" - {
    "returns the correct value in the nominal cases" in {
      val c: Count[PersonId] = 8
      c.exists(_.value == 5) should be(true)
      c.exists(_.value == 11) should be(false)
    }
    "returns the correct value on an empty count" in {
      val c: Count[PersonId] = 0
      c.exists(_.value == 5) should be(false)
    }
    "short-circuits as soon as the answer is known" in {
      val c: Count[PersonId] = 8
      var acc = 0
      c.exists { id =>
        acc += 1
        id.value == 5
      } should be(true)
      acc should be(6)
    }
    "does the whole collection if required" in {
      val c: Count[PersonId] = 8
      var acc = 0
      c.exists { id =>
        acc += 1
        id.value == 9
      } should be(false)
      acc should be(8)
    }
  }

  "forall" - {
    "returns the correct value in the nominal cases" in {
      val c: Count[PersonId] = 8
      c.forall(_.value < 9) should be(true)
      c.forall(_.value < 6) should be(false)
    }
    "returns the correct value on an empty count" in {
      val c: Count[PersonId] = 0
      c.forall(_.value == 9) should be(true)
    }
    "short-circuits as soon as the answer is known" in {
      val c: Count[PersonId] = 8
      var acc = 0
      c.forall { id =>
        acc += 1
        id.value < 5
      } should be(false)
      acc should be(6)
    }
    "does the whole collection if required" in {
      val c: Count[PersonId] = 8
      var acc = 0
      c.forall { id =>
        acc += 1
        id.value < 9
      } should be(true)
      acc should be(8)
    }
  }
}
