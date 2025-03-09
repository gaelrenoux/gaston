package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase

import scala.util.Random

class CountTest extends TestBase {

  "basics" - {
    "operators" in {
      val a: Count[PersonId] = 3
      val b: Count[PersonId] = 5
      a > b should be(false)
      b > a should be(true)
      a > a should be(false)

      a < b should be(true)
      b < a should be(false)
      a < a should be(false)

      a >= b should be(false)
      b >= a should be(true)
      a >= a should be(true)

      a <= b should be(true)
      b <= a should be(false)
      a <= a should be(true)

      a + b should be(8: Count[PersonId])
      b - a should be(2: Count[PersonId])
    }

    "random" in {
      val c: Count[PersonId] = 8

      given Random = new Random(0)

      (0 until 1000).foreach { _ =>
        val x: PersonId = c.random
        x.value should be >= 0
        x.value should be < c.value
      }
    }

    "range" in {
      val c: Count[PersonId] = 8
      val r: Seq[PersonId] = c.range

      r should be(0 until 8)
    }
  }

  "foreach" - {
    "works in the nominal case" in {
      var list: List[PersonId] = Nil
      val c: Count[PersonId] = 8
      c.foreach { (i: PersonId) =>
        list = i :: list
      }
      list should be((0 until 8).reverse.toList)
    }

    "works on an empty count" in {
      var list: List[PersonId] = Nil
      val c: Count[PersonId] = 0
      c.foreach { (i: PersonId) =>
        list = i :: list
      }
      list should be(Nil)
    }
  }

  "foreachTo" - {
    "works when finishing the count first" in {
      var list: List[PersonId] = Nil
      val c: Count[PersonId] = 8
      c.foreachTo(12) { (i: PersonId) =>
        list = i :: list
      }
      list should be((0 until 8).reverse.toList)
    }

    "works when reaching the limit first" in {
      var list: List[PersonId] = Nil
      val c: Count[PersonId] = 8
      c.foreachTo(4) { (i: PersonId) =>
        list = i :: list
      }
      list should be((0 to 4).reverse.toList)
    }

    "works on an empty count" in {
      var list: List[PersonId] = Nil
      val c: Count[PersonId] = 0
      c.foreachTo(4) { (i: PersonId) =>
        list = i :: list
      }
      list should be(Nil)
    }

    "works on a negative limit" in {
      var list: List[PersonId] = Nil
      val c: Count[PersonId] = 8
      c.foreachTo(-1) { (i: PersonId) =>
        list = i :: list
      }
      list should be(Nil)
    }
  }

  "foreachUntil" - {
    "works when finishing the count first" in {
      var list: List[PersonId] = Nil
      val c: Count[PersonId] = 8
      c.foreachUntil(12) { (i: PersonId) =>
        list = i :: list
      }
      list should be((0 until 8).reverse.toList)
    }

    "works when reaching the limit first" in {
      var list: List[PersonId] = Nil
      val c: Count[PersonId] = 8
      c.foreachUntil(4) { (i: PersonId) =>
        list = i :: list
      }
      list should be((0 until 4).reverse.toList)
    }

    "works on an empty count" in {
      var list: List[PersonId] = Nil
      val c: Count[PersonId] = 0
      c.foreachUntil(4) { (i: PersonId) =>
        list = i :: list
      }
      list should be(Nil)
    }

    "works on a 0 limit" in {
      var list: List[PersonId] = Nil
      val c: Count[PersonId] = 8
      c.foreachUntil(0) { (i: PersonId) =>
        list = i :: list
      }
      list should be(Nil)
    }
  }

  "foreachWhile" - {
    "works when finishing the count first" in {
      var list: List[PersonId] = Nil
      val c: Count[PersonId] = 8
      c.foreachWhile { (i: PersonId) =>
        list = i :: list
        i.value < 12
      }
      list should be((0 until 8).reverse.toList)
    }

    "works when reaching the limit first" in {
      var list: List[PersonId] = Nil
      val c: Count[PersonId] = 8
      c.foreachWhile { (i: PersonId) =>
        list = i :: list
        i.value < 4
      }
      list should be((0 to 4).reverse.toList)
    }

    "works on an empty count" in {
      var list: List[PersonId] = Nil
      val c: Count[PersonId] = 0
      c.foreachWhile { (i: PersonId) =>
        list = i :: list
        i.value < 4
      }
      list should be(Nil)
    }

    "have at least one iteration" in {
      var list: List[PersonId] = Nil
      val c: Count[PersonId] = 8
      c.foreachWhile { (i: PersonId) =>
        list = i :: list
        false
      }
      list should be(List(0))
    }
  }

  "foldLeft" - {
    "returns the correct value in the nominal case" in {
      val c: Count[PersonId] = 8
      c.foldLeft(List.empty[PersonId]) { (list, id) => id :: list } should be((0 until 8).reverse.toList)
    }
    "returns the correct value on an empty count" in {
      val c: Count[PersonId] = 0
      c.foldLeft(List.empty[PersonId]) { (list, id) => id :: list } should be(Nil)
    }
  }

  "map" - {
    "returns the correct value in the nominal case" in {
      val c: Count[PersonId] = 8
      c.map(_.toString) should be((0 until 8).map(_.toString))
    }
    "returns the correct value on an empty count" in {
      val c: Count[PersonId] = 0
      c.map(_.toString) should be(Nil)
    }
  }

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

  "find" - {
    "returns the correct value in the nominal cases" in {
      val c: Count[PersonId] = 8
      c.find(_.value == 5) should be(5)
      c.find(_.value == 11) should be(Id.None)
    }
    "returns the correct value on an empty count" in {
      val c: Count[PersonId] = 0
      c.find(_.value == 5) should be(Id.None)
    }
    "short-circuits as soon as the answer is known" in {
      val c: Count[PersonId] = 8
      var acc = 0
      c.find { id =>
        acc += 1
        id.value == 5
      } should be(5)
      acc should be(6)
    }
    "does the whole collection if required" in {
      val c: Count[PersonId] = 8
      var acc = 0
      c.find { id =>
        acc += 1
        id.value == 9
      } should be(Id.None)
      acc should be(8)
    }
  }

  "shuffled" - {
    "returns all values from the Count" in {
      given Random = new Random(0)
      val c: Count[PersonId] = 8

      c.shuffled.toSet should be ((0 until 8).toSet)
    }

    "returns values in a random order" in {
      given Random = new Random(0)
      val c: Count[PersonId] = 8
      c.shuffled shouldNot be ((0 until 8).toArray)
    }


  }
}
