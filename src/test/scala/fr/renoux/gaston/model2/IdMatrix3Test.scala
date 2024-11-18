package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase

class IdMatrix3Test extends TestBase {
  val depth = 2
  val height = 3
  val width = 4

  val countI: Count[SlotId] = depth
  val countJ: Count[TopicId] = height
  val countK: Count[PersonId] = width

  val testSeq: Seq[Seq[Seq[String]]] = Seq(
    Seq(
      Seq("", "yes", "no", "yes"),
      Seq("no", "no", "no", ""),
      Seq("no", "no", "yes", "no")
    ),
    Seq(
      Seq("", "hello", "goodbye", "hello"),
      Seq("goodbye", "goodbye", "goodbye", ""),
      Seq("goodbye", "goodbye", "hello", "goodbye")
    )
  )

  "Creation" - {
    "tabulate" in {
      val matrix = IdMatrix3.tabulate(countI, countJ, countK) { (i, j, k) => testSeq(i.value)(j.value)(k.value) }
      matrix.toSeq3(countI, countJ, countK) should be(testSeq)
    }

    "fill" in {
      val matrix = IdMatrix3.fill(countI, countJ, countK)("hello")
      val expected = Seq.fill(depth, height, width)("hello")
      matrix.toSeq3(countI, countJ, countK) should be(expected)
    }

    "from" in {
      val matrix = IdMatrix3.from[SlotId, TopicId, PersonId, String](testSeq)
      matrix.toSeq3(countI, countJ, countK) should be(testSeq)
    }
  }

  "apply" - {
    "read key inside the matrix" in {
      val matrix = IdMatrix3.from[SlotId, TopicId, PersonId, String](testSeq)
      matrix(0, 0, 1)(countJ, countK) should be("yes")
      matrix(1, 1, 3)(countJ, countK) should be("")
      matrix(1, 2, 3)(countJ, countK) should be("goodbye")
      for { i <- 0 until depth; j <- 0 until height; k <- 0 until width } {
        matrix(i, j, k)(countJ, countK) should be(testSeq(i)(j)(k))
      }
    }

    "read key outside the matrix" in {
      val matrix = IdMatrix3.from[SlotId, TopicId, PersonId, String](testSeq)
      an[ArrayIndexOutOfBoundsException] shouldBe thrownBy { matrix(1, 3, 3)(countJ, countK) }
      an[ArrayIndexOutOfBoundsException] shouldBe thrownBy { matrix(-1, 1, 1)(countJ, countK) }
      // an[ArrayIndexOutOfBoundsException] shouldBe thrownBy { map(1, -1, 1)(countJ) } // No, because it's flattened!
    }
  }

  "update" - {
    "update key inside the matrix" in {
      val matrix = IdMatrix3.from[SlotId, TopicId, PersonId, String](testSeq)
      matrix.update(0, 0, 1)("no")(countJ, countK)
      matrix.update(1, 1, 3)("hello")(countJ, countK)
      matrix.update(1, 2, 3)("")(countJ, countK)
      matrix(0, 0, 1)(countJ, countK) should be("no")
      matrix(1, 1, 3)(countJ, countK) should be("hello")
      matrix(1, 2, 3)(countJ, countK) should be("")
      for { i <- 0 until depth; j <- 0 until height; k <- 0 until width } {
        if ((i, j, k) == (0, 0, 1)) matrix(i, j, k)(countJ, countK) should be("no")
        else if ((i, j, k) == (1, 1, 3)) matrix(i, j, k)(countJ, countK) should be("hello")
        else if ((i, j, k) == (1, 2, 3)) matrix(i, j, k)(countJ, countK) should be("")
        else matrix(i, j, k)(countJ, countK) should be(testSeq(i)(j)(k))
      }
    }
  }

}
