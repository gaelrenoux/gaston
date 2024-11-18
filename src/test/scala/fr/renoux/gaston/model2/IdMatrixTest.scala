package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase


class IdMatrixTest extends TestBase {
  val height = 3
  val width = 4
  val countI: Count[SlotId] = height
  val countJ: Count[TopicId] = width

  val testSeq: Seq[Seq[String]] = Seq(
    Seq("", "yes", "no", "yes"),
    Seq("no", "no", "no", ""),
    Seq("no", "no", "yes", "no")
  )

  "Creation" - {
    "tabulate" in {
      val matrix = IdMatrix.tabulate(countI, countJ) { (i, j) => testSeq(i.value)(j.value) }
      matrix.toSeq2(countI, countJ) should be(testSeq)
    }

    "fill" in {
      val matrix = IdMatrix.fill(countI, countJ)("hello")
      val expected = Seq.fill(height, width)("hello")
      matrix.toSeq2(countI, countJ) should be(expected)
    }

    "from" in {
      val matrix = IdMatrix.from[SlotId, TopicId, String](testSeq)
      matrix.toSeq2(countI, countJ) should be(testSeq)
    }
  }

  "apply" - {
    "read key inside the matrix" in {
      val matrix = IdMatrix.from[SlotId, TopicId, String](testSeq)
      matrix(0, 1)(countJ) should be("yes")
      matrix(1, 3)(countJ) should be("")
      matrix(2, 3)(countJ) should be("no")
      for { i <- 0 until height; j <- 0 until width } {
        matrix(i, j)(countJ) should be(testSeq(i)(j))
      }
    }

    "read key outside the matrix" in {
      val matrix = IdMatrix.from[SlotId, TopicId, String](testSeq)
      an[ArrayIndexOutOfBoundsException] shouldBe thrownBy { matrix(3, 3)(countJ) }
      an[ArrayIndexOutOfBoundsException] shouldBe thrownBy { matrix(-1, 1)(countJ) }
      // an[ArrayIndexOutOfBoundsException] shouldBe thrownBy { map(1, -1)(countJ) } // No, because it's flattened!
    }
  }

  "update" - {
    "update key inside the matrix" in {
      val matrix = IdMatrix.from[SlotId, TopicId, String](testSeq)
      matrix.update(0, 1)("no")(countJ)
      matrix.update(1, 3)("yes")(countJ)
      matrix.update(2, 3)("")(countJ)
      matrix(0, 1)(countJ) should be("no")
      matrix(1, 3)(countJ) should be("yes")
      matrix(2, 3)(countJ) should be("")
      for { i <- 0 until height; j <- 0 until width } {
        if ((i, j) == (0, 1)) matrix(i, j)(countJ) should be("no")
        else if ((i, j) == (1, 3)) matrix(i, j)(countJ) should be("yes")
        else if ((i, j) == (2, 3)) matrix(i, j)(countJ) should be("")
        else matrix(i, j)(countJ) should be(testSeq(i)(j))
      }
    }
  }

  "scoreSumLines" in {
    val matrix = IdMatrix.from[SlotId, TopicId, String](testSeq)
    val scoreLines = matrix.mapSumLinesToScore {
      case (i, j, "yes") => (i.value + 1) * (j.value + 1)
      case (i, j, "no")  => -i.value - 1
      case _             => 0
    }(countI, countJ)

    scoreLines.toMap should be(Map(0 -> 5, 1 -> -6, 2 -> 0))
  }

}
