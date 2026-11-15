package fr.renoux.gaston.util

import fr.renoux.gaston.TestBase

import java.util.concurrent.atomic.AtomicInteger

class FastIterationsTest extends TestBase {

  /** Commodity function to count how many times a body has been called */
  def incr[A, B](i: AtomicInteger)(f: A => B)(a: A): B = {
    i.incrementAndGet()
    val result = f(a)
    result
  }

  "Simple loops" - {

    "fastLoop" - {
      "Nominal" in {
        var acc: List[String] = Nil
        fastLoop("", _ < "aaaa", _ + "a") { str =>
          acc = str :: acc
        }
        acc should be(List("aaa", "aa", "a", ""))
      }

      "Empty loop" in {
        var acc = 0
        fastLoop("z", _ < "aaaa", _ + "a") { _ =>
          acc += 1
        }
        fastLoop("a", _ < "a", _ + "a") { _ =>
          acc += 1
        }
        acc should be(0)
      }
    }

    "fastLoop (integers)" - {
      "Nominal" in {
        var acc: String = ""
        fastLoop(0, 10, _ + 2) { i =>
          acc = acc + i
        }
        acc should be("02468")
      }

      "Default advance" in {
        var acc: String = ""
        fastLoop(0, 10) { i =>
          acc = acc + i
        }
        acc should be("0123456789")
      }

      "Empty loop" in {
        var acc = 0
        fastLoop(3, 0) { _ =>
          acc += 1
        }
        fastLoop(4, 4) { _ =>
          acc += 1
        }
        acc should be(0)
      }
    }

    "fastExists (integers)" - {
      "Nominal" in {
        val bodyCount = AtomicInteger(0)

        val r = fastExists(1, 10, _ + 2)(incr(bodyCount)(_ % 7 == 0))
        r should be(true)
        bodyCount.get() should be(4) // ran on 1, 3, 5, 7
      }

      "Default advance" in {
        val bodyCount = AtomicInteger(0)

        val r = fastExists(1, 10)(incr(bodyCount)(_ % 7 == 0))
        r should be(true)
        bodyCount.get() should be(7)
      }

      "No result" in {
        val bodyCount = AtomicInteger(0)

        val r1 = fastExists(1, 10, _ + 2)(incr(bodyCount)(_ % 8 == 0))
        r1 should be(false)
        bodyCount.get() should be(5) // ran on 1, 3, 5, 7, 9
      }

      "Empty loop" in {
        val bodyCount = AtomicInteger(0)

        val r1 = fastExists(3, 3)(incr(bodyCount)(_ => true))
        r1 should be(false)
        bodyCount.get() should be(0)

        val r2 = fastExists(3, 2)(incr(bodyCount)(_ => true))
        r2 should be(false)
        bodyCount.get() should be(0)
      }
    }

    "fastFind (integers)" - {
      "Nominal" in {
        val bodyCount = AtomicInteger(0)

        val r = fastFind(1, 10, _ + 2)(incr(bodyCount)(_ % 7 == 0))
        r should be(7)
        bodyCount.get() should be(4) // ran on 1, 3, 5, 7
      }

      "Default advance" in {
        val bodyCount = AtomicInteger(0)

        val r = fastFind(1, 10)(incr(bodyCount)(_ % 7 == 0))
        r should be(7)
        bodyCount.get() should be(7)
      }

      "No result" in {
        val bodyCount = AtomicInteger(0)

        val r1 = fastFind(1, 10, _ + 2)(incr(bodyCount)(_ % 8 == 0))
        r1 should be(-1)
        bodyCount.get() should be(5) // ran on 1, 3, 5, 7, 9

        val r2 = fastFind(1, 10, _ + 2, -14)(_ % 8 == 0)
        r2 should be(-14)
      }

      "Empty loop" in {
        val bodyCount = AtomicInteger(0)

        val r1 = fastFind(3, 3)(incr(bodyCount)(_ => true))
        r1 should be(-1)
        bodyCount.get() should be(0)

        val r2 = fastFind(3, 2)(incr(bodyCount)(_ => true))
        r2 should be(-1)
        bodyCount.get() should be(0)
      }
    }
  }

  "Array extension" - {

    "fastCopy" - {
      "nominal" in {
        val initial = Array(4, 7, 11)
        val copy = initial.fastCopy()
        copy should be(initial)
        initial(0) = 42
        copy shouldNot be(initial)
        copy should be(Array(4, 7, 11))
      }
      "empty" in {
        val initial = Array.empty[Int]
        val copy = initial.fastCopy()
        copy should be(initial)
        copy.length should be(0)
      }
    }

    "fastFoldLeft" - {
      "nominal" in {
        val array = Array(4, 7, 11)
        val result = array.fastFoldLeft("#")(_ + _)
        result should be("#4711")
      }
      "empty" in {
        val array = Array.empty[Int]
        val result = array.fastFoldLeft("#")(_ + _)
        result should be("#")
      }
    }

    "fastFoldRight" - {
      "nominal" in {
        val array = Array(4, 7, 11)
        val result = array.fastFoldRight("#")(_.toString + _)
        result should be("4711#")
      }
      "empty" in {
        val array = Array.empty[Int]
        val result = array.fastFoldRight("#")(_.toString + _)
        result should be("#")
      }
    }

    "fastCount" - {
      "nominal" in {
        val array = Array(4, 7, 11)
        val result = array.fastCount(_ % 2 == 1)
        result should be(2)
      }
      "empty" in {
        val array = Array.empty[Int]
        val result = array.fastCount(_ % 2 == 1)
        result should be(0)
      }
    }

    "fastExists" - {
      "Nominal" in {
        val bodyCount = AtomicInteger(0)

        val array = Array(4, 7, 11)
        val result = array.fastExists(incr(bodyCount)(_ % 2 == 1))
        result should be(true)
        bodyCount.get() should be(2)
      }

      "No result" in {
        val bodyCount = AtomicInteger(0)

        val array = Array(4, 7, 11)
        val result = array.fastExists(incr(bodyCount)(_ % 2 == 2))
        result should be(false)
        bodyCount.get() should be(3)
      }

      "Empty" in {
        val bodyCount = AtomicInteger(0)

        val array = Array.empty[Int]
        val result = array.fastExists(incr(bodyCount)(_ % 2 == 1))
        result should be(false)
        bodyCount.get() should be(0)
      }
    }

    "fastFind" - {
      "Nominal" in {
        val bodyCount = AtomicInteger(0)

        val array = Array(4, 7, 11)
        val result = array.fastFind(-1)(incr(bodyCount)(_ % 2 == 1))
        result should be(7)
        bodyCount.get() should be(2)
      }
      "No result" in {
        val bodyCount = AtomicInteger(0)

        val array = Array(4, 7, 11)
        val result = array.fastFind(-1)(incr(bodyCount)(_ % 2 == 2))
        result should be(-1)
        bodyCount.get() should be(3)
      }
      "Empty" in {
        val bodyCount = AtomicInteger(0)

        val array = Array.empty[Int]
        val result = array.fastFind(-1)(incr(bodyCount)(_ % 2 == 1))
        result should be(-1)
        bodyCount.get() should be(0)
      }
    }

    "fastForeach" - {
      "nominal" in {
        val array = Array(4, 7, 11)
        var acc = ""
        array.fastForeach { i => acc += i }
        acc should be("4711")
      }
      "empty" in {
        val array = Array.empty[Int]
        var acc = ""
        array.fastForeach { i => acc += i }
        acc should be("")
      }
    }

    "fastForeachWithIndex" - {
      "nominal" in {
        val array = Array(4, 7, 11)
        var acc = ""
        array.fastForeachWithIndex { (i, j) => acc = acc + j + i }
        acc should be("0417211")
      }
      "empty" in {
        val array = Array.empty[Int]
        var acc = ""
        array.fastForeachWithIndex { (i, j) => acc = acc + j + i }
        acc should be("")
      }
    }

    "fastMap" - {
      "nominal" in {
        val array = Array("apple", "orange", "nut")
        val result: Array[Int] = array.fastMap(_.length)
        result should be(Array(5, 6, 3))
      }
      "empty" in {
        val array = Array.empty[String]
        val result: Array[Int] = array.fastMap(_.length)
        result should be(Array.empty[Int])
      }
    }

    "fastMapWithIndex" - {
      "nominal" in {
        val array = Array("apple", "orange", "nut")
        val result: Array[Int] = array.fastMapWithIndex { (i, str) => i + str.length }
        result should be(Array(5, 7, 5))
      }
      "empty" in {
        val array = Array.empty[String]
        val result: Array[Int] = array.fastMapWithIndex { (i, str) => i + str.length }
        result should be(Array.empty[Int])
      }
    }

    "fastFill" - {
      "nominal" in {
        val array = Array("apple", "orange", "nut")
        var incr = ""
        array.fastFill {
          incr += "a"
          incr
        }
        array should be(Array("a", "aa", "aaa"))
      }
      "empty" in {
        val array = Array.empty[String]
        var incr = ""
        array.fastFill {
          incr += "a"
          incr
        }
        array should be(Array.empty[Int])
      }
    }

    "fastTabulate" - {
      "nominal" in {
        val array = Array("apple", "orange", "nut")
        array.fastTabulate(_.toBinaryString)
        array should be(Array("0", "1", "10"))
      }
      "empty" in {
        val array = Array.empty[String]
        array.fastTabulate(_.toBinaryString)
        array should be(Array.empty[String])
      }
    }
  }

  "Iterable extension" - {

    "fastFoldLeft" - {
      "nominal" in {
        val seq = Seq(4, 7, 11)
        val result = seq.fastFoldLeft("#")(_ + _)
        result should be("#4711")
      }
      "empty" in {
        val seq = Seq.empty[Int]
        val result = seq.fastFoldLeft("#")(_ + _)
        result should be("#")
      }
    }

    "fastCount" - {
      "nominal" in {
        val seq = Seq(4, 7, 11)
        val result = seq.fastCount(_ % 2 == 1)
        result should be(2)
      }
      "empty" in {
        val seq = Seq.empty[Int]
        val result = seq.fastCount(_ % 2 == 1)
        result should be(0)
      }
    }

    "fastForeach" - {
      "nominal" in {
        val seq = Seq(4, 7, 11)
        var acc = ""
        seq.fastForeach { i => acc += i }
        acc should be("4711")
      }
      "empty" in {
        val seq = Seq.empty[Int]
        var acc = ""
        seq.fastForeach { i => acc += i }
        acc should be("")
      }
    }
  }

}
