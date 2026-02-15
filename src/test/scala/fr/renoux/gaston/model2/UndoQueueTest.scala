package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase
import scala.collection.mutable


class UndoQueueTest extends TestBase {

  "Bitwise operations" - {
    "Moves" in {
      val changes = List(
        (1, 0, 0, 0),
        (0, 1, 0, 0),
        (0, 0, 1, 0),
        (0, 0, 0, 1),
      )
      val results = mutable.ListBuffer[(SlotId, PersonId, TopicId, TopicId)]()

      val queue = UndoQueue(8)
      changes.foreach(queue.addMove)
      queue.undo { (a, b, c, d) => results.append((a, b, c, d)) } {
        throw new IllegalStateException()
      }

      results.toList should be(changes.reverse)
    }

    "Swaps" in {
      val changes = List(
        (1, 0, 0, 0, 0),
        (0, 1, 0, 0, 0),
        (0, 0, 1, 0, 0),
        (0, 0, 0, 1, 0),
        (0, 0, 0, 0, 1),
      )
      val results = mutable.ListBuffer[(SlotId, PersonId, TopicId, PersonId, TopicId)]()

      val queue = UndoQueue(8)
      changes.foreach(queue.addSwap)
      queue.undo {
        throw new IllegalStateException()
      } { (a, b, c, d, e) => results.append((a, b, c, d, e)) }

      results.toList should be(changes.reverse)
    }
  }

  "Nominal" - {

    "All moves" in {
      val changes = List(
        (1, 2, 3, 4),
        (5, 6, 7, 8),
      )
      val results = mutable.ListBuffer[(SlotId, PersonId, TopicId, TopicId)]()

      val queue = UndoQueue(8)
      changes.foreach(queue.addMove)
      queue.undo { (a, b, c, d) => results.append((a, b, c, d)) } {
        throw new IllegalStateException()
      }

      results.toList should be(changes.reverse)
    }

    "All swaps" in {
      val results = mutable.ListBuffer[(SlotId, PersonId, TopicId, PersonId, TopicId)]()
      val changes = List(
        (1, 2, 3, 4, 5),
        (6, 7, 8, 9, 10),
      )

      val queue = UndoQueue(8)
      changes.foreach(queue.addSwap)
      queue.undo {
        throw new IllegalStateException()
      } { (a, b, c, d, e) => results.append((a, b, c, d, e)) }

      results.toList should be(changes.reverse)
    }

    "Mixed moves and swaps" in {
      val changes = List(
        Left((1, 2, 3, 4)),
        Right((5, 6, 7, 8, 9)),
        Right((10, 11, 12, 13, 14)),
        Left((15, 16, 17, 18)),
      )
      val results = mutable.ListBuffer[Either[(SlotId, PersonId, TopicId, TopicId), (SlotId, PersonId, TopicId, PersonId, TopicId)]]()

      val queue = UndoQueue(8)
      changes.foreach {
        case Left((a, b, c, d)) => queue.addMove(a, b, c, d)
        case Right((a, b, c, d, e)) => queue.addSwap(a, b, c, d, e)
      }
      queue.undo { (a, b, c, d) => results.append(Left((a, b, c, d))) } { (a, b, c, d, e) => results.append(Right((a, b, c, d, e))) }

      results.toList should be(changes.reverse)
    }
  }

  "Limit" - {
    "Extreme values" in {
      val changes = List(
        Left((0, 0, 0, 0)),
        Left((127, 0, 0, 0)),
        Left((0, 127, 0, 0)),
        Left((0, 0, 127, 0)),
        Left((0, 0, 0, 127)),
        Left((127, 127, 127, 127)),
        Right((0, 0, 0, 0, 0)),
        Right((127, 0, 0, 0, 0)),
        Right((0, 127, 0, 0, 0)),
        Right((0, 0, 127, 0, 0)),
        Right((0, 0, 0, 127, 0)),
        Right((0, 0, 0, 0, 127)),
        Right((127, 127, 127, 127, 127)),
      )
      val results = mutable.ListBuffer[Either[(SlotId, PersonId, TopicId, TopicId), (SlotId, PersonId, TopicId, PersonId, TopicId)]]()

      val queue = UndoQueue(64)
      changes.foreach {
        case Left((a, b, c, d)) => queue.addMove(a, b, c, d)
        case Right((a, b, c, d, e)) => queue.addSwap(a, b, c, d, e)
      }
      queue.undo { (a, b, c, d) => results.append(Left((a, b, c, d))) } { (a, b, c, d, e) => results.append(Right((a, b, c, d, e))) }

      results.toList should be(changes.reverse)
    }

    "Capacity overload" in {
      val queue = UndoQueue(3)
      queue.addMove(1, 2, 3, 4)
      queue.addSwap(5, 6, 7, 8, 9)
      queue.addSwap(10, 11, 12, 13, 14)
      an[ArrayIndexOutOfBoundsException] shouldBe thrownBy(queue.addMove(15, 16, 17, 18))
    }
  }

}
