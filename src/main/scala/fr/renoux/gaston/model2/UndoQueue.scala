package fr.renoux.gaston.model2

/** A mutable data structure to keep a sequence of changes on a schedule, in order to be able to undo them. If you try
  * to store more changes than the capacity, this crashes with a IndexOutOfBoundsException.
  *
  * Assumes ids fit in a single Byte (so from -128 to 127).
  */
final class UndoQueue(capacity: Int = 8) {
  private val entries: Array[Long] = new Array[Long](capacity)
  private var nextEntry: Int = 0

  private inline val bitmask = 0xff
  private inline val slide1 = 8
  private inline val slide2 = 16
  private inline val slide3 = 24
  private inline val slide4 = 32

  inline def size: Int = nextEntry

  inline def addMove(sid: SlotId, pid: PersonId, tid1: TopicId, tid2: TopicId): Unit = {
    entries(nextEntry) = encodeMove(sid, pid, tid1, tid2)
    nextEntry += 1
  }

  inline def addSwap(sid: SlotId, pid1: PersonId, tid1: TopicId, pid2: PersonId, tid2: TopicId): Unit = {
    entries(nextEntry) = encodeSwap(sid, pid1, tid1, pid2, tid2)
    nextEntry += 1
  }

  private inline def encodeMove(sid: SlotId, pid: PersonId, tid1: TopicId, tid2: TopicId): Long = {
    (sid.value.toLong << slide4) |
        (pid.value << slide2) |
        (tid1.value << slide1) |
        (tid2.value & bitmask)
    // left-most bit is by default set to 0, marking a move (and making the entry positive)
  }

  private inline def decodeMove[A](entry: Long)(inline action: (sid: SlotId, pid: PersonId, tid1: TopicId, tid2: TopicId) => A): A = {
    val sid = ((entry >> slide4) & bitmask).toInt
    val pid = ((entry >> slide2) & bitmask).toInt
    val tid1 = ((entry >> slide1) & bitmask).toInt
    val tid2 = (entry & bitmask).toInt
    action(sid, pid, tid1, tid2)
  }

  private inline def encodeSwap(sid: SlotId, pid1: PersonId, tid1: TopicId, pid2: PersonId, tid2: TopicId): Long = {
    (sid.value.toLong << slide4) |
        (pid1.value << slide3) |
        (tid1.value << slide2) |
        (pid2.value << slide1) |
        (tid2.value & bitmask) |
        Long.MinValue // switches the left-most bit to 1, marking a swap (and making the entry negative)
  }

  private inline def decodeSwap[A](entry: Long)(inline action: (sid: SlotId, pid1: PersonId, tid1: TopicId, pid2: PersonId, tid2: TopicId) => A): A = {
    val sid = ((entry >> slide4) & bitmask).toInt
    val pid1 = ((entry >> slide3) & bitmask).toInt
    val tid1 = ((entry >> slide2) & bitmask).toInt
    val pid2 = ((entry >> slide1) & bitmask).toInt
    val tid2 = (entry & bitmask).toInt
    action(sid, pid1, tid1, pid2, tid2)
  }

  inline def undo
      (inline ifMove: (sid: SlotId, pid: PersonId, tid1: TopicId, tid2: TopicId) => Unit)
          (inline ifSwap: (sid: SlotId, pid1: PersonId, tid1: TopicId, pid2: PersonId, tid2: TopicId) => Unit)
  : Unit = {
    var ix = nextEntry - 1
    while (ix >= 0) {
      val entry = entries(ix)
      if (entry >= 0) decodeMove(entry)(ifMove)
      else decodeSwap(entry)(ifSwap)
      ix -= 1
    }
  }

  inline def undoMoves(inline ifMove: (sid: SlotId, pid: PersonId, tid1: TopicId, tid2: TopicId) => Unit): Unit = {
    undo(ifMove) { (_, _, _, _, _) =>
      throw new IllegalStateException("Unexpected swap")
    }
  }

  inline def undoSwaps(inline ifSwap: (sid: SlotId, pid1: PersonId, tid1: TopicId, pid2: PersonId, tid2: TopicId) => Unit): Unit = {
    undo { (_, _, _, _) =>
      throw new IllegalStateException("Unexpected move")
    }(ifSwap)
  }

  inline def reset(): Unit = {
    nextEntry = 0
  }


}
