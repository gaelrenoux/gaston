# Review of the `full-rework` branch (`model2`, `engine2`, `InputTranscription2`)

Scope: the new packages `model2` and `engine2`, plus `input/InputTranscription2`, and their tests.

About 400 lines of tests were added to existing test files. **10 of the new tests fail on purpose**: each one confirms a
bug listed below. The other 483 tests in these packages pass.

## Build

The branch doesn't compile at HEAD. `build.sbt` has `-Werror` enabled (`CLAUDE.md` says it's disabled, which is out of
date), and there are 6 unused-symbol warnings:

- `input/InputTranscription2.scala:10` (unused import)
- `model2/IdMatrix.scala:28` (unused `countI` in `update`)
- `model2/idMatrix3.scala:18` (unused `countI` in `update`)
- `model2/SlotAssignment.scala:52` (unused `pid` in `isAddableToTopic`)
- `model2/ids.scala:3` (unused import)
- `util/IntModulo.scala:33` (unused implicit in `!=`)

To run the tests without changing main code:

```
sbt 'set scalacOptions -= "-Werror"' "testOnly fr.renoux.gaston.model2.* fr.renoux.gaston.engine2.* fr.renoux.gaston.input.InputTranscription2Test"
```

## Bugs confirmed by a failing test

### 1. Swapping across linked topics can corrupt the schedule

`engine2/AssignmentImprover.scala:212-227`

This happens when the current topic's linked topics and the target topic's linked topics share a slot. That is the
common case of two 2-slot topics on the same pair of slots. The first loop (over `currentLinkedTids`) has already moved
`pid` onto the target's linked topic. The second loop (over `targetLinkedTids`) then reads `personsToTopic(pid)`, gets
the target linked topic itself, and calls `swap(pid, B2, otherPid, B2)`. After that, `personsToTopic` and
`topicsToPersons` no longer agree on that slot.

The `canSwapOn...LinkedSlots` checks have the same problem: they are evaluated on the state before any change.

Test: `AssignmentImproverTest` > "Swapping persons on linked topics" > "when two linked groups share the same slots" >
"keeps the schedule consistent".

### 2. Swaps on linked slots are refused whenever a topic is full or at its minimum

`engine2/AssignmentImprover.scala:190-201`

On linked slots, the swap check uses `isAddableToTopic` and `isDroppableFromTopic`, which test the max and min as if it
were a move. A swap doesn't change the head count of either topic, so only the mandatory and forbidden checks are
relevant. As written, this blocks exactly the cases where a swap is the only option (full topics). The main slot doesn't
have this problem, because the swap there doesn't check min/max.

There is also no forbidden check on linked slots: `pid` can be swapped onto a topic it is forbidden on. Only the
`MinReward` score stops that from being accepted.

Test: `AssignmentImproverTest` > "Swapping persons on linked topics" > "when the linked topics are full" > "improves the
schedule".

### 3. `SlotAssignment.scheduledTopics` gets out of sync with `topicsToSlot`

`scheduledTopics` duplicates information from `Schedule.topicsToSlot`, but it is only set correctly when the `Schedule`
is constructed:

- `Schedule.addTopic` / `Schedule.removeTopic` (`Schedule.scala:27-39`) don't update it.
- `ScheduleMaker.mkSchedule` sets it inside the per-person loop (`ScheduleMaker.scala:98`). Topics without persons are
  only included if they happen to be iterated before a topic with persons, which depends on the iteration order of a
  `mutable.Set`. A slot with only empty topics never gets it set.
- `ScheduleMaker.fromOldSchedule` never sets it, so it stays empty. In `NewModelSpecialCasesSpec`,
  `pickGoodOpenTopicFor` therefore always returns the unassigned topic.

Suggestion: derive it from `topicsToSlot`, or have a single method that mutates both.

Tests: `ScheduleTest` > "Basics" > "scheduledTopics on each assignment matches slotsToTopics, including empty topics",
"addTopic updates the assignment's scheduledTopics"; `ScheduleMakerTest` > "fromOldSchedule sets the scheduled topics
on each assignment".

### 4. `Schedule` overrides `equals` but not `hashCode`

`model2/Schedule.scala:247`

Test: `ScheduleTest` > "Basics" > "hashCode is consistent with equals".

### 5. Persons with no positive wish get infinite or NaN preferences

`input/InputTranscription2.scala:189`

The score factor is `PersonTotalScore / totalWishScore`. When a person has no positive wish, that is `1000 / 0 =
Infinity`. A negative wish then becomes `-Infinity`, and a zero wish becomes `NaN`. The old `InputTranscription` guards
against this with `if (totalWishScore == 0) 1.0`.

Test: `InputTranscription2Test` > "Persons without any positive wish have finite preferences".

### 6. More than 64 persons or topics is not reported as an error

`SmallIdSet` can only hold ids 0 to 63, and it silently wraps around (`1L << 64 == 1L`). `InputTranscription2` doesn't
check the counts, so a too-large input produces a garbage `SmallProblem` without any error. Remember that the topic
count includes the unassigned topics (one per slot) and each part of each occurrence. `checkErrors` needs a check for
this.

Tests: `InputTranscription2Test` > "Too many entities for a SmallProblem" > "persons", "topics (including unassigned
topics)".

### 7. Simultaneous constraints on multi-occurrence topics make every occurrence simultaneous with every other

`input/InputTranscription2.scala:158-163`

`topicsIdsByBaseName` returns all occurrences, so `Alpha #1` ends up simultaneous with `Alpha #2`, `Beta #1` and
`Beta #2`. The old engine pairs occurrences by index (`Alpha #1` with `Beta #1`, `Alpha #2` with `Beta #2`), and the
existing error "different occurrence counts" suggests that pairing is the intent. **To confirm**: the test assumes the
pairing-by-index behavior.

Test: `InputTranscription2Test` > "Simultaneous constraint on multi-occurrence topics pairs occurrences by index".

## Issues without a test

- **`assertPendingChanges` has the wrong condition** (`Schedule.scala:76`). It checks
  `personsWithPendingScoreChanges.nonEmpty || personsUnmovedWithPendingScoreChanges.isEmpty`. It was almost certainly
  meant to be `nonEmpty || nonEmpty`.
- **Crash risks on linked slots** (`AssignmentImprover.scala`):
  - If a person is absent on the slot of a linked topic, `personsToTopic(pid)` is `TopicId.None`. Then
    `topicsToMandatories(Int.MinValue)` throws an index-out-of-bounds exception (lines 136, 193, 199).
  - If a linked topic is not scheduled, `schedule.topicsToSlot(linkedTid)` is `SlotId.None` and
    `slotsToAssignment(SlotId.None)` crashes the same way. This can't happen yet, but will once the planning is changed
    by the engine.
- **`pickGoodOpenTopicFor`** (`SlotAssignment.scala:38`) falls back to the unassigned topic without checking that it is
  scheduled on this slot or allowed by the settings. It also never picks a linked topic.
- **`SlotAssignment.saveScores()`** (`SlotAssignment.scala:141`) saves only its own scores and the parent's, but
  `Schedule.restoreSavedScores()` restores every assignment. It's unused today, but it's an easy mistake to make later.
- **`registerPendingChanges`** (`Schedule.scala:145`) can leave a person in both the moved and unmoved sets (it only
  removes the newly moved persons from the unmoved set). The result is still correct, just redundant work.
- **`UndoQueue` packing is wrong.** Its doc says ids from -128 to 127 are fine, but the shifts are done on `Int` before
  being combined into a `Long`, so negative values (and values ≥ 128 in the higher fields) sign-extend into the other
  fields and the swap marker. It's unused today. `SmallUndoQueue`'s doc says "between 0 and 64", which should be 0 to 63.
- **Performance:**
  - `Schedule.slotsToTopics` is a `def` that rebuilds the result (a transpose, with an allocation) on every call, and
    the improver calls it on every slot pass.
  - `assert` runs inside `getTotalScore`, which is on the hot path of the improver.
  - `defaultMaxSuccessiveRoundsCountPerSlot` is computed from the default round count, even when `improve` is called
    with a different `maxRounds`.
- **Conventions:**
  - Classes that aren't `final`: `ScheduleMaker.ScheduleDef`, `ScheduleMaker.SlotDef`, `ScheduleMaker.TopicDef`,
    `SchedulePrinter`.
  - Mutability not explained: `SlotAssignment` isn't documented as mutable, and `Schedule` says "Mutable." without
    saying why.
  - Boolean naming: `goodChangeForPerson` and `goodChangeForPersonTopic` return a `Boolean` without an `is`/`has`
    prefix.
  - The "inline this method" TODO on `Count.find` is stale; it's already inline.
  - `SlotAssignment.scoreWishesFor` uses `tid.value < problem.unassignedTopicsCount.value` instead of
    `problem.isTopicUnassigned(tid)`.
  - `checkTopicErrors` uses the old model's `Topic.SyntheticPrefix` while the class defines its own
    `TopicSyntheticPrefix`.

## Tests

### Problems with the existing tests

- The `IdTest` ordering tests could never fail: ScalaTest's `sorted` matcher uses the same implicit `Ordering` as the
  one being tested. Fixed by comparing with the sort of the raw `Int` values.
- In `AssignmentImproverTest`, the second "Multiple runs should produce the same result" test shares one `Random`
  across all 10,000 runs. It checks that every run converges to the same score, not that runs are reproducible.
- Several tests print to stdout (`ScheduleTest.Model`, `AssignmentImproverTest`).

### Tests added

- `ScheduleTest`:
  - `hashCode` agrees with `equals`.
  - `scheduledTopics` stays in sync after building with `mkSchedule`, and after `addTopic` / `removeTopic`.
  - `move`, `swap`, `undoMove` and `undoSwap` each update all the indexes (this replaces two TODOs in the file).
  - 1,000 random moves, swaps and undos, checking the incremental score against `recalculateAll()` after each one.
    This passes, so the incremental scoring looks right.
- `AssignmentImproverTest`:
  - A consistency check after `improve` on the existing scenarios (`slowCheckup`, `ScheduleInspector.checkAll`, and a
    full recalculation of the score).
  - Two small linked-topic scenarios, built from inline HOCON, for bugs 1 and 2.
- `ScheduleMakerTest`: `fromOldSchedule` sets `scheduledTopics`.
- `InputTranscription2Test`: more than 64 persons or topics, persons with no positive wish, and simultaneous
  constraints on multi-occurrence topics.
- `SmallIdSetTest`: `dropThenForeach` and `toArray`.

The failing tests make `sbt test` red. To keep it green until the fixes land, they can be wrapped in
`pendingUntilFixed`.
