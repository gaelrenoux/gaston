# Unit test review

All unit tests under `src/test` were reviewed and 91 tests were added. The suite went from 200 to 291 passing tests, with no compiler warnings in the test code. Three new tests exposed real bugs in input validation; those bugs have since been fixed. Existing tests were only modified in the files listed under "Tests added".

## Bugs found in the main code

All three were in `InputTranscription`, and are now **fixed**: each one is reported as an `InputError` by `checkPersonErrors` or `checkConstraintErrors`, and the corresponding tests in `InputTranscriptionSpec` are no longer pending. Before the fix, each bad input crashed with an exception instead of returning an `InputError`:

1. **A wish for an unknown person** (`personWishes = Map("Zorro" -> …)`) throws `NoSuchElementException: key not found: Zorro`. `checkPersonErrors` never checks `personWishes`.
2. **An unknown topic in a `limitedCount` constraint** throws `NoSuchElementException`. `checkConstraintErrors` doesn't check limited-count constraints.
3. **A `limitedCount` constraint whose count is ≥ its number of topics** throws `AssertionError`. The message also prints the raw array (`[Z@b827a9e`), because `$this` is used in the assert.

Smaller issues in the main code:

- The not-simultaneous error message has a typo: `"SNot-simultaneous constraint"`.
- There's a stale `// TODO Add unit test` in `TopicsLinked.scala`; `TopicsLinkedSpec` already exists.
- There's an unused import in `RandomScheduleGenerator.scala:5`, the only compiler warning in the build.
- `Topic.unassigned(min = 1)` gives `min = 0` but `forced = false`, while `min = 0` gives `forced = true`. The comment in `inputModel.scala` suggests this is intended; `TopicSpec` records the current behavior.

## Issues in the existing tests

### Tests that never run

- `AssignmentImproverSpec` was declared `abstract`, so it never ran. It didn't even appear as "ignored" in the counts. Both of its tests were `ignore` with a TODO.
- `ProblemSpec` was an empty class that didn't extend `AnyFlatSpec`.
- `InputCleanerSpec` is entirely commented out.
- `ScheduleSpec` has about 8 commented-out tests (`merge`, `personsBySlot`, and the negative cases of `swapPersons`/`movePerson`).

### Bugs and copy-paste errors

- `ScheduleParserSpec:59` prints the literal string `"result.toOption.get.toFormattedString"`, and line 60 repeats the assertion from line 56.
- In `ScheduleParserSpec`, the `"parseTopic"` subject tests `parseRecord`, and `readAllRecords` is called with `Morning` but given afternoon records.
- `CanGroupToMapSpec:36`: the empty `Set` test is named "works on Seq.empty". All the test names there say "should works".
- Wrong behavior names: `TopicDirectPreferenceSpec` uses `"TopicDirectPreferenceSpec"`, and the person preference specs use `"PersonsPersonPreference"` / `"PersonsTopicPreference"`.
- `TopicsNotSimultaneousSpec` names its constraint `leadingFightingMachinesSimultaneous`, which is the opposite of what it is.
- `MapExtensionsSpec` uses the subject `"getMinKey"` for `minKeyOption`, and `ArrayMapSpec` has the typo "elements elements".

### Weak or misleading assertions

- `ScoreCalculatorSpec`'s weighted-score test was identical to the unweighted one, because every weight is 1, so weighting was never tested.
- The magic number `10.2265625` in `ScoreCalculatorSpec` isn't explained; it is the fold of `[5,5,5,6,6,6,6,6,6]` with rank factor 2.
- `ScheduleSpec`'s `"isSound should validate a correct schedule"` uses `Simple`, which has Bianca on Acting even though she's forbidden there. `isSound` doesn't check forbidden persons, so the test passes, but the fixture isn't correct.
- `ScheduleSpec`'s `"Empty schedule … negative-or-zero score"` only asserts `<= 0`.
- `SyncRunnerSpec` lives in the `engine` package but tests `command.SyncRunner`, and it imports `PrivateMethodTester` without using it. `ScoreCalculatorSpec` is also in `engine` while `ScoreCalculator` is in `model`.

### Fixtures and dead code

- `ComplexTestModel` isn't used anywhere. Its TODO ("add a version with unassigned topics") is stale because it already has them, and it adds `Preferences.Unassigned` twice.
- `SimpleTestModel.Topics` mixes `SimpleTestModel.Slots.X` (the companion object) with `Slots.X` (the instance).
- `CanTakeChunksSpec` has an unused `log` and a "Created by gael" header.
- `TableReaderSpec` has blocks of commented-out diffx code.
- `InputSpec` and `InputTranscriptionSpec` overlap, and a TODO says so.
- Many specs leave a `println` behind: `ScheduleSpec`, `RandomScheduleGeneratorSpec`, `SyncRunnerSpec`, `RandomAssignerSpec`, `MutableArraySetSpec`, `InputRendererSpec`, `CombinerAnonymizerSpec`, `ScheduleParserSpec`.
- `CommandLineTest` and `RendererTest` use the `*Test` suffix instead of `*Spec`.

## Tests added

| File | Covers |
|---|---|
| `model/ProblemSpec` (replaced the empty class) | slot next/previous, slot couples, forced-topic ordering, followups, mandatory/forbidden indexes, base scores, incompatible topics by topic and by slot, simultaneous index, how constraints and preferences are split up |
| `model/RecordSpec` (new) | position counts, add/remove/replace/clear, every `isSolution` and `slowErrors` failure case, ordering, formatting |
| `model/TopicSpec` (new) | `Topic.unassigned`, including how `min` and `forced` interact |
| `model/constraints/TopicsLimitedCountSpec` (new) | this constraint previously had no tests at all |
| `engine/TerminationSpec` (new) | every check, `reduceCount`, `intCount` |
| `engine/ScoreCalculatorSpec` (extended) | real weights, base scores, impersonal preferences, short-circuit to negative infinity |
| `engine/AssignmentImproverSpec` (rewritten) | never lowers the score, keeps the planning, reaches a local optimum, finds the best assignment on the simple model, `maxRounds = 0`, udocon 2017 |
| `engine/PlanningSpaceNavigatorSpec` (new) | every neighbour is a valid unfilled solution, Add/Remove moves match the change, simultaneous topics are added together, determinism, `Move.reverts` |
| `engine/GreedyEngineSpec` (new) | scores strictly increase, stops on count and on score, determinism, with and without unassigned topics |
| `input/InputTranscriptionSpec` (extended) | the three missing tests from its TODOs (simultaneous, not-simultaneous, absence-adjusted wishes), plus limited count, exclusive, linked, duration parts, defaults, forced/slots/presence, max topics per slot, absences, incompatibility, weight, min free slots, all validation errors, including the 3 bugs listed above |
