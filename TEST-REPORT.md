## Issues in the existing tests

### Tests that never run

- `AssignmentImproverSpec` was declared `abstract`, so it never ran. It didn't even appear as "ignored" in the counts. Both of its tests were `ignore` with a TODO.
- `ProblemSpec` was an empty class that didn't extend `AnyFlatSpec`.
- `InputCleanerSpec` is entirely commented out.
- `ScheduleSpec` has about 8 commented-out tests (`merge`, `personsBySlot`, and the negative cases of `swapPersons`/`movePerson`).

### Weak or misleading assertions

- `ScheduleSpec`'s `"isSound should validate a correct schedule"` uses `Simple`, which has Bianca on Acting even though she's forbidden there. `isSound` doesn't check forbidden persons, so the test passes, but the fixture isn't correct.

### Fixtures and dead code

- `ComplexTestModel` isn't used anywhere. Its TODO ("add a version with unassigned topics") is stale because it already has them, and it adds `Preferences.Unassigned` twice.
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
