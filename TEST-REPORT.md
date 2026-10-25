## Issues in the existing tests

### Tests that never run

- `AssignmentImproverSpec` was declared `abstract`, so it never ran. It didn't even appear as "ignored" in the counts. Both of its tests were `ignore` with a TODO.
- `InputCleanerSpec` is entirely commented out.

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
