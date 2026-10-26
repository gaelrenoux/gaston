# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Gaston builds schedules for conventions (originally RPG conventions): it assigns topics to slots and persons to topics, optimizing a global score that favors the least-satisfied persons (fairness over total satisfaction). The input format (HOCON) and the table-import feature are documented in `README.md`.

## Commands

Scala 3.5.2, sbt 1.10, JDK 21 (CI).

- Build fat jar: `sbt assembly` → `target/scala-3.5.2/gaston-assembly-<version>.jar` (version comes from `build.sbt`; the README's jar name may be stale).
- Run: `java -jar <jar> --from problem.conf` (other options: `--from-table <file>`, `--generate-input`, `--duration`, `--seed`, `--parallelism`, `--silent`, `--debug`; see `command/CommandLine.scala`).
- Fast tests (what CI runs): `sbt test`
- Single test class: `sbt "testOnly fr.renoux.gaston.model.ScheduleSpec"`; single test by name: `sbt "testOnly fr.renoux.gaston.model.ScheduleSpec -- -z \"some test name\""`
- Slow tests (integration tests on real past conventions, benchmarks): `sbt test-slow:test` / `sbt "test-slow:testOnly fr.renoux.gaston.itests.PastUsesSpec"`. These live in `src/test-slow/` (a custom sbt config extending `Test`), take minutes, and are currently disabled in CI.
- `-Werror` is disabled until the Scala 3 migration is finished, but the `-Wunused:*` warnings are on: keep compilation warning-free.

## Architecture

Packages under `src/main/scala/fr/renoux/gaston/`:

- `input`: `InputLoader` reads HOCON via pureconfig into `InputModel` (`inputModel.scala`, using Iron refined types). `TableReader` merges a CSV/TSV wishes table into an `InputModel`. `InputTranscription` is the key step converting `InputModel` into a `model.Problem`: it creates the `Slot`/`Topic`/`Person` objects with dense integer ids, normalizes wishes (each person has 1000 points), and translates settings into `Constraint`s and `Preference`s. Input errors are returned as `Either[InputErrors, _]`; any other failure is a bug and throws.
- `model`: `Problem` (immutable, with many precomputed lazy indexes) and `Schedule` → `SlotSchedule` → `Record` (one topic on one slot with its persons). Scoring lives in `ScoreCalculator`: personal scores are divided by weight, sorted, and folded with a `RankFactor` so the worst-off person dominates; impersonal preferences are added on top.
  - `Constraint`s are hard rules and are only *checked* for sanity (a violated constraint means a bug); most are "hard-coded" into schedule construction (`isHardCoded`).
  - `Preference`s are soft and produce a `Score`. They are classified by evaluation level (`RecordLevel` / `SlotLevel` / `GlobalLevel`) and `Personal` vs impersonal; `Problem` partitions them so scoring can be done incrementally per slot. Some hard rules are expressed as preferences with `Preference.NecessaryPreferenceScore`.
  - Unassigned persons are modeled as synthetic per-slot topics (`Topic.isUnassigned`), created in `InputTranscription` when `settings.unassigned.allowed` is true; unassignment penalties are ordinary `PersonTopicPreference`/`TopicsExclusive` preferences on those topics.
- `engine`: `Engine.lazySeq(seed, termination)` yields a lazy list of improving schedules for one random chain. The initial schedule comes from `RandomScheduleGenerator` (backtracking) or, when the problem has unassigned topics, from `Schedule.startingUnassignedOrForced`. `GreedyEngine` (the one in use; `TabuSearchEngine` exists but is unused) walks the planning space via `PlanningSpaceNavigator` (add/drop/swap topics), fills each neighbour with `RandomAssigner`, optimizes it with `AssignmentImprover`, and emits the first neighbour that improves the score.
- `command`: CLI entry point `Main`, `Output`/`Renderer` for display. `SyncRunner` runs chains in one thread, restarting a new chain with a fresh seed when one ends; `ParallelRunner` runs one `SyncRunner` per core and keeps the best result. Engines are single-threaded and immutable; parallelism is only at the runner level.
- `tools`: anonymizers and a `ScheduleParser` for reading back printed schedules.
- `util`: performance-oriented collections keyed by `Identified.id` (`ArrayMap`, `ArraySet` as opaque types over arrays, plus mutable variants), and `Context` (debug flag + `Chrono` profiling, passed as a `given`).

Vocabulary used in code and comments: *planning* = which topic is on which slot (only mandatory persons assigned); *assignment* = putting persons on an existing planning; *scheduling* = both; an *unfilled* schedule has a planning but most persons are not assigned yet.

### Conventions specific to this codebase

- `Problem`, `Context`, and often `Random` are passed as Scala 3 `given`/`using` parameters throughout the model and engine.
- Performance matters (the engine evaluates millions of schedules): hot paths use `ArrayMap`/`ArraySet`, `inline`, opaque types (`Score`, `Weight`), and imperative loops. Keep that style in hot code; ids must stay dense from 0 for the array-based collections to work.
- Tests use ScalaTest `AnyFlatSpec` with `Matchers`. Shared fixtures are `MinimalTestModel`, `SimpleTestModel`, `ComplexTestModel`; `TestUtils` provides `.force` on `Either` and the `slot"..."`, `t"..."`, `p"..."` interpolators to look up entities by name in the given `Problem`.
- Real convention inputs (`src/test-slow/resources/<event>/`) and output traces (`traces/<event>/`) are kept in the repo as regression material.
