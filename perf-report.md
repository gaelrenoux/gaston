# Gaston performance report

Date: 2026-11-10. Based on commit `6177e374` (branch `improvements`).

## Summary

Most of Gaston's CPU time goes to two algorithmic problems, not to collection overhead:

1. **Exponential backtracking in `RandomAssigner`.** When a neighbour planning cannot be filled, the assigner tries
   every combination before giving up. It uses 67–92% of CPU on the recent conventions, and a single neighbour can
   take more than 90 seconds, which also breaks `--duration`.
2. **Delta scoring is disabled on every recent input.** `Schedule.deltaScoreIfSwapPerson` gives up as soon as a global
   preference depends on persons, and every config since r3-2023 has such preferences. So every candidate person-swap
   rescores the whole schedule from scratch (96% of the remaining time on r32026).

Behind those, there is significant constant-factor overhead: Scala 3 thread-safe lazy vals that box their values,
immutable hash maps and sets keyed on case classes, and boxed `Score`s. An array-based model for the hot path would
remove most of it. Once deltas are cheap, a better search algorithm becomes possible.

## Problem sizes (from `src/test-slow/resources`)

| Input | Slots | Topics (before expansion) | Persons | Wishes | Unassigned allowed |
|---|---|---|---|---|---|
| r32026/full | 6 | 28 (3 with occurrences, 2 multi-slot) | 29 | ~425 | yes |
| r32025/full | 6 | 48 (10 with occurrences, 4 multi-slot) | 32 | ~536 | yes |
| r32024/full | 6 | 25 | 22 | ~206 | no |
| r32023/tts2 | 6 | 34 (5 with occurrences) | 27 | ~401 | yes |
| udocon2019 | 5 | 30 | 32 | ~313 | no |

Occurrences and multi-slot topics are expanded into several topics. There is also one synthetic unassigned topic per
slot. A slot holds roughly 5–10 topics with 3–6 persons each. Every input fits comfortably in a few KB of primitive
arrays, and all have 64 persons or fewer (one `Long` bitset per record would be enough).

## Methodology

```
sbt assembly
java -XX:StartFlightRecording=filename=r26.jfr,settings=profile \
  -jar target/scala-3.9.0/gaston-assembly-1.2.0.jar \
  --from src/test-slow/resources/r32026/full.conf -p 1 --seed 42 -d "90 seconds"
jfr print --events jdk.ExecutionSample --stack-depth 64 r26.jfr > r26.samples.txt
```

The runs are single-threaded with a fixed seed. Samples were aggregated by top frame (self time) and by any Gaston
frame on the stack (inclusive time).

| Input | Duration | Samples | Result |
|---|---|---|---|
| r32026/full | 90s | 13,085 | 92% inclusive in `RandomAssigner.backtrackFillPersons`. **Did not stop at timeout** (`TimeoutException` from `ParallelRunner`'s `Await` after 180s). |
| r32025/full | 60s | 7,458 | 67% in `backtrackFillPersons`, 32% in full `Schedule.score`. **Did not stop at timeout.** |
| udocon2019 | 60s | 3,812 | 90% in `Schedule.deltaScoreIfSwapPerson` (no unassigned topics, so the delta path is active). Terminated correctly. |

The production traces in `traces/r32026` show the same behaviour. Status lines are only printed between engine steps,
and there are gaps of up to 20 minutes between them.

## Findings

### 1. Exponential backtracking in `RandomAssigner` (critical)

`engine/assignment/RandomAssigner.scala`, `backtrackFillPersons` and `assignRemainingPersons`.

- **What happens:** the slot fill tries a person, recurses, and on failure skips that person and recurses again. If
  the slot is feasible, it usually succeeds fast. If it is not, it explores the whole tree before returning `None`.
  That is about C(n, k) per record multiplied across records in phase 1, and up to #records^#persons in phase 2.
- **Why infeasible slots reach it:** the navigator's checks (`isAddPossible`, `isSwapPossible`, `isExtSwapPossible`)
  only compare sums of `min`/`max` against the number of persons present. They ignore forbidden persons and persons
  who are mandatory on another topic of the same slot. A topic with `min = 5` where many present persons are forbidden
  passes the check, and then cannot be filled.
- **Evidence:**
  - 92% (r32026) and 67% (r32025) of CPU time.
  - The worst self frames are `Record.countPersons$lzyINIT1` (29%), `Statics.anyHash` (15%) and
    `BoxesRunTime.equals2` (11%). These come from creating a new `Record` and `SlotSchedule` at every step of the
    recursion.
  - `SpecialCasesSpec."Bad situation"` (ignored, with a FIXME) is a known reproduction on r32019.
- **Side effect:** `Termination.timeout` is only checked between neighbours, so one pathological neighbour blocks the
  thread far past the deadline.
- **Fix:** this is a bipartite b-matching problem (persons to records, with forbidden edges and `[min, max]`
  capacities), solvable exactly in polynomial time with augmenting paths. See the separate fix proposal. As a stopgap,
  a step budget on the backtracker that returns `None` when exhausted would work.
- **Expected gain:** by Amdahl's law, up to ~12× on r32026 and ~3× on r32025. It also restores `--duration`.
- **Measured with a prototype matcher** (r32026, seed 42, 1 thread, 90s): before the fix, 128 schedules reported at
  29s, then stuck, with a best score of −219.6 and a `TimeoutException`. After the fix, 1,337 schedules at 84s, a best
  score of 285.3, termination on time, and slot filling under 1% of CPU. The profile is then dominated by finding 2.
  `SpecialCasesSpec."Bad situation"` returns `None` in under a second: that slot is in fact infeasible.

### 2. Delta scoring disabled on all recent inputs (critical)

`model/Schedule.scala`, `deltaScoreIfSwapPerson`.

- **What happens:** the method returns `None` when `problem.hasGlobalPreferencesWherePersonsMatter`. The
  `AssignmentImprover` then builds the swapped `Schedule` and computes `score` from scratch for every candidate.
- **Why it's always the case:** recent inputs generate global person-dependent preferences:
  - `TopicsExclusive`, from occurrences and the unassigned settings
    (`person-multiple-anti-preference`, `person-multiple-same-cycle-anti-preference`)
  - `TopicsLinked`, from multi-slot topics
  - `PersonFreeSlotPreference`, from `min-free-slots`
- **Evidence (r32026, excluding the backtracking stacks):**
  - 96% in `Schedule.score`
  - 57% in impersonal global preferences: `TopicsExclusive.scoreSchedule` 39%, `TopicsLinked.scoreSchedule` 14%
  - Both re-walk `Schedule.personsByTopic`, a `Map` rebuilt for every schedule.
- **Fix:** each of these preferences can be split into per-person terms:
  - `TopicsExclusive`/`TopicsExclusiveFor`: for each person, how many topics of the group they attend. The penalty is
    `reward × max(0, count − 1)`.
  - `TopicsLinked`: for each person, how many topics of the group they attend. There is an anomaly if the count is
    neither 0 nor the group size. This needs aligning with the current formula, which compares against the union of
    groups.
  - `PersonFreeSlotPreference`: for each person, the number of slot cycles where they are unassigned.

  A person swap or move touches 2 persons and 2 topics, so all of these become O(1) delta updates.
- **Semantic issue to decide on:** the current `TopicsExclusive.scoreSchedule` folds over the groups and counts
  "groups containing someone already seen". The result depends on the iteration order of a `HashMap`. For example,
  groups `{p1, p2}`, `{p1}`, `{p2, p3}` give 2 × reward in that order, but 1 × reward in the order
  `{p1}`, `{p2, p3}`, `{p1, p2}`. Defining the penalty per person makes it deterministic and incremental, but changes
  the scores.

### 3. Scala 3 lazy vals box and synchronize (quick win)

Every `lazy val` in the model compiles to a `private volatile Object` field, initialized through a `VarHandle` CAS.
`Int`/`Boolean`/`Score` values are boxed. Confirmed with `javap` on `Record` (16 such fields). `Schedule` and
`SlotSchedule` have many more, and they are created by the million.

- **Evidence:** the 29% self time in `Record.countPersons$lzyINIT1` and 4.6% in `Integer.valueOf` (r32026).
- **Fix:** schedules are thread-confined, so annotate the model's lazy vals with `@scala.annotation.threadUnsafe`
  (plain bitmap-flag scheme, no boxing of the field, no CAS). For cheap fields (`countPersons`, `canAddPersons`,
  `requiresMorePersons`, `countOpenPositions`, ...), use plain `val`s.

### 4. Collection and boxing overhead in the hot path (major, needs a rewrite)

The remaining self time is spread across generic collection code:

- `Statics.anyHash`, `BoxesRunTime.equals2`, `Person.equals`
- CHAMP iterators and `HashMapBuilder.update`
- `MapMonoid.combineAll`
- `Double.compare` via `Ordering[Score]` on boxed values

Specific spots:
- `Record.unweightedScoresByPerson`: for each person, scans all their personal preferences (10–25) and calls
  `scoreRecord` on each, to find the one or two that match the record's topic.
- `SlotSchedule.deltaScoreIfSwapPersons`: builds a `Map[Person, Score]`. The in-code TODO estimates `toMap` at 12%.
- `ScoreCalculator.personalScoreFrom`: does `toSeq.map(...).sorted.foldRight` on boxed scores (TODO estimates 17%).
- `Schedule` creates a `ScoreCalculator` eagerly, including for short-lived intermediate schedules
  (`clearSlots(...)` followed by `addTopics(...)`).
- `Schedule.wrapped: Map[Slot, SlotSchedule]` and `SlotSchedule.wrapped: Map[Topic, Record]`. The ids are dense, so
  arrays would do.

**Proposed representation:**
- **Precomputed in `Problem`:**
  - `topicScore: Array[Double]` indexed by `p * T + t`. It merges all `PersonTopicPreference`s, including the
    unassigned anti-preferences.
  - `pairScore: Array[Double]` indexed by `p * P + q`. It merges `PersonPersonPreference` and
    `PersonGroupAntiPreference`, since `reward × count` is a sum of pairwise terms.
  - A person's score on a record is then `topicScore(p, t) + Σ pairScore(p, q)` over the other members.
- **Mutable working state for the `AssignmentImprover`:**
  - `topicOf: Array[Int]` indexed by `slot * P + p`, and record members as `Long` bitsets.
  - Per-person scores in an `Array[Double]`, plus the counters from finding 2.
  - Moves are apply → read delta → undo if not better.
  - An immutable `Schedule` is only built when the engine emits a result.
- **Ranking:** to compute the global score, sort a `double[P]` copy (P ≤ ~35, so this is cheap).

This should take a candidate evaluation from microseconds to around 100 ns. That is an estimate to be confirmed by
benchmark.

### 5. Search algorithm (larger rewrite)

- **`GreedyEngine.step` (cost per neighbour):**
  - Each planning neighbour gets a full random fill plus a full `AssignmentImprover.improve`.
  - The improver's queue includes every slot, so the 4+ untouched slots are re-scanned in full only to confirm that
    they are still optimal.
  - At a local optimum, the whole neighbourhood is evaluated this way before the step gives up. That is several
    hundred plannings: about 6×10 additions, 6×5×10 external swaps, 15×5×5 swaps and 30 removals on r32026.
  - **Fix:** start the improver with the changed slots, and only enqueue other slots when the global-preference
    counters changed.
- **`GreedyEngine.step` (noisy evaluation):** a neighbour is judged on a single random fill.
- **Restarts:** when a chain reaches a local optimum, the runner starts a new random chain, which throws away
  everything learned so far. Iterated local search (perturb the best schedule found, then descend again) is usually
  much better.
- **Simulated annealing or late-acceptance hill climbing:** with cheap incremental deltas, these can run over a mixed
  move set (topic add/remove/swap, person move/swap). This is the standard approach for this kind of timetabling, and
  "Simulated cooling" is already in `IDEAS.md`.
- **`AssignmentImprover.goodMoveOnSlot`:** it reshuffles and restarts the full enumeration after every improvement.
  That's fine with O(1) deltas, but quadratic waste without them.
- **`GreedyEngine` `distinctBy(_._1.planning)`:** it hashes and keeps every neighbour's planning map, even though moves
  are distinct by construction. The existing TODO already questions it. Remove it.

### 6. Minor items

- **`Context.chrono` with `Chrono.NoOp`:** the `inline override` cannot be inlined through the virtual call on
  `c.tools.chrono`, so each call still allocates a by-name closure. `Chrono$NoOp$.apply` shows up in the profiles.
  Use a static flag, or remove `chrono` from inner loops.
- **Case-class equality:** `Person`, `Topic` and `Slot` override `hashCode` to `id` but keep structural `equals`.
  Override `equals` to compare ids too.
- **Shuffling:** `rand.shuffle` on `Set`/`Iterable` allocates on every call (navigator, improver, assigner).
  Fisher–Yates on reused `Array[Int]`s avoids that.
- **`randomExtension.pick`:** converts the `Set` to an `IndexedSeq` on every call. Only used at startup, so this one
  is cosmetic.
- **`RandomAssigner` list concatenations:** `personsSkipped ++ personsLeft` and `recordsSkipped ++ otherRecords` are
  O(n) per recursion. Replacing the backtracker makes this moot.
- **GC:** with 21 parallel threads at the current allocation rate, `-XX:+UseParallelGC` may be worth a quick
  comparison against the default G1. Low priority, and mostly irrelevant after the array rewrite.

### 7. Measurement and correctness issues that affect benchmarking

- **`PerformanceAnalysis` uses udocon2019,** which avoids both critical problems. Benchmark on r32025 and r32026.
- **The "schedules tried" count is unreliable:** neighbours tried in a step that finds no improvement are not counted
  (`FIXME` in `GreedyEngine.step`). Measure improver candidate evaluations per second instead.
- **The timeout is only checked between neighbours.** Pass a deadline down to the assigner and the improver.
- **The fat JAR has no logging:** `assemblyMergeStrategy` discards all of `META-INF`, including SLF4J's
  `META-INF/services` provider file. The JAR prints "No SLF4J providers were found" and `--debug` does nothing. Use
  `MergeStrategy.concat` (or `filterDistinctLines`) for `META-INF/services/*`.

## Suggested order

| # | Change | Size | Expected impact |
|---|---|---|---|
| 1 | Exact matching-based slot fill (or a step budget as a stopgap) | Small | Up to ~3–12× on recent inputs. Fixes timeout overruns. |
| 2 | `@threadUnsafe` / plain `val` in the model | Trivial | Moderate constant factor |
| 3 | Per-person incremental global preferences, re-enabling delta scoring | Medium | Large: removes most full rescoring |
| 4 | Array-based model and mutable working state for the improver | Large | Large constant factor (estimated 10× or more on candidate evaluation) |
| 5 | Simulated annealing / iterated local search on top of the fast evaluator | Large | Better scores per unit of time. Needs benchmarking on score, not just throughput. |
| — | Benchmark on r32025/r32026, fix the attempt count, pass a deadline down | Small | Needed to measure everything above |
