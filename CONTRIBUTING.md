
# Vocabulary

## Basic concepts
- Planning is when we decide which topic is on which slot. It does NOT include assigning persons to those topics or checking score. It does however require we check that mandatory persons are availabe.
    - Therefore, a planning is a schedule with no one assigned apart from the mandatory persons.
- Assignment is when we assign persons to an existing planning.
- Scheduling is doing both.
- An unfilled schedule is a schedule where the planning is done, but most persons are not assigned yet. Some persons may have been assigned, which is why we don't call it unassigned.

## Constraints

### Linked topics
Any person on one of these topics must be on all of these topics. Mostly used for multi-slot topics: topics are divided into parts, and any person has to be on all parts, or no part.

# Brain dump

The Engine produces a lazy list of schedules, each new schedule being better than the next one. The list ends when no
further improvement can be found, which never happens on real-world examples. It starts with a seed for all random
operations.

The Engine is CPU-bound and monothreaded. Typically, you would run multiple Engines in parallel, one per CPU, with
different seeds.

An Engine run (in `Engine.lazySeq`) starts with an initial schedule respecting all constraints, but with probably a terrible score.
- If using backtracking, a schedule with topics and such will be generated. If there are enough topics to occupy everyone and not too many constraints, that is the recommended solution.
    - This uses the ScheduleGenerator, which is only used for this.
    - This schedule has been assignment-optimized (see below).
- If not using backtracking (backtrack-initial-schedule option is set to false), we start with an empty schedule. This is recommended if you expect some persons to be unassigned on some slots.
    - This starts using `Schedule.startingUnassignedOrForced`.

Then, the Engine generates a lazy-list by improving the state step-by-step.
There are two Engines implemented, GreedyEngine and TabuSearchEngine. Only the first one is currently being used.

In GreedyEngine, a generation step starts from the latest generated schedule, and procedes with the following steps:
- Figure out all neighbours of the latest schedule in the planning spaces. Neighbours are plannings that we can get from that schedule by adding or dropping a topic, or swapping two topics.
- Eliminate neighbours that would be just a revert of the previous change (e.g. latest schedule was obtained by adding topic A, then dropping topic A is not a neighbour we need to consider).
- For each neighbour (lazily)
    - Fills in the planning by assigning people to topics. We are respecting constraints (a person can't be on two topics in the same slot), but ignoring preferences (even strong ones).
    - Improves the assignment as much as possible by swapping or moving persons around, stop when all moves make it worse.
- As soon as we find a schedule that's better than the latest schedule, we stop iterating over neighbours and output that schedule.


