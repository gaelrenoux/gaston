# Gaston

Gaston is a tool to build schedules for conventions and such. It was built for RPG conventions.

It works by generating schedules and adjusting them, trying to get the best possible global score. The global score is a
function giving more importance to people with a low score (ensuring the end schedule is fair, not sacrificing
completely a person's satisfaction so that other can have a great schedule).



## Running Gaston

Clone the repository, and build with: `sbt assembly`

This produces a fat jar `target/scala-3.5.2/gaston-assembly-<version>.jar`, where `<version>` is the one defined in
`build.sbt`. In the commands below, `gaston.jar` stands for that file. Run it with:
```
java -jar gaston.jar --from problem.conf
```

Gaston runs until you stop it (Ctrl+C), regularly displaying the best schedule found so far. Main options:
- `--from <file>` (`-f`): the input file.
- `--duration <duration>` (`-d`): stop after this time, and display the best schedule found. Example: `--duration "15 minutes"`.
- `--parallelism <n>` (`-p`): how many optimization loops run in parallel. Default is the number of CPUs minus one.
- `--seed <number>`: global seed for randomization, to reproduce a run.
- `--silent` (`-s`): do not display anything until the end. Only useful with `--duration`.
- `--from-table <file>` (`-t`) and `--generate-input` (`-g`): see *Importing a table* below.
- `--debug`: log debug messages (slows Gaston down).

To get the full list of command line options, use:
```
java -jar gaston.jar --help
```



## Input file

The input file describes the problem to solve. It follows the HOCON format, and should be in UTF-8. To give you an idea the values of the scores, each person has 1000 points distributed over their wishes.

- `gaston`: Root element of the configuration.
    - `settings`: Global settings. All fields are optional and have a default value.
        - `status-display-interval`: How often you want to display new good schedules. You can write any time unit, for example: `20 seconds` or `30 minutes`, or abbreviate it as `20s` or `30m`. Default is `20 seconds`.
        - `incompatibility-anti-preference`: Negative score for a person forced to share a topic with someone they would rather not (see `incompatible` on persons). Default is `-1000`.
        - `default-max-topics-per-slot`: Accepted but currently ignored. Use `max-topics` on each slot instead.
        - `default-min-persons-per-topic`: Minimum number of persons per topic, can be overridden on specific topics. Default is `1`.
        - `default-max-persons-per-topic`: Maximum number of persons per topic, can be overridden on specific topics. Default is `10`.
        - `unassigned`: A bunch of settings about how to handle unassigned persons (to be unassigned is to not be scheduled on any topic, on some slot). Default is to not allow it at all.
            - `allowed`: If true, Gaston will allow for people staying unassigned. Default is `false`.
            - `person-anti-preference`: Negative score attached to a person being unassigned. It's counted once for every slot when a person is unassigned. Default value is `-1000`.
            - `person-multiple-anti-preference`: Additional negative score attached if unassigned on multiple slots (in addition to the basic anti-preference). Useful if you want to make one unassignment acceptable, but not two. Default value is `0`.
            - `person-multiple-same-cycle-anti-preference`: Yet another additional negative score attached if unassigned on multiple slots in the same cycle (in addition to the previous two anti-preferences). Default value is `0`.
            - `max-persons`: On each slot, maximum number of persons who can be unassigned. Default is unlimited.
            - `min-persons`: On each slot, minimum number of persons who can be unassigned (the idea being that enough unassigned persons can do something together). Default is `0`, i.e. no minimum.
            - `person-anti-preference-scaling`: If present, this setting makes the unassignment anti-preference scale with the number of forbidden entries of a person. The more topics are forbidden for them, the less impactful an unassignment will be. Default is none.
                - `forbidden-ratio-for-maximum`: For a person, if their ratio of forbidden topics to total topics reach this value, the unassignment anti-preference will be set to its maximum value below. Default is `75%`.
                - `maximum-anti-preference`: Unassignment anti-preference for a person reaching the ratio above (this is the maximum value, i.e. the closest to zero). Default is `-1`.
        - `absence-adjustment-factor`: If a person is absent, with a factor of 1, their score will be adjusted so that it will be the same as someone having the same % of their time on the same wish level. For example: if you're only present on 3 slots and have 3 wishes at 5, you'll have the same score than someone present on 5 slots and 3 wishes at 5. This factor lets you reduce that a bit. Putting a lower factor means that absence counts for less than 1, leading to slightly better schedules for people who are missing some slots. The reasoning: if they're not here as much, they should at least enjoy the slots they are here. Slots requested through `min-free-slots` count as absences here. Default is `0.8`.
    - `table-settings`: Settings for importing a table. Optional (leave that field out if you are not importing a table). See the *Importing a table* section below for details. 
    - `slots`: The slots on which to schedule, as an array of arrays of slots. The inner arrays contain the slots following each other directly (such as one in the morning and the other in the afternoon of the same day), so that multi-slot topics may be handled. The following lines describe the structure of one slot.
        - `name`: Name of the slot. Must be unique among slots.
        - `max-topics`: Maximum number of topics to schedule on this slot (e.g. because space is limited at that time). Default is no limit.
    - `topics`: The topics to schedule. The following lines describe the structure of one topic.
        - `name`: Name of the topic. Must be unique among topics. It cannot start with `@`, nor contain `#` or `~` (those are used for generated names).
        - `min`: Minimum number of persons on a topic. Default is empty (will use the settings' value).
        - `max`: Maximum number of persons on a topic. Default is empty (will use the settings' value).
        - `occurrences`: How many sessions of the topic are possible. A person can only be on one of them. Default is `1`.
        - `duration`: How many consecutive slots (in the same inner array of `slots`) the topic occupies. The same persons are on all of them. Only `1` and `2` are supported. Default is `1`.
        - `slots`: An array of slot names on which the topic can be scheduled. Default is missing (can be scheduled on any slot).
        - `presence`: A score added for just having this topic present. Can be used to favor a topic, or to set a topic as to avoid if possible (with a negative score). Default is `0`.
        - `forced`: If `true`, this topic *must* be scheduled. Default is `false`.
    - `persons`: The persons to schedule. The following lines describe the structure of one person.
        - `name`: Name of the person. Must be unique among persons.
        - `weight`: A person with a higher weight counts more when calculating the global score. Specifically, that person's individual score will be divided by their weight, bringing them down and making the schedule optimizing more for them. Default is `1`.
        - `base-score`: You can add a starting value for this person's score. This is useful only in very specific situations. Default is `0`.
        - `absences`: An array of slot names on which this person is missing, and therefore cannot be scheduled. Default is empty.
        - `mandatory`: An array of topic names for which this person is mandatory: it must be present on those topics. Default is empty.
        - `forbidden`: An array of topic names for which this person is forbidden: they cannot be scheduled on those topics. Default is empty.
        - `incompatible`: An array of person names this person wishes not to share a topic with (see `incompatibility-anti-preference` in the settings). It only goes one way: the other persons are not affected. Default is empty.
        - `wishes`: A mapping of topic names to scores. The higher the score, the more that person wishes to be on that topic. The positive scores will be rebalanced so that everyone has a 1000 points total (including person wishes, next line).
        - `person-wishes`: A mapping of person names to scores. The higher the score, the more that person wishes to be on a topic with the target person. The positive scores will be rebalanced so that everyone has a 1000 points total (including topic wishes, previous line).
        - `min-free-slots`: How many free slots (slots where the person's not assigned to anything) they want. Requires unassigned persons to be allowed in the settings. Note that free slots must be on different inner arrays of `slots` (different days). Default is none.
    - `constraints`: Global constraints on the schedule. All are arrays, and default to empty.
        - `simultaneous`: An array of simultaneity constraints.
            - `topics`: An array of topics. Those topics must all be scheduled on the same slot, or not at all. They must have the same number of occurrences.
        - `not-simultaneous`: An array of non-simultaneity constraints.
            - `topics`: An array of topics. No two of those topics can be scheduled on the same slot.
        - `exclusive`: An array of exclusivity constraints.
            - `topics`: An array of topics. A person can only be scheduled on one of these topics.
            - `exemptions`: An array of person names to which this constraint does not apply. Optional.
            - `inclusions`: An array of person names: if present, the constraint only applies to those persons. Optional, cannot be used together with `exemptions`.
        - `linked`: An array of linking constraints.
            - `topics`: An array of topics. A person on any of these topics must be on all of them.
        - `limited-count`: An array of count constraints.
            - `topics`: An array of topics.
            - `count`: At most this number of the topics above can be scheduled. Must be lower than the number of topics.

You can display an input file filled with all default values with:
```
java -jar gaston.jar --from problem.conf --generate-input
```


### Example

```hocon
gaston = {

  slots = [
    [
      {
        "name": "D1-afternoon",
        "max-topics": 4
      }, {
        "name": "D1-evening"
      }
    ], [
      {
        "name": "D2-afternoon",
        "max-topics": 4
      }, {
        "name": "D2-evening"
      }
    ]
  ]

  persons = [
    {
      absences = ["D1-afternoon"]
      name = "Anatole"
      weight = 1.0
      mandatory = ["Gamma"]
      forbidden = ["Alpha"]
      wishes = {
        "Beta" = 10
        "Gamma" = 10
        "Delta" = 10
      }
    }
    {
      incompatible = ["Anatole", "Colin"]
      name = "Bernadette"
      weight = 1.0
      mandatory = ["Alpha"]
      wishes = {
        "Delta" = 10
      }
    }
    {
      absences = ["D1-afternoon", "D1-evening"]
      name = "Colin"
      weight = 1.5
      mandatory = ["Beta"]
      wishes = {
        "Alpha" = 1
        "Delta" = 10
      }
    }
    {
      name = "Dominique"
      weight = 1.5
      mandatory = ["Beta"]
      wishes = {
        "Alpha" = 1
        "Delta" = 10
      }
    }
    {
      name = "Eric"
    }
    {
      name = "Françoise"
    }
    {
      name = "Garfield"
    }
    {
      name = "Harley"
    }
  ]

  topics = [
    {
      max = 5
      name = "Alpha"
    }
    {
      max = 4
      min = 4
      name = "Beta"
    }
    {
      name = "Gamma"
    }
    {
      min = 5
      name = "Delta"
    }
    {
      max = 6
      min = 3
      name = "Epsilon"
    }
    {
      max = 6
      min = 3
      name = "Zeta"
      forced = true
    }
    {
      max = 6
      min = 3
      name = "Eta"
      presence = -100
    }
  ]

  constraints = {
    simultaneous = [
      {topics = ["Eta", "Zeta"]}
    ]
    exclusive = [
      {topics = ["Eta", "Zeta"]}
      {topics = ["Beta", "Gamma"], exemptions = ["Eric"]}
    ]
  }
}
```



## Importing a table

Quite often, the wishes of the participants will be expressed in a double-entry table. The importing feature can transform this table into a HOCON file.

The table must have the topics in lines, and each person in a column (this may become selectable in the future). To import a table:
```
java -jar gaston.jar --from params.conf --from-table table.txt --generate-input
```

The configuration file will hold the information necessary to properly import the table, as well as basic information not present in the tables (settings and slots).

Note that you can use directly the table to generate schedules (by omitting the `--generate-input` option) but this is not recommended. You should generate the input, check it (and possibly modify it), then generate schedules using the input.

The table only provides topics (name, occurrences, min and max persons) and persons (mandatory and forbidden topics, wishes). Settings and slots come from the configuration file, while its topics, persons and constraints are discarded: add them back to the generated input if needed.

The parameters for the import are in the `table-settings` section of the input file. All indexes start at 0 (first line is index 0, second line is index 1, and so on).
- `separator`: Separator between columns on a single line (should be inside quotation marks). Default value is `\t` (tabulation). 
- `persons-row`: Row for the persons' names. Default value is `0` (first row).
- `wishes-start-row`: First row of wishes, typically `1` because there's the persons' line before. Default value is `1` (wishes start on the second line).
- `persons-start-col`: First column containing a person. Default value is `4`.
- `topic-col`: Column for the topics' names. Default value is `0`. 
- `topic-occurrences-col`: Column for the topics' occurrences (see field `gaston.topics.occurrences`). Default value is empty, all occurrences being let at their default value of `1`. 
- `mandatory-person-col`: Column for a mandatory person on a topic (see field `gaston.persons.mandatory`, there is often only one person per topic). It is not possible to set multiple mandatory persons on a topic in the table, this is something you will have to do by modifying the input file afterwards. Default value is `1`. 
- `min-persons-col`: Column for the min number of persons on a topic (see field `gaston.topics.min`). Default value is empty (topics won't have a specific min). 
- `max-persons-col`: Column for the max number of persons on a topic (see field `gaston.topics.max`). Default value is `3`. If a cell is not a number, that topic won't have a specific max. 
- `persons-count-add`: A number to add to the min and max number of persons read, because sometimes the mandatory person isn't counted in the table. Default value is `0`. 
- `mandatory-person-weight`: A special weight for a person if they are mandatory on at least one topic (see field `gaston.persons.weight`). Default value is `1`, i.e. same as everyone else. 
- `forbidden-person-marker`: This text in a wish cell indicates that that person is forbidden on that topic (see field `gaston.persons.forbidden`). Default value is empty (no forbidden marker).
- `preferences-score-mapping`: An object mapping for each possible text in a wish cell, the associated score. Unknown texts are ignored. Optional: if the field is missing (or set to `null`), the text in the cell will be converted to a number and used (if the text is not a number, it is ignored).

### Example input

```hocon
gaston {
  
  settings {
      default-max-topics-per-slot = 3
  }
  
  table-settings {
      separator = "\t"
      persons-row: 0
      wishes-start-row = 1
      persons-start-col = 4
      topic-col = 0
      topic-occurrences-col = null
      mandatory-person-col = 1
      min-persons-col = 2
      max-persons-col = 3
      persons-count-add = 0
      mandatory-person-weight = 1.5
      forbidden-person-marker = "0"
      preferences-score-mapping = {
        "+" = 1.0
        "++" = 2.0
        "+++" = 5.0
      }
  }
  
  slots = [
    [
      {
        "name": "D1-afternoon",
        "max-topics": 4
      }, {
        "name": "D1-evening"
      }
    ], [
      {
        "name": "D2-afternoon",
        "max-topics": 4
      }, {
        "name": "D2-evening"
      }
    ]
  ]
}
```


# Developer's corner

## Brain dump

Some vocabulary:
- Planning is when we decide which topic is on which slot. It does NOT include assigning persons to those topics or checking score. It does however require we check that mandatory persons are available.
    - Therefore, a planning is a schedule with no one assigned apart from the mandatory persons.
- Assignment is when we assign persons to an existing planning.
- Scheduling is doing both.
- An unfilled schedule is a schedule where only the planning is done, most persons are not assigned yet. Some persons may have been assigned, which is why we don't call it unassigned.

The Engine produces a lazy list of schedules, each new schedule being better than the previous one. The list ends when no
further improvement can be found, which never happens on real-world examples. It starts with a seed for all random
operations.

The Engine is CPU-bound and monothreaded. Typically, you would run multiple Engines in parallel, one per CPU, with
different seeds.

An Engine run (in `Engine.lazySeq`) starts with an initial schedule respecting all constraints, but with probably a terrible score.
- If unassigned persons are not allowed, a schedule with topics and such is generated by backtracking.
    - This uses the `RandomScheduleGenerator`, which is only used for this.
    - This schedule has been assignment-optimized (see below).
- If unassigned persons are allowed, we start with an almost empty schedule: everyone is unassigned, except on forced topics.
    - This is done by `Schedule.startingUnassignedOrForced`.

Then, the Engine generates a lazy-list by improving the state step-by-step.
There are two Engines implemented, GreedyEngine and TabuSearchEngine. Only the first one is currently being used.

In GreedyEngine, a generation step starts from the latest generated schedule, and proceeds with the following steps:
- Figure out all neighbours of the latest schedule in the planning spaces. Neighbours are plannings that we can get from that schedule by adding or dropping a topic, or swapping two topics (this is done by `PlanningSpaceNavigator`).
- Eliminate neighbours that would be just a revert of the previous change (e.g. latest schedule was obtained by adding topic A, then dropping topic A is not a neighbour we need to consider).
- For each neighbour (lazily)
  - Fills in the planning by assigning people to topics. We are respecting constraints (a person can't be on two topics in the same slot), but ignoring preferences (even strong ones).
  - Improves the assignment as much as possible by swapping or moving persons around, stop when all moves make it worse.
- As soon as we find a schedule that's better than the latest schedule, we stop iterating over neighbours and output that schedule.


